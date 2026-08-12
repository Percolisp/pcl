# Large-scale duplication & inefficiency review of the v2 compiler (s386, Fable)

USER ask: "smaller parts of code seem good, but there seems to be doubled
functionality" — review the compiler for large-scale inefficiencies.

**Method.** Beyond re-verifying the two standing reviews
(`docs/var-handling-review-s379.md` — variable identity;
`docs/v2-code-review.md` — precedence copies / seam / two brains), this
review adds a measurement neither had: a **dynamic call trace of every
`Pl::*` sub over a full 138-file perl-tests corpus transpile** (wrap-all-subs
tracer, scratchpad `tracer/PclTrace.pm`; per-sub call counts in the session
scratchpad).  Numbers below are from that trace.

**Caveats (read before deleting anything).**  The trace covers FILE
transpiles only.  Three path families legitimately did not run and are
EXCLUDED from every dead-code claim here: eval-mode compilation
(`parse_code`, `_assemble_eval_mode`, `_eval_*` — exercised at RUNTIME by
string eval through the pl2cl server), bundle/executable mode
(`Environment::clone/merge`, `CLForm::to_program`), and env-gated
instrumentation (`_seam_*`, `_raw_census`, `SET_DEBUG`).  A "never called"
verdict on anything else means: not called while transpiling all 138
corpus files.

---

## 1. THE headline: v1 is still the PRIMARY expression compiler, not a residue

The user's instinct "doubled functionality" is correct, and the biggest
instance is live, not dead:

- `Parser2::_lower_expr` (the generic expression entry): **19,165 calls**.
- `Parser::_parse_expression_form` (the v1 fallback): **16,897 calls**.
- Native successes (PExpr tree + `ExprToCL2->gen_form`): ~2,268 ≈ **12%**.

**88% of the expressions that reach the generic path are still lowered by
the v1 machinery.**  The 12% native rate is unchanged from the s316t
estimate (`v2-code-review.md` Finding 4) — two years of feature work
happened *inside* the fallback, not by shrinking it.  Consequences,
all measured:

- v1's whole text-section infrastructure runs at every fallback:
  `_emit` ×171k, `_sections` ×279k, `_cur_bucket` ×234k, `indent_level`
  ×290k per corpus — plus the ~40-line save/drain/restore dance in
  `_lower_expr` and the 101-line `_fallback_stmt_capture` twin.
- **126 of Parser.pm's 139 subs are live** (9.0k of 9.4k lines) — the
  seam reaches v1's statement layer too (`parse_block_as_function`
  ×1,363 for embedded/anon bodies).  v1 cannot be deleted; it can only
  be *starved*.
- Parser2 keeps its own lexical registry INSIDE the v1 object:
  `fallback_parser->{_let_bound_vars}` is accessed from Parser2 at
  **27 sites** (plus `_catch_labels`, `_eval_span_captures`).  One
  variable-identity fact, two owners — every scoping change (all of
  #291/#296's subject matter) must be correct in both compilers at once.

**Ruling: this is what task #153 (Option B `_reduce_term` + FOLD) is for,
and this measurement is its justification and its progress metric.**  Track
the fallback rate (the seam census already exists: `PCL_V2_SEAM_CENSUS=1`);
the FOLD is done when it reaches 0 and `_parse_expression_form` dies.  The
`_let_bound_vars` relocation (one registry, owned by Parser2, read by the
fallback through ONE accessor) should be the FOLD's first chunk — it
removes the cross-compiler state entanglement before the big port.
No new task; this re-affirms #153's priority after the #296→#291→#292
chain.

## 2. Confirmed-dead mass: ~3.5k lines (~10% of compiler Perl), mechanically deletable

The literal "doubled functionality": E2 converted v1's emitter to CLForm
by adding `*_form` twins beside the old text emitters — **and the old ones
were never deleted.**  They are what you read when you open the file.

| where | what | dead lines |
|---|---|---|
| `Pl/ExprToCL.pm` | 35 never-called subs — the pre-E2 TEXT emitters, each with a live `*_form` twin: `gen_funcall` (**883 lines**), `gen_prefix_op` (214), `gen_methodcall` (118), `gen_tree_val` (80), `gen_progn` (74), `gen_glob` (73), `gen_hash_access` (55), … | **2,238** |
| `Pl/BlockAnalyzer.pm` | whole module, 0 of 11 subs called; its only callers are v1 statement paths that themselves never fire.  Plus its Pl/t test files. | **327** |
| `Pl/Parser.pm` | v1's superseded handlers: `_process_toplevel_state_declaration` (109), `_process_state_declaration` (93), `_process_use_lib` (55), `_eval_scope_parts/_free`, `_block_is_anon_sub`, … | **437** |
| `Pl/VarAnnotator.pm` | `_analyze_text` + `_scan` + `_diff_report` — the s272 text-scan prototype, mode-gated remnant of the W12 dual-run (#27 deleted the flag's OLD default but not the machinery) | **~200** |
| `Pl/PExpr.pm` + misc | debug helpers + orphans (`debug_dump_tree`, `_tok_run_desc`, dead small preds), `Environment::body` (157) + friends | **~600** |

Filed as the **dead-code deletion batch** (new task): per-sub textual
verification before each delete (a "dead" sub referenced from a LIVE line
is a stop-and-look), pure-deletion bar = **corpus-diff byte-identical**,
gate + full sweep (lib-reach rule).  Sequence it AFTER #291 lands — #291
itself deletes the three poisoned-my rename passes and their veto
predicates from the same files, and the two deletions must not collide on
the branch.

## 3. Compile-time hot spots, now measured (Target-A / #213 material)

Per full-corpus transpile:

- **`PExpr::DEBUG` — 4.3M calls.**  It is `sub DEBUG { $DEBUG_VAL }`, a
  real function call in every `if 1 & DEBUG` guard; it never inlines.
  `SET_DEBUG` has zero callers (trace + grep: dev-only).  One-line fix:
  `use constant DEBUG => $ENV{PCL_PEXPR_DEBUG} // 0` — keeps the dev
  override, deletes 4.3M calls.  Sign-off-rule eligible (simpler, faster,
  zero emission change; corpus-diff must be byte-identical).
- **Accessor churn in the inner loop**: `PExpr::token_utils` ×8.0M,
  `node_tree` ×3.1M, `OpcodeTree::nodes` ×3.1M, `ExprToCL::expr_o` ×1.0M,
  two `environment` accessors ×1.0M combined.  Classic Moo-accessor cost;
  the cure is hash access or lexical hoisting in the ~5 hottest loops —
  but measure per-change, and only in R2 (speed) unless a change is free.
- **`CLForm::_flat` ×1.24M** — direct evidence for #213's quadratic
  nesting diagnosis (94% of a nested file's CL is leading whitespace).
- `Parser2::_elem_within` ×219k, `Parser::_find_all_declarations` ×175k,
  `_interp_token_candidate` ×110k — per-token/per-node scans called
  per-statement; caching candidates for #213's session.

These numbers go to task #213 (its description now points here); none of
this is scheduled ahead of the correctness queue.

## 4. What the standing queue already owns (verified still true, do not re-file)

- **Interp scanning**: `InterpScan` exists (667 lines, correct) but has
  exactly ONE consumer wired (`ExprToCL`'s regex path, s382f).  The other
  ~25 ad-hoc sites (s379 §1.5) are still live, including the three
  near-identical `_interp_canon`/`_interp_names`/`_fix_interp_token`
  walkers (#301 just fixed the same bug in all three at once — the cost of
  the duplication made visible).  That is #237 consumers 2–3, re-sized
  after #291.
- **The rename-suffix zoo + three veto predicates + two decl counters**
  (`_shadow_rename_blocker` / `_state_container_blocker` /
  `_state_rw_blocker`; `_hard_decl_count` / `_count_name_decls`) — all
  still present; #291 deletes the three poisoned families, s379 direction
  A/C owns the rest.
- **Two expression brains** (ExprToCL2 as a 12%-hit fast path duplicating
  the BINOP/context tables) — `v2-code-review.md` R1-d, folds during #153.
- `_forward_global_decls` scanning EMITTED CL text (102 lines) — s379 §1.6,
  dies with direction A.

## 5. What this review does NOT recommend

- No restructuring now.  The #296→#291→#292 chain is mid-flight and touches
  the same files; everything above is sequenced behind it.
- No "port more expression shapes to ExprToCL2 one by one" — that grows the
  second brain instead of killing the first; the FOLD (#153) is the ruled
  direction.
- No accessor-optimization campaign — R2 material, measured per change.

## 6. Sequenced plan

1. (in flight) #296 finish → #291 → #292 — unchanged.
2. **DEBUG→constant** micro-fix — any filler slot, corpus-diff identical.
3. **Dead-code deletion batch** (§2, new task) — one session, after #291.
4. **#153 chunk 0**: move `_let_bound_vars`/`_catch_labels`/
   `_eval_span_captures` ownership into Parser2 (one accessor for the
   fallback) — the entanglement cut, before the FOLD proper.
5. #153 FOLD chunks (Fable-led per plan), fallback-rate as the metric.
6. #213 with §3's numbers (R2 / Target-A).
