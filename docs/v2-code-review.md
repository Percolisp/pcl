# v2 code & design review — can it be simpler, can it be clearer?

Session 316t (2026-08-01, Fable), at gen v2-88, census 111/111 v2-native.
Method: full read of the small modules (ExprToCL2, CLForm), structural read
of Parser2's entry/dispatch, two systematic inventories over Parser2/Parser
(token-shape fast paths; v1-shared mutable state; v1 reachability), live
probes for every divergence claimed here.  Line refs are at commit f74a623.

**Verdict in one line: the architecture is right and the invariants are the
correct ones, but the complexity center is not where the plan says it is —
it is the *seam*, not v1 itself — and the deletion step (E4.1) as written
will remove far less code than the 22k→14k expectation suggests.  The
simplification that actually pays is: one statement-splitter, one seam
object, one expression brain.**

---

## 1. What is already right (keep, and defend)

- **CLForm as the only text producer** (`Pl/CLForm.pm`, 239 lines).  Parens
  balance and indentation-encodes-depth *by construction*; nothing re-parses
  emitted text.  This is the single best structural decision of the rewrite.
- **Gates die hard with named reasons** (`Parser2 TODO: …`, ~28 distinct
  messages).  Every unsupported shape is enumerable and greppable; nothing
  silently degrades — with the exceptions in §2, which is exactly why §2
  matters.
- **The census/seam instrumentation** (T0.2) gives real numbers instead of
  impressions: 111/111 files native, 2,266 native vs 16,843 seam
  expressions (88.1% seam).
- **The tree-walk VarAnnotator** (W12) with its event vocabulary and
  conservative default is a model piece: one page of contract comment, one
  verdict, dual-run diff history.
- **Pre-pass source rewrites that reparse** (state family,
  `_normalize_tie_my`) rather than surgically editing live trees mid-lower:
  contained, and the reparse re-establishes invariants.

## 2. Finding 1 — hand-rolled precedence knowledge, N copies (correctness)

s316t fixed the `$lex = RHS` statement fast path folding a depth-0
`,`/`or` tail into the RHS (`_tail_below_assign_prec`, Parser2:5056).  A
systematic sweep finds this is a *family*, not an instance.  There are now
**three separate hand-rolled precedence tables**:

1. `_tail_below_assign_prec` (5056) — `, => or and xor`
2. `_state_init_end` (3180) — stops at `;`, statement-shaped depth-0 comma,
   and `or and xor not if unless while until for foreach when` — a
   *different* set (`not`, `when`)
3. the `return`-list comma gate (4890) — `,`/`=>` incl. one-level
   parenthesized-list peek

and **unguarded siblings of the fixed bug**, both confirmed by live probe:

- `_single_scalar_decl` (6370): `my $x = 1, $y = 2;` → v2 gives x=2
  (perl: x=1 — the comma splits; init is `1`).  Task #138.
- `_extract_params` (4060): matches `my (LIST) = @_` with `@k >= 4` instead
  of exact arity, so `my ($a) = @_, g();` **silently drops the `, g()`
  tail — the call never runs** (perl runs it).  Silent statement-text
  deletion is the worst failure mode this codebase has; the fixed bug at
  least computed a wrong value.  Task #138 (extended).
- state scalar init peel (4270) — same "everything after `=`" slice,
  unguarded.
- The C-for init site (5247) *documents* this exact bug class and guards it
  locally — the knowledge exists but lives at one call site instead of in
  the shared helper.  **(s316u: its guard covers only the `>= 2` multi-`my`
  case; the single-`my` sibling `for (my $i = 0, $j = 9; …)` folded and the
  loop ran ZERO times.  A local guard is not a shared one.)**
- **(s316u, not in the original list)** v1's `local` handler
  (`Pl/Parser.pm:3735`) slices the same "everything after `=`" run —
  `local $l = 1, $m = 2` gave `$l == 2`.  It is a LIVE v2 bug too: v2 routes
  `local` through `_lower_local` → the v1 seam.

**Recommendation R1-a (small, do before release): one splitter.**  A single
`_split_at_lowprec($toks, %opts)` owning the one table (with the
`statement-vs-expression comma` and `not`/`when` variations as options),
used by: the two statement fast paths (already via the new guard),
`_single_scalar_decl`, the state init peel, `_extract_params` (as an exact
arity check), `_state_init_end`, and the return gate.  This is CLAUDE.md
rule 11 applied to Parser2's own internals: the sibling mechanism exists
since s316t; normalize the other six sites into it.  Estimated: one short
session including probes.

**Why not "just ask PExpr"?**  The principled answer is that PExpr owns
precedence, and every fast path should parse-then-inspect.  The obstacle is
real, though: PExpr's parse is *destructive* (fat-comma rewrite via
`set_content`, adhoc parse-state keys), which is why `_lower_expr` carries
snapshot/restore machinery (5750-5800) around every native attempt.  Until
PExpr has a non-mutating parse mode, "parse to classify" costs a
snapshot/restore per statement and risks side-effect duplication (the
`_v2_embed` discipline exists precisely because a *discarded* parse must
not run lowering hooks).  So: shared-table splitter now; a read-only PExpr
classifier is the E5-quality endpoint that would delete the table
entirely.  The destructive parse is itself a clarity debt worth an E5 line
item — it is the root cause of both the snapshot machinery and the
fast-path culture.

## 3. Finding 2 — the seam is the complexity center (clarity + fragility)

The inventory of mutable state Parser2 shares with its embedded v1 parser
is the largest single source of incidental complexity in the pipeline:

- `_let_bound_vars`: ~20 touch points, **eight** hand-written save/restore
  pairs (sub hoist, block scope, if/while/for/foreach cond-`my`, two
  speculative embed sites) plus a conditional pair in
  `_fallback_stmt_capture` (save only for compounds).  Each pair is a leak
  bug waiting to happen — the C-for site's comment even records one (stale
  string-eval capture alist).
- Bucket machinery: the save/reset/drain/restore dance is written out
  **twice** (`_lower_expr` fallback 5826-5851; `_fallback_stmt_capture`
  6303-6363), differing only in target bucket and drain routing, each
  juggling 3-4 slots (`_sections`, `_cur_bucket`, `indent_level`,
  `_block_depth`, `_local_let_depth`).
- `_v2_embed` is `local`-ed on and off at five sites with a subtle rule
  (off during native attempts and analysis parses, on during real
  lowering) enforced only by comments.
- Environment regime state (`wa_void_active`, `in_subroutine`,
  package/scope stacks) is `local`-ed or push/popped at another dozen
  sites; #49 (do-block package leak) was exactly a missed pop.

**Recommendation R1-b (E5, high value): one seam object.**  A
`SeamSession` (or two constructors: `for_expression`, `for_statement`) that
owns *all* of: bucket save/reset, `_v2_embed` arming, depth counters,
`_let_bound_vars` scoping policy, the drain, and restore-on-scope-exit
(guard object, so early `die` cannot leak state).  The eight
`_let_bound_vars` pairs become one `scoped_lexicals { ... }` helper.  This
removes no capability — it makes the existing discipline structural instead
of copied.  Most future seam bugs die here.

## 4. Finding 3 — E4.1 "delete v1" needs re-scoping (plan accuracy)

Reachability analysis says the flag-day deletes much less than the plan's
framing implies:

- Deletable at E4.1 as-is: v1's file-level entry/assembly/forward-decl
  passes (~550 lines of Parser.pm), the ~45-line `pl2cl` gate
  (`parse_with_fallback`, `PCL_V1`), the runtime cache-key branch, a
  handful of test/env consumers.  **Order ~600-700 lines, not 8,000.**
- Still live after E4.1: the entire v1 statement layer (~6,700 lines)
  through `_fallback_stmt` → `_process_element` (12 intra-file call sites:
  use/require/BEGIN-END, `local`, prototype/signature subs,
  goto/next/last/redo, multi-element `return`, loop statement-modifiers,
  anon-hash-as-block); the block-to-string family through PExpr's embed
  declines; all of ExprToCL through `_parse_expression_form`;
  preprocessing, prototype/signature parsing, and naming helpers (genuinely
  shared — these get *ported*, not deleted).
- One v1-only bypass hides in the driver: bundle mode calls
  `Pl::Parser->parse_file` directly (`pl2cl:283`) — must be ported first
  or E4.1 breaks bundles.

**Recommendation R1-c: restate E4.1/E5 in the plan as a burn-down, not a
deletion.**  E4.1 stays valuable (it removes the *dual-pipeline* property:
no more per-file fallback, no PCL_V1, cache key single-dialect, gates→hard
errors) but its line-count yield is small.  The 22k→14k arrives via E5
steps, each independently shippable:
  1. make `lower_embedded_block` total (closes the 4 remaining
     `body_cl`/`raw_lambda` raw sites and the block-to-string family —
     this is task #78's remainder, already tracked);
  2. burn down the 12 `_fallback_stmt` classes one at a time, each
     retiring its v1 `_process_*` handler;
  3. then delete `raw`/`raw_wrap` and the text half of ExprToCL
     (`gen_node` + 36 string emitters + the 25-entry handlers table) once
     the two structural decline paths (`ExprToCL:546,571`) can no longer
     fire.
The current #78 note "remaining raws all die WITH v1 at E4" is accurate
for the `raw()` *sites* but not for the text-generator half — that needs
step 3's decline-closure first.

## 5. Finding 4 — two expression brains (duplication)

`ExprToCL2` (264 lines) natively lowers ~12% of expressions; everything
else goes through ExprToCL — which since E2 *also* emits CLForm trees.  So
the original justification (text vs tree) is gone; what remains is a perf
carve-out that duplicates semantic decisions: the BINOP table, the
string-escape/interp subset, the context-bind rules, and the
`argbox` head-swap — task #131 had to be implemented in **both** brains
(six seam sites + the native head-swap), and every future calling-
convention change pays the same double cost.

**Recommendation R1-d (E5, as planned but with a direction): fold
ExprToCL2 into the one emitter as early-return branches** inside
`gen_node_form`, keyed on the same node shapes, sharing the one BINOP/ctx
table.  The raw-slot verdict then selects an emission *mode*, not a
different emitter.  The plan already lists "fold ExprToCL2 into EmitCL";
the point here is that the fold should *delete the duplicated tables*, not
just relocate the file.

## 6. Finding 5 — implicit contracts that should be code

- **VarAnnotator ↔ Parser2 native-root agreement.**  The unboxable verdict
  is only sound if the annotator's "statement-root write" classification
  and Parser2's fast-path shape recognition agree; today that agreement is
  comments on both sides ("this branch and the verdict key on the same
  shape").  s316t added a third participant (the new guard) and the safety
  argument was made by hand.  Export one shared predicate
  (`native_root_write($stmt)` returning the split) consumed by both the
  annotator's event walk and the statement fast path — drift becomes
  impossible instead of argued-about.
- **Context encoding.**  Three representations of one concept: v2 strings
  (`undef`/`'nil'`/`'t'`/`':void'`/`'inherit'`, plus a `'1'` coercion),
  PExpr numeric constants (0-3), and the emitted `*wantarray*` values —
  with mapping tables at each seam (`_lower_expr` has both).  One set of
  shared named constants; mechanical E5 cleanup.

## 7. Clarity — organization

- `Parser2.pm` is 6,562 lines / 130 subs in one file spanning six phases:
  source pre-pass rewrites, facts scan, capture/span promotion + rename,
  segmentation, statement lowering, seam management.  An E5 file split
  along exactly those phase lines (no behavior change) would make the
  module navigable and make the phase boundaries — which are already real
  in the code — visible in the tree.  The state-family pre-pass alone
  (~600 lines, 3032-3640) is a self-contained module today in all but
  filename.
- Several dispatch functions substantially exceed the size the project
  enforces for Lisp (~80 lines); `_lower_block` and the compound lowering
  are the worst.  Same discipline, same reason: a function that fits on a
  screen has checkable control flow.
- The comment discipline is genuinely good — most fast paths cite the test
  that motivated them.  The review's complaint is not missing comments; it
  is that several *load-bearing rules* (embed-hook arming, verdict
  agreement, bucket dance order) exist **only** as comments.

## 7b. Execution feedback (s316v)

`docs/opus5-review-requests-s316v.md` carries four items back from
execution.  The one that bears on this document: **both consolidations
predicted here found live bugs — 2 for 2** (#138 a silently deleted
statement and a zero-iteration loop; #140 two divergences from an operator
set missing `&.= |.= ^.=`), which is an argument for pulling E5.5's shared
predicates — or at least the non-mutating PExpr parse mode §2 depends on —
earlier than last.  It also records three failed attempts at bareword class
names, whose failure shape lands inside `pexpr-term-parsing-review.md`'s
Option B region.

## 8. Recommended order

**Before R1 (cheap, correctness):**
- ~~#138 extended~~ **DONE (s316u).**  The table now lives once, in
  `Pl::PExpr::TokenUtils::lowprec_idx` / `lowprec_split_safe` (plain subs —
  both statement parsers need it and neither may depend on the other).
  Six consumers: the two statement fast paths (via
  `_tail_below_assign_prec`), the `my`-decl init, the state init peel,
  `_state_init_end`, the single-`my` C-for init, and v1's `local` handler;
  `_extract_params` became an EXACT-arity match.  Two sites found beyond the
  review's list — the single-`my` C-for init (`for (my $i = 0, $j = 9; …)`
  ran ZERO iterations) and v1's `local` (live in both pipelines, since v2
  routes `local` through the v1 seam).
  **Resolution note:** the fix is NOT a span-returning splitter.  Wherever
  the caller can hand the whole `$x = …` run to PExpr it does, because PExpr
  already owns the parenless list-op ambiguity (`my $c = h 1, 2` keeps both
  args) and a span splitter would have to re-derive it.  Only the state
  once-guard must interpose code between head and tail, so only it splits —
  guarded by `lowprec_split_safe`, which declines an ambiguous comma and
  leaves the run whole (keeping `state $c = \substr $s, $i, 1` v2-native).
  **Residual:** `state $s = f 1, $t = 2` — an ambiguous comma after a
  parenless list operator in a state init — still folds; resolving it needs
  PExpr's arity knowledge, i.e. the read-only classifier this section
  proposes as the E5 endpoint.
- The `return`-list comma gate (4890) deliberately did NOT move to the
  shared table: it is a *comma-list detector* answering a different question
  (does this return need v1's list spreading?), and adding `or`/`and`/`xor`
  to it would gate returns that lower natively today.

**E4.1 (unchanged in spirit, re-scoped in expectation):** kill the dual
pipeline (gate, PCL_V1, cache key, v1-only entry/assembly, bundle-mode
port), gates → hard errors.  Expect ~600-700 lines, not thousands.

**E5 (restructured as five independently-shippable steps):**
1. seam object (Finding 2) — biggest fragility payoff;
2. embedded-block totality → delete block-to-string + raws (#78 tail);
3. `_fallback_stmt` class burn-down → v1 statement layer shrinks for real;
4. fold ExprToCL2 into the emitter, deleting duplicated tables (Finding 4);
5. shared predicates/constants (Finding 6) + phase file-split (Finding 7).

The 22k→14k goal stays plausible — but it is earned in E5 steps 2-4, and
the plan should say so.
