# One compiler, the simpler way — the plan (s411, Fable, 2026-08-18)

*Answers task #379: "the two-compiler entanglement seems much too complex —
can it be done in a simpler way than E5.1–E5.5 (9–17 sessions)?"  Yes.
This document is the plan, sized from measurements taken this session, and
it REPLACES the E5.1–E5.5 ordering in `docs/v2-endgame-plan.md` as the
worklist (that file's E5 section is history now; its target,
`docs/v2-target-architecture.md`, is unchanged — this is a shorter road to
the same shape).  The USER's framing (2026-08-18): structural progress,
"but not at any cost" — every step below names its bar.*

## 0. The two answers, up front

**Q1 — simpler than E5.1–E5.5?**  Yes: **3–5 Opus sessions to the release
shape instead of 9–17**, because two of the five E5 steps turn out to be
almost empty once measured (§2), one is a 40-line helper instead of an
object, and the long tail (E5.3, the 12 statement classes) does not have to
precede the release at all — the seam that reaches those classes becomes
ONE stateless function, which is all "one compiler" needs to mean for
v0.1.

**Q2 — "one compiler" for the release = one GENERATOR or zero v1 lines?**
One generator + one statement entry.  Zero v1 lines is E5.3, class by
class, after the release (§4), each port deleting one `_process_*`
handler; nothing in the release shape depends on it.

## 1. What is actually there (measured s411 — do not re-derive)

Numbers over the 111-file `perl-tests` corpus unless stated.

| fact | measurement | source |
|---|---|---|
| expressions lowered by ExprToCL2 (the "native" generator) | 2 270 of 19 174 (11.8 %); the other 88.2 % re-parse and lower through v1's ExprToCL | `tools/v2-census.pl` |
| **the ENTIRE emission difference between the two generators** | **two rules**: (a) `=` onto a let-bound container's element emits `(setf (p-gethash/p-aref …) V)` instead of `(p-setf …)`; (b) a call to a `*wantarray*`-INSENSITIVE user sub gets no `(let ((*wantarray* X)) …)` bind.  643 diff lines in 24 files, nothing else — no string, number, operator or context shape differs | native attempt forced off (`return undef` at `ExprToCL2::gen_form`), `tools/corpus-diff.pl --show=all` vs HEAD |
| compile-time cost of the native attempt (a full PExpr parse that is discarded 88 % of the time) | whole-corpus transpile 68.4 s → 62.3 s with it off (−9 %) | timed loop, this machine |
| embedded blocks (map/grep/sort/eval/do/sub bodies) reaching `lower_embedded_block` | 1 064 (eval 379, sub 292, map 166, do 122, sort 86, grep 19) | instrumented decline sites |
| **its declines** | **12 (1.1 %)**: 7 `embed-unsafe: raw_wrap` (a `local` statement inside the block — v1's open-paren text), 4 `tail-Include` (`eval { require X }`), 1 tail decl not convertible | same |
| **embedded blocks compiled by v1's TEXT route and then DISCARDED** | **≈ 1 500 per corpus** — 924 inside the native attempt (`_v2_embed` cleared, so PExpr routes the block to `parse_block_to_cl_string`, ExprToCL2 then declines the enclosing call), 882 in the file-level analysis parse, 27 in `_expr_scalar_rooted`, 85 in `_lower_sub_inner`… — i.e. **every embedded block is compiled ~2.4×, 1.4 of them for nothing** | `PCL_E2_RAW_CENSUS=1` `decl:no-hook` rows by caller chain |
| where compile time goes (12-file sample, 50.4 s) | v1 `_process_element` 11.2 s incl. (embedded blocks + the 12 classes), `PExpr::parse` 9.0 s (14 147 top-level parses), `_scan_lex_facts` 5.7 s, VarAnnotator's own expression parses 3.4 s, `_interp_names` 3.4 s (1.65 s EXCLUSIVE — one sub), `_lower_block` 0.95 s excl. | `tools/sub-call-census.pl --sample 12` (new this session) |
| the bucket dance | THREE copies: `_lower_expr` (Parser2.pm:8717–8754, 40 lines), `_fallback_stmt_capture` (9527–9608, 82 lines), `VarAnnotator::_tw_expr_parse` (689–728, no drain — output discarded) | seam survey |
| ExprToCL → v1 Parser reach | 4 accessors, ALL through `lex_home` (which under v2 returns the Parser2 object) + one write `{_eval_env_used}` read only by Parser2 — ExprToCL never touches sections/buckets/`_emit` | dependency survey |
| the 12 statement kinds v1 lowers whole (+ `local`) | 1 051 statements per corpus: local 310, use 229, no 119, BEGIN 119, sub-with-sig/proto 106, plain-with-loop-modifier 42, push 33, require 30, delete 14, return-list 11, my-with-loop-modifier 10, … | seam census |
| context encodings | three (PExpr integers, the seam's strings `'t'/':void'/'inherit'`, runtime `t/nil/:void`); the string one exists ONLY to talk to ExprToCL2 and is mapped back for the fallback | survey |

The complexity, then, is not the two generators' OUTPUT — that differs by
two rules.  It is the machinery around the second attempt: a discarded
parse, a discarded block compile, a snapshot/restore, an extra context
encoding, a census, and a bucket dance, all wrapped around every one of
19 174 expressions.  Delete the second attempt and most of the wrapping has
nothing left to wrap.

## 2. The plan — four phases, each shippable alone

Every phase ships behind `tools/prove-core` + `tools/corpus-diff.pl`; a
phase marked IDENTICAL must be byte-identical (it is a refactor and the
diff proves it); a phase marked EMISSION carries a generation bump, the
three artifacts, the full sweep (TOTAL/LOST/DROPS unchanged) and
`tools/bench-exec.pl` on the touched shape.  Compile time: the whole-corpus
loop (`for f in perl-tests/*.t; do ./pl2cl < $f >/dev/null; done`, 68.4 s
at s411 on this machine) before and after every phase — the "not slower
later" number.

### Phase R — the optimization registry (½ session, first, IDENTICAL)

The USER's stated target: optimizations behind a flag, so they can be
worked on after the compiler is done.  Today: no registry, one ad-hoc knob
(`PCL_NO_RAW_VERDICT`).  `docs/v2-target-architecture.md` §3 designs the
mechanism; nothing about it needs E5.4 except the Kind-B pass hooks'
CLOSED tree.

- New `Pl/Passes.pm`: `enabled($name)` for Kind-A (facts-licensed
  emissions) and `register_pass($name, \&f)` / `run_passes($form)` for
  Kind-B (CLForm→CLForm), an ordered registry, empty at first.
  `PCL_OPT=none | -name,+name,…` parsed once at load; `PCL_NO_RAW_VERDICT`
  becomes an alias of `-raw-numeric` (documented, kept).
- Name and gate the FIVE existing Kind-A transforms at their emission
  sites: `raw-numeric` (VarAnnotator's B verdict, `native_root_write`),
  `str-buffer` (S1), `foreach-range` (`p-foreach-range`, Parser2 ~8146),
  `insensitive-call` (arrives with Phase A), `elem-setf` (Phase A).  Each
  gate reads `Pl::Passes::enabled(...)` and falls to the general form —
  which is what "facts license emissions" already guarantees exists.
- `run_passes` is called at the ONE point where a lowered top-level form
  is handed to the printer.  With no passes registered it is the identity.
- Bar: `PCL_OPT` unset → corpus-diff IDENTICAL; `PCL_OPT=none` → the gate
  and the sweep still pass (the general forms are exercised — that is the
  first time all of them are, together); a Pl/t row per name asserting
  the flag changes emission.  #73/#74/#77 land later as Kind-B passes,
  one file each, exactly as the architecture doc says.

### Phase A — one expression compiler (1–2 sessions, EMISSION)

E5.4 as measured, not as estimated.

- **A1** Port the two rules into `Pl/ExprToCL.pm`: `insensitive-call`
  (needs `sub_info` on ExprToCL — Parser2 constructs the generator itself
  from now on and passes `_cur_sub_info` + `_let_bound_vars` as
  `lexicals`; the wantarray bind lives in `_ctx_wrap_form`) and `elem-setf`
  (port `ExprToCL2::_elem_place`'s predicate EXACTLY: plain `$name` base,
  `%name`/`@name` in `lexicals`, not state-renamed, key a plain
  scalar/number/string — widening the key predicate is a separate
  measured item; note the ARGBOX rule already lives in ExprToCL at
  `_elem_accessor`, so this closes #131's double implementation too).
- **A2** `Parser2::_lower_expr` = ONE parse (`Pl::PExpr->new(parser =>
  fallback_parser)`, `annotate_contexts`, `ExprToCL->gen_node_form`); delete
  the native attempt, `_ppi_state_snapshot/restore` around it, the
  `$native_ctx` string encoding and its map-back, `_expr_via_fallback`
  (its reader at ~7739 sees "always fallback"), the `PCL_V2_SEAM_CENSUS`
  expression half.  `_seam_lex_assign_fix` goes too: ExprToCL emits
  `p-my-=` directly for a name in `lexicals` (the v1 `_emit` text rewrite
  stays for v1-routed statements until each class is ported).
- **A3** delete `Pl/ExprToCL2.pm`; retarget `tools/v2-census.pl` (the
  88 % metric is gone by construction — the metric that replaces it is the
  raw-residue count, `PCL_E2_RAW_CENSUS`, plus the statement seam count).
- **A4** one DIALECT too: v1's four `->generate(` (TEXT) sites in Parser.pm
  (`_parse_expression_internal`, `_compile_constant_value`,
  `_compile_default_expr`, `parse_hash_block_to_cl_string`) switch to
  `to_flat(gen_node_form(...))`.  E2 made the two dialects byte-identical
  under a dual-run diff, so corpus-diff should be IDENTICAL; if it is,
  delete the text twins (`generate`, `gen_node`, `gen_internal_node`,
  `gen_internal_node_text`, `gen_leaf`, `gen_binary_op` — 25 of whose lines
  are an exact copy of `gen_binary_op_form`, duplicate family 22 —
  `gen_inline_lambda`, the last `handlers` entry), keeping ONE
  `_symbol_text($id)` for the nine deliberate "I want the bare name" uses.
  Measured s411 (`PCL_E2_RAW_CENSUS` over the corpus): the text path is
  reached from `gen_node_form` only for `inline_lambda` nodes whose body is
  v1 TEXT (94 — ~50 of them inside v1-routed sub-with-signature statements,
  the rest under discarded analysis parses) and 2 `PPI::Token::Number`
  leaves; so `gen_inline_lambda_form` takes a text body as a `raw` inside
  the lambda form (E2's residue rule), the Number leaf gets its missing
  format, and the two decline sites (ExprToCL.pm:551, :576) DIE (rule 12: a
  form handler that declines is a compiler bug, not a fallback).  Bar:
  IDENTICAL, then the census reports zero `pcl-raw node:`/leaf events over
  both populations (`tools/gate-set-scan.pl`-style, both stderr sets).
- Bar: A1 alone should reproduce the native forms byte for byte AND
  produce MORE of them (the fallback path used to lose the two rules on any
  expression the all-or-nothing native attempt declined) — every corpus-diff
  hunk is one of the two shapes, verified by a normalizer (`(setf (p-X …) V)`
  ↔ `(p-setf …)`, bind ↔ no bind) as the s410 ask 7.7 (a) shape; sweep
  clean; bench neutral-or-better on the arithmetic benches; compile time
  ≤ 62 s (the native attempt's 9 % back).
- What is NOT in Phase A: PExpr stays as it is (its `parser` attribute,
  the has_parser gates, the destructive `cleanup_for_parsing`); embedded
  blocks still reach v1 on the 12 decline shapes.

### Phase B — one seam function, and no discarded block compiles (1 session, IDENTICAL)

E5.1 as a function, not an object.

- **B1** PExpr attribute `analysis_only => 1`: the three block sites
  (`:3248` map/grep/sort, `:3401` eval/do/&-proto, `:3537` anon sub) leave
  the body uncompiled — no `_v2_embedded_body`, no `parse_block_*`.  Used
  by `VarAnnotator::_tw_expr_parse` and `Parser2::_expr_scalar_rooted`;
  their save/redirect/restore blocks are deleted (nothing can emit).  This
  removes ~900 discarded v1 block compiles per corpus and the third copy of
  the bucket dance.  Bar: IDENTICAL (analysis trees never reach emission;
  `_tw_walk` reads block statements through PPI, not through the compiled
  body — verify by grep before deleting), compile time measured with
  `tools/sub-call-census.pl` (`_process_element` incl. must fall).
- **B2** `Pl::Parser::capture_v1(sub { … })` (name to taste — it lives in
  Parser.pm, where the sections are): saves `_sections/_cur_bucket/
  indent_level/_local_let_depth/_block_depth`, installs the scratch section
  and the `_v2_embed` hook, runs the code, drains ALL buckets by name,
  restores, returns `{ runtime => \@r, decls => \@d, defs => \@f, opens => N }`.
  `_lower_expr` and `_fallback_stmt_capture` become callers; Parser2 stops
  reading any of those five fields (grep count → 0).  Bar: IDENTICAL.
- **B3** `lower_embedded_block` handles its own decline: instead of
  returning `undef` and letting PExpr call `parse_block_to_cl_string`
  behind Parser2's back, the hook ALWAYS answers — on the 12 decline shapes
  it calls v1's block compiler itself through `capture_v1` and returns the
  captured text as a `raw` form (the drained hoists go to `_captured_decls`
  from here, the one place).  Then `_lower_expr`'s own drain is deletable
  (the expression path no longer emits into v1 buckets from anywhere), and
  PExpr's two-route block logic collapses to "ask the hook".  Bar:
  IDENTICAL; `PCL_E2_RAW_CENSUS` `decl:no-hook` count → 0 in lowering
  parses.

### Phase C — the 12 decline shapes (½–1 session, EMISSION for the two fixes)

Not a burn-down of `_fallback_stmt`; just the two shapes that keep an
embedded block off the structural route:

- `raw_wrap` inside an embedded block (7 sites): a `local` statement in an
  eval/do body.  Either teach the embed printer to render `raw_wrap`
  (`Pl::CLForm::to_flat` dies on it today, CLForm.pm:144 — inside a
  lambda body the wrap is closed by construction, so this is a printer
  case, not a semantics change), or lower `local` natively (E5.3's
  biggest class, 310/corpus — the right long-term answer, but its own
  session).  Take the printer route first; measure.
- `tail-Include` (4 sites): `eval { require X }` — lower `require` as an
  expression-position statement (v1's `_process_include_statement` already
  produces `(p-require "X")`; the tail-value semantics are the only new
  part).
- After C, `lower_embedded_block` has no decline path; `_ppi_state_
  snapshot` around it goes (nothing re-parses); `capture_v1` has exactly
  one caller left, `_fallback_stmt_capture`.  Bar: EMISSION for the two
  fixes (sweep), IDENTICAL for the deletions.

**Release shape after R+A+B+C (3–5 sessions):** one expression compiler
(PExpr → annotate → ExprToCL forms, ONE dialect, constructed by Parser2), one statement
compiler with ONE stateless call into v1 for the 12 classes + `local`, no
discarded parses, no snapshot/restore in the lowering path, one context
encoding in the compiler (PExpr's integers; the runtime's values are the
target), an optimization registry with five named Kind-A gates and empty
Kind-B hooks, and `Pl/ExprToCL2.pm` gone.  `Pl/Parser.pm` still exists
and is still ~9 k lines; that is §4.

## 3. Compile time — the numbers this plan is held to

| step | expected | why |
|---|---|---|
| Phase A | −9 % (68.4 → ≤ 62 s) | the discarded second parse of 88 % of expressions, measured with the attempt forced off |
| Phase B1 | a further cut, size measured by the tool | ~900 v1 block compiles per corpus whose output is thrown away; `_process_element` is 22 % of compile time today |
| Phase B2/B3/C | neutral | refactors; drains move, they do not multiply |
| Phase R | neutral (a hash lookup per gated site) | — |

Two hot spots the census found that are NOT this plan's but are recorded
so nobody re-measures them: `Parser2::_interp_names` (1.65 s exclusive in
a 50 s sample — one sub, family 27 of the duplicate census, an InterpScan
consumer-2 site) and `_scan_lex_facts` (5.7 s inclusive over 345 calls).
Both are #213 material; the second is worth a look when Phase B lands
because B1's flag may already remove its parse cost.

## 4. After the release — the long tail, in the order the numbers give

1. **E5.3, `local` first** (310/corpus): the biggest class and the cause
   of the `raw_wrap` decline shape; then the Include family (`use`/`no`/
   `require`, 378 — mostly `sched => 1` capture, one mechanism), then
   BEGIN/END (119), sub-with-signature/prototype (106 — Parser2 already
   lowers plain subs; the signature desugaring is the delta), the
   loop-modifier family (`EXPR for LIST`, `return … while`, ~90 — one
   `_lower_modifier_loop`), return-list (11), the anon-hash-as-block PPI
   mis-lex (rule 13, already logged).  Each port deletes one `_process_*`;
   the calibration stays 2–3 classes/session; when the last one goes,
   `capture_v1` and Parser.pm's statement layer go with it.
2. **§2.4 non-destructive PExpr** (2–4 sessions): with A2 there is ONE
   lowering parse per expression, so the remaining mutations
   (`replace_child` for `$h{bar}`→`$h{"bar"}`, the `CORE::` `set_content`,
   the three ad-hoc keys) matter only to the analysis parses, and B1 makes
   those parse the SAME tokens with the same PExpr — the snapshot/restore in
   `_tw_expr_parse` is the last one standing and is what §2.4 deletes.
3. **PExpr's `parser` attribute** shrinks to the facts it reads
   (`_is_known_callable`, `_sort_pair`'s package region, the CL package
   designator) — an `environment`/host object, not a Parser; the
   `has_parser` gates become "is there a host".  Small; do it when B3 has
   made the block route hook-only.
4. **Kind-B passes**: #73 inline method cache, #74 pack/sprintf template
   memo, #77 return-family transfer — one file each on Phase R's registry,
   corpus-diff + bench + sweep per pass, as `v2-target-architecture.md` §3
   already specifies.
5. **Boxed aggregates** (E5, Fable design) — unchanged, after v0.1.

## 5. What this plan deliberately does NOT do

- **No `SeamSession` object** (E5.1 as designed): with one generator and
  hook-only blocks, the seam is one function with one caller; an object
  owning "depth counters, `_v2_embed` arming, `_let_bound_vars` scoping"
  would encapsulate a dance that mostly no longer happens.
- **No `_fallback_stmt` burn-down before the release** (E5.3): 1 051
  statements per corpus, ten months of quirks in v1's handlers, and the
  sweep the only oracle — the calibrated 4–8 sessions buy zero simplicity
  for the release shape once the seam is one call.  It is the post-release
  worklist (§4.1), and every fix landing in a v1 handler until then is
  FILED on its class so the port re-verifies it (s410 ask 7.1).
- **No rewrite of `parse()`'s main loop, no `_reduce_term` first**: Option
  B phase 2 (`docs/option-b-phase2-plan.md`) is orthogonal — term grammar
  inside PExpr — and stays queued after this plan (see §6).
- **No extraction inside code this plan deletes**: `Pl/ExprToCL2.pm`, the
  three bucket-dance copies, the native attempt, `_seam_lex_assign_fix`;
  and no extraction inside v1's `_process_*` handlers, which §4.1 retires
  class by class.  The duplicate-code worklist
  (`docs/dup-census-worklist-s411.md`) is tagged accordingly.

## 6. The queue, re-ordered (USER, 2026-08-18: structural first, not at any cost)

Opus sessions, in order:

1. **Phase R** (½) + **Phase A** (1–2) — R first, then A1–A3 in one
   session and A4 in the next if it does not fit; A carries the sweep.
2. **Phase B** (1).
3. **Phase C** (½–1) + the first batch of the duplicate worklist's
   EXTRACT items (`docs/dup-census-worklist-s411.md` §2, each
   corpus-diff IDENTICAL, cold code first, hot code with the timing).
4. Then the previous queue resumes where it stood (`docs/plan-post-s408.md`
   §2): #281 items 1+2+6 (with the s410 7.7 (a) normalizer — it is the same
   tool Phase A's bar needs, so it likely already exists by then), Option B
   phase 2 (#371 → #372 → #343 → #369/#370 → the flip), then release
   phases 3–5 (#279 → #280 → #282 → #283).

Standing rules for the reorder:

- **A newly found silent-wrong is FILED with its reproducer and jumps the
  queue only if it regresses a baseline or blocks a phase.**  The baselines
  (pass/fail, drop census, gate) pin it; it costs the same to fix later.
- **A fix that lands in code a phase deletes is wasted** — check the
  deletion list (§2, §5) before fixing in `ExprToCL2.pm`, the seam
  wrappers, or a `_process_*` handler; if the shape is reachable and the
  fix is small, fix it AND file it on the class (7.1).
- **Fable sessions**: the rulings that need one (a per-session
  `opus5-review-requests-sNNN.md` still lists them), the phase's design
  questions if any, probes when a change is semantic — not a cold
  re-verification of a gate Opus already ran.  Fable's next own item is
  the B1 operand grammar (Option B phase 2) when the queue reaches it.

## 7. Files this session touched / created

- `tools/dup-census.pl` — the duplicated-code census (families, exact/shape
  levels, `--calls` hot/cold tags, `--tsv`).
- `tools/sub-call-census.pl` — per-sub call counts and time over a corpus
  sample (Devel::NYTProf; installed into the perlbrew perl this session).
- `docs/dup-census-worklist-s411.md` — the family-by-family verdicts.
- `docs/fable-answers-s410.md` — the s410 asks, ruled short.
- Tasks: #379 → in_progress with this plan; #383 Phase R, #384 Phase A,
  #385 Phase B, #386 Phase C, #387 the extraction worklist.
