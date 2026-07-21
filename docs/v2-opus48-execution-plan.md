# v2 Remaining Work — Execution Plan for Opus 4.8

**Written:** 2026-07-12 (session 285, Fable), for **Opus 4.8 to execute**.
**State updated s306b (2026-07-21, Fable): task #78 STEP 2 SHIPPED — the
`raw_lambda` seam: do{} + expression `sub {…}` bodies lower through
Parser2 too (`_lower_embedded_anon` = named-sub body regime inside v1's
exact `&rest %_args` wrapper form; do{} = `(funcall (lambda () (progn …)))`
with loop transparency; func_ref carries `lambda_form`).  Gen v2-51; gate
118 files / 4336 green; PCL_V1 byte-diff zero, failure set = HEAD + the 1
relocated v2-only guard; 55-file sweep 0 new/0 fixed; fuzzer 1056/1060.
NEW: guard file `Pl/t/transpile-test-06.t` — future transpile guards go
there (04b and earlier are large/slow).  raw_lambda remains only for
&-prototype block args (named-defun route), the no-parser fallback, and
declined shapes.  E2 REMAINING: hash-constructor blocks, declined shapes,
empty-shape quirks, `\(RANGE,…)`, then E2.final (text printer +
raw/raw_wrap deletion — bulk gated on E4.1 since the seam's
_parse_expression contract is v1 text).**
**Previous state s306 (2026-07-21, Fable): task #78 CORE SHIPPED — the
`inline_lambda` re-host: map/grep/sort/eval{} block bodies lower
structurally through Parser2's `lower_embedded_block` (PExpr `_v2_embed`
hook; body_form CLForms on the node; `gen_inline_lambda_form`), with
transactional declines to v1 text for hash-constructor / deref-chain /
tail-modifier / tail-decl / tail-compound / package-in-block / raw_wrap
shapes.  Gen v2-50; gate 117 files / 4330 tests green; PCL_V1 byte-diff
zero and failure set = HEAD + 1 new v2-only guard; 49-file sweep 0 new/
0 fixed; fuzzer 1056/1060 (same 4 documented).  REMAINING for E2.final:
do{}/anon-sub `raw_lambda` bodies, hash-constructor blocks, the declined
shapes, empty-shape quirks, `\(RANGE,…)`, then delete text printer +
raw/raw_wrap (ExprToCL2 folds away).  s306 session-log entry has the
detail (incl. the two v1 latent bugs fixed on v2: tail_position clobber,
bare `...` statement).**
**Previous state s295c-2 (2026-07-18, Fable): census 106 v2-native / 5 gated,
cache gen v2-37, v2 gate green (115 files / 4073 tests).  Conditional-my +
Scheduled-block (BEGIN/END) capture promotion shipped; lfs.t de-gated;
closure.t's residual gate = bare fork-pipe open (runtime gap, task #70 =
the de-gate path); remaining gates: chdir, postfixderef, state,
signatures, closure (see e1-remainder.md s295c-2 update).**
**Previous state s295b+c (2026-07-18, Fable): census 105 v2-native / 6 gated,
cache gen v2-36, v2 gate green (115 files / 4070 tests).  #63 dynamic goto
SHIPPED: array.t de-gated at BETTER-than-v1 (167+15+13skip/195 complete run,
strict-subset fail set) via the forward-goto catch-wrap (porter spec ir-spec
§6.4), the our-alias requalify pre-pass, and the p-set-array-length nil-hole
fix (runtime, both pipelines); PCL_V1 gate = known 7 + the 3 new v2-only #63
guards; remaining gates: chdir, lfs, postfixderef, state, signatures,
closure (see e1-remainder.md s295b+c update).**
**Previous state s295 (2026-07-18, Fable): census 104 v2-native / 7 gated,
cache gen v2-35, v2 gate green (115 files / 4064 tests).  M-F eval family
SHIPPED: eval.t de-gated at BETTER-than-v1 (126+34/163, strict-subset fail
set) and ref.t at exact parity, via the ALIAS rule — normative spec
ir-spec §9.1; remaining gates: chdir, lfs, array, postfixderef, state,
signatures, closure (see e1-remainder.md s295 update).**
**Previous state s293+s293b (2026-07-16, Fable): census 102 v2-native / 9
gated, cache gen v2-34, v2 gate green (115 files / 4058 tests).  E2.0
SHIPPED (task #57): the emitter-conversion scaffold (CLForm::to_flat +
ExprToCL form_handlers/gen_node_form + corpus-diff --show) and the first
3 emitters converted at byte parity on BOTH pipelines (gen_ternary,
gen_string_concat, gen_array_str_interp).  E2.1 STEP 1 SHIPPED (s293b,
task #68 in progress): gen_funcall's GENERIC path is form-producing
(word:is/ok frontier head re-housed); `%FUNCALL_FORM_DECLINES` in
ExprToCL.pm = the live remaining-branch worklist.  PCL_V1 gate = 7
pre-existing v2-only-test fails (identical at HEAD) — the v1-gate
criterion is failure-set-identical, see §E2.0 recipe.  **Cadence rule
(user, 2026-07-17, supersedes "alternate E1/E2" for Fable sessions):
hard parts first — E2 conversion steps are now mechanical (§E2.0 recipe)
and are OPUS 4.8 material; Fable sessions take the hard problems (E1
M-F eval family SHIPPED s295 — the ALIAS rule, ir-spec §9.1; remaining
hard problems: closure, state #56, dynamic goto #63, chdir #55,
method.t stop@157).**  Before this,
s291–s292 (Opus 4.8) de-gated scalar.t (M-B) and substr.t (M-E).**
**Previous state s290: census 100 v2-native / 11
gated, cache gen v2-32, Pl/t gate green.  M-B session 1 SHIPPED (task
#51): sort.t de-gated (202/2/1, same fails 170/177 as v1 — exact parity)
via per-declaration span instances + canon-exact container promotion +
the unreachable-goto pre-pass + per-section forward-decl exclusion
(`_seg_lex`) + the on-demand file-top p-declare-sub stub sweep.  START
NEXT SESSION at the STAGED interp-refusal drop in
`_rename_spanning_lexicals` (marked STAGED): removing it de-gates
scalar.t, which currently diverges (78+36/128 PARTIAL, new fail t64,
early stop after t126) vs the v1 baseline 81/35/12 — debug that first
(s290 session-log entry has the full trail).  ref.t re-triaged to the
M-F eval family (string eval names the lexical — eval-unsafe refusal is
correct until the s250 capture alist carries renamed cells).  Remaining
11 gates: eval/ref/scalar (span+eval family), chdir, closure, lfs,
substr, array (#63), postfixderef (#45), state/signatures (M-F).**
**Previous (s289): M-A SHIPPED (task #67): pack.t (5638/87) +
yadayada.t (21/15) de-gated via the interp rewrite in
`_rename_decl_within`, PLUS oversized-extent flattening ($RUN_NEST_MAX /
$RUN_FORM_MAX), PLUS `_premerge_include_prototypes`.**
**Previous (s288): task #60 void-wrap hoist DONE — sub-body :void regime,
large-sub heap blowup gone; cl/pcl-pack.lisp regenerated by v2.  Task
#64 open: bare-block sub tail loses its value (both pipelines).**
E1-a is DONE (s285: element foreach-alias; s286b: counting-loop range
foreach perf; s287: bare-block continue, standalone label, list self-ref +
chained my, container capture de-conflation — §"E1-a" and §"E1-b" below
carry per-item DONE marks).  This doc is the ordered, session-sized
worklist for finishing v2; it packages — it does not replace — the
governing docs below.  When this doc and a governing doc disagree, the
governing doc wins; update this one.

## 0. Required reading, in order (do not skip)

1. `docs/v2-endgame-plan.md` — THE PLAN (phases E1–E5, simplicity contract
   §1.2 = the five acceptance invariants, calibrated estimates, decisions
   D1–D3 all resolved).
2. `docs/e1-remainder.md` — the living E1 worklist (per-file triage of the
   remaining gates by clearing mechanism; **the newest header note (s289)
   is the current state** — the per-file table rows lower down can lag the
   header notes, and several s283 hypotheses were later disproven; read the
   header notes top-down first).
3. `docs/session-log.md` §283–§284 + §289 — the mechanisms you will be
   extending (`_promote_captured` shadow-aware/positional model, identity
   promotion, `_hard_decl_count`, container interp rewrite, embedded-my
   let-hoist, CAPREFUSE/SPANREFUSE diagnostics; s289 added
   `_interp_fixer`/`_fix_interp_token`, oversized-extent flattening with
   `$RUN_NEST_MAX`/`$RUN_FORM_MAX`, and `_premerge_include_prototypes`).
4. `docs/v2-transfer-plan.md` — mechanism detail for E2 (T-C(ii)), E3 (T-B),
   E4 (T-D).  Its sequencing table is superseded; its mechanics are not.
5. `docs/ir-spec.md` — normative semantics of the generated CL.  **Update it
   on any semantic emission change.**
6. `docs/parser2-prototype.md`, `docs/v2-completion-plan.md` — architecture
   background (skim; deep-dive only when touching the relevant pass).

## 1. Non-negotiable standing rules

These are the reason the burn-down has produced zero regressions.  Every
session, every item:

1. **Verification triple** for any emission-affecting change:
   - **Corpus byte-diff vs HEAD**: run **`tools/corpus-diff.pl`** (built
     s287 — handles the worktree, the dual-root normalization, and the
     perl-not-grep marker strip; exit 1 when files differ).  Acceptance
     for a de-gate session: ONLY the de-gated files appear, each explained.
     Do not re-derive this by hand — the hand-rolled version got the
     normalization wrong twice.
   - **`--jobs 1` sweep parity** (`perl sweep-perl-tests.pl --jobs 1 <files>`)
     of every changed/de-gated file, HEAD vs new.  *A de-gate that loses even
     one test vs the v1 fallback is a regression, not a win* (bop.t, s283).
     Compare against `.faillog/` / `docs/fail-baseline.tsv`: a de-gated file
     matching v1's OLD failures exactly (same test numbers) is parity.
   - **Full gate**: `tools/prove-core` (== `prove -j8 Pl/t/`, ~2:30).
2. **Bump `*pcl-cache-generation*`** (`cl/pcl-runtime.lisp`) on every
   emission-changing commit — stale module caches otherwise reuse old
   transpiles.  (Don't trust a "Current:" snapshot here — read the
   defparameter and bump from whatever it says.)
3. **Gate, don't half-implement.**  If a construct can't be lowered fully
   correctly, `die "Parser2 TODO: …"` with a precise reason.  No silent
   miscompiles by construction.
4. **Copy v1's emitted shapes exactly** when porting a lowering; v1 is the
   parity oracle until E4 and must stay green (run the v1 gate when touching
   shared code: PExpr/Environment/runtime).
5. **One commit per item**, to `main`, each session's finished work
   committed.  Update `docs/session-log.md` (new section at top),
   `docs/e1-remainder.md` (in place), and the census/STATE line in memory.
6. **Never simplify or comment out a failing test.**  Not-supported →
   `cl/skip-registry.lisp` after user discussion, never edit
   `perl-tests/*.t`.
7. **HARD REQUIREMENT (user): dynamic string eval must keep working** at
   every intermediate state.  Until E3+E4, that means the per-eval v1 retry
   stays; a v2 gate inside an eval body must still produce a working eval
   via fallback.
8. **Hidden second gates are normal**: a de-gated file often reveals the
   next gate (s284: 4 of 7 cleared captures re-gated on new causes).  The
   census after every session is the tracker — re-run it, don't trust this
   doc's snapshot.
9. Diagnostics before mechanism: `PCL_SPAN_DEBUG=1` prints `SPANHIT` /
   `SPANREFUSE` / `CAPREFUSE` lines.  Verify a hypothesis from a refusal
   trace before building anything (s284 disproved two survey hypotheses
   this way).

Guardrails added from s286–s287 experience:

10. **Perl trap — `grep` mid-`||`-chain**: in
    `die … if A || grep {…} @x || B;` the grep's LIST is `@x || B`
    (evaluates to the array's *count*), so the grep silently tests the
    wrong thing.  s287 lost an hour to a spurious gate from exactly this
    (`/\b1\b/` matching digit-y RHS text).  Parenthesize every grep that
    is not the last term: `(grep {…} @x)`.
11. **Corpus byte-diff normalization**: the HEAD worktree embeds ITS OWN
    root in emitted `@INC`/path forms.  Map BOTH roots to one placeholder
    (`s{$worktree}{ROOT}g; s{/home/bernt/pcl}{ROOT}g`) or every file
    "differs".  Strip the pipeline marker with perl, not grep (NULs).
    A correct diff for an E1 session shows ONLY the de-gated files.
12. **Stale gate-guards**: `Pl/t/parser2-0*.t` contains guards of the form
    "construct X still dies to v1".  When you implement X, that guard
    FAILS — flip it to assert the new lowering (keep the assertion
    strength; never delete or weaken).  Budget for it: grep parser2 tests
    for the gate string you are removing before running the gate.
13. **Dump PPI before trusting statement boundaries.**  PPI splits an
    unlabeled bare-block `continue {}` into an orphan sibling statement
    and gloms the FOLLOWING statement's tokens into it (no `;` ends it);
    the labeled form keeps it in-compound.  This class of quirk (split /
    glom / mis-typed statements) is why a construct can pass a minimal
    probe yet gate — or silently miscompile — in the real file.  Bisect
    the real file to the exact statement (perl prefix-bisect with
    `PCL_V2_VERBOSE=1`) before concluding anything.
14. **"Falls back to v1" ≠ "v1 handles it."**  Verify what v1 actually
    does with a gated shape: array.t's `goto` out of a `map` lambda
    CRASHES v1 (compile error on the emitted `(go …)`).  Byte-identical
    fallback is still parity — but log such shapes as crash-fix items
    (task #63), and never assume the fallback is semantically complete.
15. **Raw-slot soundness comes in two regimes — don't mix them.**  The
    shipped `p-foreach-range-raw` loop var needs NO overload/dualvar
    analysis: range elements are fresh plain scalars by construction,
    writes must be arith-shaped, and any `eval` in the region vetoes raw
    (see `docs/ir-spec.md` §6.2).  The PLANNED raw-numeric/raw-string
    verdict (task #62) is the opposite regime: values from arbitrary
    expressions, so it requires the no-overload corpus scan AND the
    strict write-site check that dies on an overloaded ref / genuine
    dualvar (`docs/raw-numeric-verdict.md` §"Scope boundary" +
    §"Checked coercion").  If the strict check ever fires, fix the
    classifier or re-box the variable — never weaken the check.

Estimate calibration note: all session counts below were calibrated on
Fable throughput (~2.5 de-gates/session incl. mechanism work).  Treat them
as effort ratios, not promises; the census is the real tracker.

## 1b. The per-session checklist (mechanical — follow it in order)

Every E1/E2 session runs this loop.  Steps 1–3 cost minutes and prevent
the two classic wasted sessions (building on a stale survey; "fixing" a
pre-existing failure).

1. **Re-census**: `perl tools/v2-census.pl` → current native/gated + gate
   reasons.  Pick the target from `docs/e1-remainder.md` reconciled
   against this output (the census wins).
2. **Reproduce the gate**: `PCL_V2_VERBOSE=1 ./pl2cl < perl-tests/X.t`
   and, for capture/span gates, `PCL_SPAN_DEBUG=1` refusal traces.  If the
   gate fires somewhere unexpected, prefix-bisect the file to the exact
   statement; dump the PPI tree of that statement before theorizing.
3. **Record the baseline**: what does the file do TODAY (sweep counts,
   `.faillog/` rows, stop-points)?  What does V1 emit for the shape
   (`PCL_V1=1 ./pl2cl`)?  Copy v1's emitted shape — do not invent one.
4. **Implement**, gating everything you cannot lower fully correctly
   (`die "Parser2 TODO: <precise reason>"`).  Minimal probes first
   (perl vs `./runpl` on scratch files), then the real file.
5. **Verification triple** (§1 rule 1): `tools/corpus-diff.pl`, sweeps of
   changed files, `tools/prove-core`.
6. **Regression guards**: emission shapes → `Pl/t/parser2-01.t`
   (grep it FIRST for stale "still dies to v1" guards of the gate you
   removed — flip them); perl-vs-CL behavior → a bundled battery in the
   smallest `transpile-test-NN.t` (never -01).
7. **Bump `*pcl-cache-generation*`** if emission changed.
8. **Commit** (one commit per item, to main), then update:
   `docs/session-log.md` (new § at top), `docs/e1-remainder.md` (header
   note + per-file rows), memory STATE line, this doc's state header.
   New found-but-not-fixed problems become tasks, not TODO comments.

## 2. Phase worklist

Default cadence per the endgame plan: **alternate E1 and E2 sessions** so
gate wins keep landing while the structural work proceeds.  s289 did E1-d
session 1 (M-A, task #67); s290 did E1-d session 2 (M-B core — sort.t
de-gated, task #51 still in_progress for scalar.t).  As of s290 the
recommended next session is **M-B session 3: the scalar.t divergence**
(debug 78+36/128-vs-81/35/12, then drop the STAGED interp refusal in
`_rename_spanning_lexicals` — see the s290 session-log entry) or **E2.0
dual-run scaffold** (task #57 — cadence says an E2 session is overdue) or
**E1-a finish: the magic-lvalue foreach** (substr.t's last gate);
E1-a/E1-b are done except chdir.t.

### E1 — remaining 11 gates (~2–4 sessions)

#### E1-a. M-E: foreach over an aliasable lvalue element — **DONE (s285 + s287)**

s285 shipped the element foreach-alias (chop/aassign/sub de-gated;
substr.t re-gated on the narrower magic-lvalue gate + the E2 void-wrap
heap issue).  s287 shipped the rest of this batch:
- ~~loopctl.t~~ **DONE** — the actual gate was the BARE-block
  `LABEL: { } continue { }` (while/until `:continue` already worked);
  67/67 fully passing.  The unlabeled form was a **silent v2 miscompile**
  (continue block dropped) — fixed by the orphan-sibling join.
- ~~array.t list self-ref~~ **DONE** (`my (undef,@bee) = @bee` per-var
  copy-binding dance + chained `my @a = my @a = …`), but the file
  **re-gated on `forward goto to a standalone label`** — see task #63
  below; array.t is NOT expected to de-gate in E1.

#### E1-b. M-E singles, second batch (part done s287)

- ~~my.t~~ **DONE (s287)** — standalone label lowered to
  `(tagbody :label <block-remainder>)`; 49/1 = exact v1 parity (t46 is a
  pre-existing failure in both pipelines — do not chase it as a
  regression).
- **chdir.t** — `BEGIN block with sub-existence introspection`: read the
  refusal trace first; likely needs the BEGIN-time visibility the eval-when
  wrapping already provides — verify what exactly is introspected before
  building.
- **Task #63 (new, s287): dynamic `goto LABEL`** — `map { …; goto X } @a;
  X:` needs a throw-based unwind (a lexical `(go)` cannot escape the
  lambda).  **v1 CRASHES on this shape today** (compile error), so this is
  a crash-fix for BOTH pipelines, not merely a de-gate; array.t also stops
  at t114 at HEAD for an unrelated reason.  Design sketch in the task.
  Do NOT try to clear array.t by widening the s287 tagbody lowering — the
  forward-goto gate is what keeps the file on byte-identical v1 fallback.

#### E1-c. E1.4 postderef_qq — task #45, decision D2 = implement (1 session, clears 1 file, fixes BOTH pipelines)

Extend `Pl/PExpr/StringInterpolation.pm`'s arrow handling to
`->$*` / `->@*` / `->%*` / `->$#*` / slice forms inside interpolated
strings, conditioned on the `postderef_qq` feature being enabled (it is in
the `:5.24` bundle — any `use v5.24;` module has it silently).  Remove the
`_check_interp_postderef` stopgap gate.  De-gates postfixderef.t; v1 gains
the same fix.

#### E1-d. M-A interp-rewrite generalization + M-B per-declaration spans (2 sessions, clears 5 files) — session 1 DONE (s289)

Session 1 — **M-A — DONE (s289, task #67)** (cleared pack.t, yadayada.t;
prerequisite for ref/scalar; shipped as `_interp_fixer`/`_fix_interp_token`
wired into `_rename_decl_within`, plus oversized-extent flattening and
`_premerge_include_prototypes` — see session log §289).  Original spec:
generalize `_rewrite_var_uses`'s interpolation rewriter (already sigil- and
shadow-aware after s284) so the OTHER rename passes consume it: the
condition-my rename (pack.t `foreach my $base (split '', …)` @224), the
shadow rename (yadayada.t `$err` fallback block).  Must handle
`QuoteLike::Readline` tokens (`<$fh>`) for scalar.t.  Never rewrite interp
text inside a scope that re-declares the name.

Session 2 — **M-B — CORE DONE (s290, task #51)**: per-declaration
instances shipped (innermost-first, re-scanned facts, any-instance-spans
→ rename-all); sort.t de-gated at exact parity.  Also shipped en route:
canon-exact container promotion, `_rewrite_unreachable_gotos`,
per-section forward-decl exclusion (`_seg_lex`), on-demand file-top
p-declare-sub stub sweep — s290 session-log entry has the detail.
Session 3 — **remaining M-B target = scalar.t only**: debug its
divergence (78+36/128 PARTIAL, new fail t64, early stop after t126, vs
v1 81/35/12), then drop the STAGED "interpolated use" refusal in
`_rename_spanning_lexicals` (the fixer wiring is already in place).
ref.t moved to the M-F eval family (its `$x` is named in a string eval —
the eval-unsafe refusal stays correct until the s250 capture alist
carries orig-name → renamed-cell pairs).

#### E1-e. M-F residue — DECIDE WITH THE USER, don't grind (0–2 sessions)

Present these to the user before implementing; the recommendations:

| file | gate | recommendation |
|---|---|---|
| state.t | `state` in anon-sub / map-grep-sort block | **implement** (~1 session): per-closure state cell allocation in the lowering.  state is common in modern Perl. |
| signatures.t | `state` inside a signature default | bless as residue unless it falls out of state.t work |
| eval.t, closure.t | my-lexical spans + **dynamic `eval $code` after decl** | own task = #69 (s293c–s295, in progress): renamed cells become eval-visible by original name via (a) the site alist carrying let-bound + cross-package span pairs and (b) the **alias rule** — `(p-alias-eval-cell '$x $x__file__N)` at the decl's run position writes the cell into the original-name global, restoring v1's stop-2 lookup visibility with ONE storage location (no side registry — the s294 registry design was replaced after its structural stale-shadow regression).  Normative spec: ir-spec §9.1.  Satisfies the dynamic-eval HARD REQUIREMENT natively (dark strings need no compile-time inspection).  closure.t still carries the known per-iteration closure-binding limitation (CLAUDE.md TODO) — de-gating may leave those tests failing in both pipelines. |
| lfs.t | file lexical in END + heredoc interp | **bless as residue** (file is in the sweep's known-hang skip list — a de-gate is unverifiable end-to-end) |

E1 acceptance: census ≈ 106–108 native + small user-blessed residue
(= plan's "111 minus blessed-permanent").

### E2 — seam re-housing: ExprToCL emitters → CLForm (8–12 sessions, the structural core)

This phase delivers invariants 1–2 of the simplicity contract (no text CL
between parse and print; one printer).  Mechanism = the W12 dual-run
playbook (see `docs/v2-transfer-plan.md` T-C(ii)):

- **E2.0 (1 session): dual-run scaffold — DONE (s293, task #57).**  The
  "old" side of every step's dual run is git HEAD via `tools/corpus-diff.pl`
  (no in-tree duplicate emitters, no in-process double-run of side-effectful
  emitters — the s287 tool already runs both compilers side by side).  What
  shipped:
  - `Pl::CLForm::to_flat($form)` — EXACT flat rendering (one line, single
    spaces, raw atoms verbatim, dies on raw_wrap): the boundary every
    converted emitter is byte-parity-checked through.
  - `Pl::ExprToCL`: `form_handlers` table beside `handlers` (same node-type
    keys, same `($self,$node,$node_id,$kids)` signature).  A form handler
    WINS for its type but may DECLINE a not-yet-converted shape by
    returning undef → the text emitter runs as before (so big emitters
    convert branch by branch).  **Convention: decline BEFORE any side
    effect** (gensym counters, _emit, environment mutation) — the text path
    re-runs the node.  `gen_internal_node` = form dispatch + flat-print;
    `gen_internal_node_text` = the pre-E2 dispatch; `gen_node_form` = what
    converted emitters call on their children (form when converted, else
    the child's v1 text as an opaque raw atom — bytes preserved verbatim).
  - `tools/corpus-diff.pl --show[=N|all]` — prints the normalized diff
    hunks per changed file (localizes a parity break to the exact
    expression without hand-diffing temp trees).
  - First conversions at byte parity: `gen_ternary` (the pilot),
    `gen_string_concat` + `gen_array_str_interp` (frontier rank 4 —
    `node:string_concat`, 882 seam expressions).  Pattern each time:
    handlers entry moved to form_handlers, body returns a nested-array
    form with `gen_node_form` children; old text body deleted — HEAD
    keeps it.  Guards: `Pl/t/clform-01.t` (to_flat contract +
    converted-in-converted nesting + raw-child embedding + string_concat
    shapes; pure perl, no SBCL spawn).

  **Per-step conversion recipe (E2.1–E2.n):** (1) move the emitter's
  `handlers` entry to `form_handlers` (or add a declining form handler
  beside a kept text one for partial coverage); (2) rewrite the body to
  return a CLForm — children via `gen_node_form`, text fragments you can't
  structure yet via `Pl::CLForm::raw`; (3) `tools/corpus-diff.pl --show`
  must show ZERO files (byte parity — flat print == old text), and
  `PCL_V1=1 tools/corpus-diff.pl` likewise (ExprToCL is shared; v1 must
  stay the parity oracle); (4) `tools/prove-core` green + `PCL_V1=1
  tools/prove-core` failure set IDENTICAL to HEAD's — the v1-legitimate
  failures are the V2-ONLY FEATURE tests, and that set GROWS as features
  ship (s293's "known 7" grew to 14 by s303: state family, capture
  promotion, forward-goto-from-lambda, span renames, …).  Verify by
  failure-NAME comparison against a HEAD run, or accept when every failing
  name is a v2-only feature AND the PCL_V1 corpus byte-diff is zero.
  "100% green under PCL_V1" is a stale W9-era claim;
  (5) no cache-gen bump needed while parity holds
  (emission unchanged); (6) once a type's text handler is deleted its form
  handler must NEVER decline.  A parity break that v1's text can't express
  structurally (e.g. layout inside multi-line raws) is a finding to bring
  back to the plan, not to paper over.
- **E2.1–E2.n: convert the 69 emitters in frontier order:**
  1. funcall family first — `word:*` is one generic emitter covering
     is/ok/cmp_ok/join/… (biggest coverage per step).  **Step 1 DONE
     (s293b): `gen_funcall_form` covers the GENERIC call path** (user
     subs + non-special builtins, prototype machinery `(p-scalar …)`/
     `(p-backslash …)`, print-family `$_` default, die/warn `:loc`,
     my/our identity, split/join wraps, `%WANTARRAY_SENSITIVE` +
     `_ctx_wrap_form` context binds) at byte parity on both pipelines.
     **`%FUNCALL_FORM_DECLINES` in ExprToCL.pm is the live remaining
     worklist** — each name stays on the kept text gen_funcall until its
     branch converts (shrink the list branch by branch, one verification
     cycle each).  **STATE (s304): E2.1 IS COMPLETE except
     `inline_lambda`.**  Converted s304 (all at byte parity, both
     pipelines): s/// + tr/// leaves (gen_substitution_form /
     gen_transliteration_form; /e + interp replacement bodies stay raw
     atoms inside the lambda form); Symbol/Magic compound leaves
     (gen_symbol_form — stash/typeglob/&sub-callers-args/compound
     SPECIAL_VARS as single-level forms, shared by both leaf paths);
     `\(LIST)` family (single-scalar / no-range multi-term with the
     text emitter's counter-bump mirrored / general list); `-bareword`
     unary-minus-of-call + `SUPER::` indirect heads; the `eval` funcall
     branch (block + string with the capture alist —
     `_eval_lexical_alist` now returns a CLForm) — **the
     %FUNCALL_FORM_DECLINES table is deleted**.  Remaining declines,
     all verified by s304 corpus census:
     - `inline_lambda` (LAST, per item 4) — its body_cl AND the fixed
       multiline `(lambda (…)\n…)` layouts need the structured
       block-lowering re-host; parse_block_to_cl_string (Parser.pm
       text buckets) is the seam;
     - empty-shape trailing-space quirks (7 sites: 4 empty slices,
       empty `()`, empty anon-sub body, empty hash-init, empty
       `eval {}`) — normalize at E2.final;
     - `\(RANGE, …)` range-mix multi-term (multiline let + gensym'd
       loop vars);
     - never-firing safety nets (non-Word call head, unknown leaf
       type, compound-atom guard) — zero corpus hits.
  2. sym/magic reads, `string_concat`, literals (`quote-double`,
     `number-hex`),
  3. `op:=`/`++`/`!` families, regex forms,
  4. **`inline_lambda` `body_cl` → CLForm subtrees LAST** — it is the
     hairiest (pre-generated CL strings for map/grep bodies; converting it
     also removes VarAnnotator's structural `seam` walk special case).
- **E2.final: delete the text printer path + `raw`/`raw_wrap`.**
  Acceptance is grep-able: zero `raw(`/`raw_wrap(` call sites; ExprToCL2.pm
  (242 lines) folds away; the library is renamed `Pl/EmitCL.pm`.

Byte-parity per converted step; commit per step or per coherent group.
E2 sessions interleave with E1 (different files, no conflicts).

### E3 — eval-mode on v2 (1–2 sessions, after E1 is mostly done) — **SHIPPED s304**

Parser2 entry point for runtime string-eval: single anonymous segment, no
preamble/section assembly, capture alist names pre-registered as let-bound,
`p-eval-thunk` lambda shape **bit-for-bit** per
`docs/eval-lexical-capture.md`.  Keep a per-eval v1 retry until E4 (eval
bodies are arbitrary user code).  Gate: `Pl/t/eval-capture-01.t` identical
under both pipelines + eval.t parity.

**Shipped (s304):** Parser2 `eval_mode`/`eval_pkg` + `_assemble_eval_mode`
(head/body split, v1's exact thunk wrapper; free vars = AST scope scan ∪
the text-scan candidates, `$a`/`$b` defvar'd + param when referenced);
pl2cl admits `eval_mode`/`eval_pkg` into v2 and `--server` routes through
`parse_with_fallback` (v2-first, per-eval v1 retry).  Retry gates: top-level
`package` statement in the eval string (multi-segment assembly); a
trailing `my`/`our` declaration (v2's empty-body let loses the statement
value); a lone bareword in an ARRAY subscript (out-of-frame `use constant`
— v1 emits the runtime call).  The fallback_parser carries
eval_mode/eval_pkg so v1's eval-mode error contract holds in the seam
(`&sub = 1` dies → the eval fails, the CMM lvalue-probe idiom).  Two bugs the switch exposed, both fixed at the right layer:
(1) the file-level forward-decl text scan matched names inside STRING
LITERALS (the embedded eval source!) and defvar'd them — proclaiming the
eval's lexicals special and collapsing its closures to dynamic reads
(eval.t #39); `_blank_string_innards` now blanks string/comment innards
(pipe symbols and `#\X` literals preserved) before both scans — this also
kills task #66's sprintf `%x` phantom (61 corpus files lose only phantom
defvars; gen bump v2-45).  (2) `p-eval-lex-lookup` now INSTALLS the
autovivified container as the global value so cross-eval-only globals
persist (ir-spec §9.1 stop 3).  (3) eval-minted state cells carry a
source-hash tag (`$s__state__e<md5:8>_0`) so they cannot collide with the
enclosing file's `__state__N` cells (state.t #148/149).  Gates:
eval-capture-01.t 32/32 both pipelines (+2 new regression scenarios),
eval.t 126+34 = baseline, state.t 157+0, changed-file sweep clean.

### E4 — fuzz + external corpora, then delete v1 (2–4 sessions; E4.1 is the one irreversible step)

- **E4.0 first (1–2 sessions): strengthen `tools/difftest-ops.pl`** while
  v1 still exists as the parity oracle (decision D3).  Extend axes to what
  the v1-parity oracle has been catching: promotion/rename shapes (shadows,
  spans, captures, interp), context (`ctx-*`), interpolation forms.  Fuzz
  hard; fix what falls out.

  **STATE (s304): SHIPPED.**  Five new axes (18–22): call shapes × sub
  definitions (the #80 &optional class), pragma visibility (strict on/off ×
  bareword shapes), shadows/captures/interp (rename engine), string-eval
  shapes (E3 coverage), and a two-file module mode (`add(desc, code,
  {Mod => code})` — Exporter direct/glob-aliased/default, constant subs
  under strict, cross-file calling convention).  1060 valid snippets.
  First run found ONE new root cause — glob-installed constant subs
  (`*_c = sub () {…}`) had an invisible `()` prototype, so `_cnum + 1`
  swallowed the operand and `=~ _cnst ?` strung the bareword — fixed by
  `_premerge_glob_const_prototypes` (third member of the premerge family);
  guards in transpile-test-04b.t.  Steady state: **1053/1060 match; all 7
  residual mismatches are known** (3× parked `**` float divergence, 1×
  documented `() = split` count, 2× bare `*alias = \&sub` + 1×
  import-into-caller glob-install = task #83, the stash-visibility class).
- **E4.0b (user requirement, 2026-07-14): don't rely on fuzzing alone —
  re-run the external test corpora on the v2 default before deletion:**
  1. **Perl's own `t/` subdirectories that are NOT in the sweep**
     (`tools/run-perl-suite.pl --dir <subdir>`, sbcl CWD = perl's t/ dir):
     re-run the surveyed dirs (base, cmd, comp, re, io, opbasic) and cover
     the never-surveyed ones — **t/mro and t/class first** (task #25),
     per `docs/perl-test-suite-survey.md` / `docs/perl-test-suite-coverage.md`.
  2. **CPAN module suites** under the v2 default: at minimum the tracked
     set (Try::Tiny, Sub::Uplevel, Role::Tiny, Data::Dumper, JSON::PP,
     Test::More self-tests — see `docs/cpan-module-blockers.md` and the
     memory CPAN-suite entries); compare pass counts against the recorded
     baselines (s276b), not against zero.
  Any divergence found here is an E4-blocking bug to fix while v1 still
  exists as the parity oracle — that is the whole point of running these
  *before* E4.1, not after.
- **E4.1 (1–2 sessions): deletion**, only when: census at target, eval-mode
  native, E4.0b corpora re-run clean (or divergences fixed/user-blessed),
  one full session cycle with `PCL_V1` unused.  Steps (T-D):
  gates → hard errors (remove `parse_with_fallback`; count must already be
  zero); delete `PCL_V1` from pl2cl + runners; delete pipeline component
  from `p-compute-cache-path` (bump generation); delete `Pl/Parser.pm`
  statement layer + ExprToCL text emission; purge pipeline-aware test
  branches, v1-dialect rows in `docs/ir-spec.md` §2b, CLAUDE.md pipeline
  paragraph, memory entries.  Re-baseline sweeps: the perl-diff sweep +
  fuzzer are now the only oracles.

### E5 — post-deletion simplification (2–4 sessions, the finish the user asked for)

- Fold ExprToCL2 into EmitCL; prune v1-only paths in PExpr/Environment.
- Finish invariant 3: retire remaining bespoke rename loops in the spanning
  pass in favor of the shared `_scan_lex_facts` + `_rewrite_var_uses` +
  `_block_captures_name` engine (each pass declares what it reads/writes).
- Rewrite `CODEGEN_DESIGN.md` against the final architecture; single-dialect
  `docs/ir-spec.md`; CLAUDE.md module table; target file inventory =
  endgame plan §1.1 (~22k → ~14k lines).

## 3. Known open items that are NOT gates (don't confuse them with E1)

- **method.t stops at test 157** (v2-native file; mid-file stop, not a
  gate).  Diagnose via `.faillog/_status.tsv` col6 protocol.
- **W15 perf menu** (`docs/v2-completion-plan.md` §W15; W15.8 string append
  is the one bench loss) — interleaves freely, orthogonal.
- **Sweep fully-passing regression hunt** (parked, see memory) and the
  fail-baseline re-bless TODO — separate tracks.

## 4. Session template (repeat until done)

1. Re-read `docs/e1-remainder.md` header + last session-log entry; re-run
   the census to confirm the snapshot.
2. Pick the next item from §2 (alternate E1/E2).
3. Refusal-trace or dual-run-diff the hypothesis before coding.
4. Implement; verification triple (§1.1); cache-gen bump if emission
   changed.
5. Add regression tests (smallest `transpile-test-NN.t`, never -01; guard
   shapes in `Pl/t/parser2-01/02.t`).
6. Commit (one per item), update session-log + e1-remainder + memory STATE
   line.
