# v2 Remaining Work — Execution Plan for Opus 4.8

**Written:** 2026-07-12 (session 285, Fable), for **Opus 4.8 to execute**.
**State updated s287 (2026-07-12, Fable): census 97 v2-native / 14 gated,
cache gen v2-29, Pl/t gate green (114 files / 4034 tests), commit 8bb3792.**
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
   19 remaining gates by clearing mechanism; **the s284 header note is the
   current state** — several s283 hypotheses were disproven, read it).
3. `docs/session-log.md` §283–§284 — the mechanisms you will be extending
   (`_promote_captured` shadow-aware/positional model, identity promotion,
   `_hard_decl_count`, container interp rewrite, embedded-my let-hoist,
   CAPREFUSE/SPANREFUSE diagnostics).
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
gate wins keep landing while the structural work proceeds.  As of s287 the
recommended next session is **E1-d M-A** (interp rewrite → pack.t,
yadayada.t) or **E2.0 + the void-wrap hoist** (task #60 — unblocks
substr.t and every large file); E1-a/E1-b are done except chdir.t.

### E1 — remaining 14 gates (~3–5 sessions)

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

#### E1-d. M-A interp-rewrite generalization + M-B per-declaration spans (2 sessions, clears 5 files)

Session 1 — **M-A** (clears pack.t, yadayada.t; prerequisite for ref/scalar):
generalize `_rewrite_var_uses`'s interpolation rewriter (already sigil- and
shadow-aware after s284) so the OTHER rename passes consume it: the
condition-my rename (pack.t `foreach my $base (split '', …)` @224), the
shadow rename (yadayada.t `$err` fallback block).  Must handle
`QuoteLike::Readline` tokens (`<$fh>`) for scalar.t.  Never rewrite interp
text inside a scope that re-declares the name.

Session 2 — **M-B** (clears sort.t, then ref.t, scalar.t): the shadow
re-decl sits at the top of a *flattened blk segment* (block contains a
`package` statement), so the shadow itself spans segments.  The span model
keyed by bare name (`%live/%decl_seg/%spanning`) conflates outer and shadow
— re-key span candidates **per declaration instance** (decl statement →
its own extent; uses attributed to the innermost declaring instance), then
rename each instance independently.  Building blocks exist:
`_hard_decl_count` + shadow-skipping rewrite (s283).  Order: **sort.t
first** (no interp), then ref.t (`"$test"` @437 needs M-A), scalar.t
(readline + `open my $fh` shadows).

#### E1-e. M-F residue — DECIDE WITH THE USER, don't grind (0–2 sessions)

Present these to the user before implementing; the recommendations:

| file | gate | recommendation |
|---|---|---|
| state.t | `state` in anon-sub / map-grep-sort block | **implement** (~1 session): per-closure state cell allocation in the lowering.  state is common in modern Perl. |
| signatures.t | `state` inside a signature default | bless as residue unless it falls out of state.t work |
| eval.t, closure.t | my-lexical spans + **dynamic `eval $code` after decl** | scheduled own task (s282c decision): the s250 capture alist must carry original-name → renamed-cell pairs.  Interacts with the dynamic-eval HARD REQUIREMENT — v1 fallback keeps these working meanwhile.  closure.t also carries the known per-iteration closure-binding limitation (CLAUDE.md TODO) — de-gating may leave those tests failing in both pipelines. |
| lfs.t | file lexical in END + heredoc interp | **bless as residue** (file is in the sweep's known-hang skip list — a de-gate is unverifiable end-to-end) |

E1 acceptance: census ≈ 106–108 native + small user-blessed residue
(= plan's "111 minus blessed-permanent").

### E2 — seam re-housing: ExprToCL emitters → CLForm (8–12 sessions, the structural core)

This phase delivers invariants 1–2 of the simplicity contract (no text CL
between parse and print; one printer).  Mechanism = the W12 dual-run
playbook (see `docs/v2-transfer-plan.md` T-C(ii)):

- **E2.0 (1 session): dual-run scaffold.**  Every emitter conversion runs
  old-text vs new-form-printed side by side with a corpus-wide byte-diff
  per step.  Build this FIRST; it is what makes the rest mechanical.
- **E2.1–E2.n: convert the 69 emitters in frontier order:**
  1. funcall family first — `word:*` is one generic emitter covering
     is/ok/cmp_ok/join/… (biggest coverage per step),
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

### E3 — eval-mode on v2 (1–2 sessions, after E1 is mostly done)

Parser2 entry point for runtime string-eval: single anonymous segment, no
preamble/section assembly, capture alist names pre-registered as let-bound,
`p-eval-thunk` lambda shape **bit-for-bit** per
`docs/eval-lexical-capture.md`.  Keep a per-eval v1 retry until E4 (eval
bodies are arbitrary user code).  Gate: `Pl/t/eval-capture-01.t` identical
under both pipelines + eval.t parity.

### E4 — fuzz, then delete v1 (2–4 sessions; E4.1 is the one irreversible step)

- **E4.0 first (1–2 sessions): strengthen `tools/difftest-ops.pl`** while
  v1 still exists as the parity oracle (decision D3).  Extend axes to what
  the v1-parity oracle has been catching: promotion/rename shapes (shadows,
  spans, captures, interp), context (`ctx-*`), interpolation forms.  Fuzz
  hard; fix what falls out.
- **E4.1 (1–2 sessions): deletion**, only when: census at target, eval-mode
  native, one full session cycle with `PCL_V1` unused.  Steps (T-D):
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
