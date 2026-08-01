# v2 Endgame Plan — time budget and the simplicity contract

**Written:** 2026-07-10 (session 280), at census **80/111 native**, gate
114/3948 green, cache gen v2-20.  Supersedes the "Sequencing and effort"
table of `docs/v2-transfer-plan.md` (whose T-A/T-B/T-C/T-D framework this
plan inherits — read that for mechanism detail; this doc adds calibrated
estimates, the end-state definition, and the user decision points).

> **Current state lives elsewhere** — the numbers above are this plan's
> birth snapshot.  For the live census run `perl tools/v2-census.pl`; for
> the current worklist, per-session checklist, and guardrails see
> `docs/v2-opus48-execution-plan.md` (state header kept current) and
> `docs/e1-remainder.md`.

**The user's goal (2026-07-10):** v2 ends up a **simple and well-structured
end product** — not merely "v1 deleted", but a codebase a newcomer can read.
Simplicity is therefore a deliverable with its own acceptance criteria
(§1), not a cleanup afterthought.

**Estimating unit: the session** (~one focused working day).  All numbers
calibrated against observed throughput, not guesses (§4).

---

## 1. The end-state contract (what "simple and well structured" means)

### 1.1 File inventory at the end

| file | role | today | target |
|---|---|---|---|
| `Pl/PExpr.pm` (+ PExpr/*) | expression parsing → OpcodeTree | 5538 | stays; prune dead v1-only params |
| `Pl/Environment.pm` | constants/prototypes/package state | — | stays |
| `Pl/Parser2.pm` | THE statement compiler | 3357 | ≤ 3500 (consolidation offsets growth) |
| `Pl/EmitCL.pm` (new name) | op emitter library — CLForm producers (re-housed from ExprToCL.pm) | 3848 (text) | ~3000 (structured) |
| `Pl/CLForm.pm` + printer | the ONE form model + ONE printer | 125 | ~300 (absorbs all formatting) |
| `Pl/VarAnnotator.pm` | box/raw-slot analysis | 847 | stays |
| `Pl/ExprToCL2.pm` | today's thin native emitter | 242 | **deleted** (folds into EmitCL) |
| `Pl/Parser.pm` | v1 statement layer | 8076 | **deleted** |

Net: ~22k lines → ~14k, one dialect, one printer, zero text islands.
(s316t reachability note: `Pl/Parser.pm`'s deletion is EARNED via the E5.3
burn-down, not flipped at E4.1 — see the re-scoped E4.1/E5 below and
`docs/v2-code-review.md` §4.  The target picture this table sketches is
drawn in full, for new contributors, in `docs/v2-target-architecture.md`.)

### 1.2 Structural invariants (each is an acceptance test, not a wish)

1. **No text-form CL between parse and print.**  Every emitter returns
   CLForm nodes; `raw`/`raw_wrap` are deleted (map/grep `body_cl`
   pre-generated strings become CLForm subtrees).  Grep-able: zero
   `raw(`/`raw_wrap(` call sites.
2. **One printer.**  All indentation/line-breaking in CLForm's printer;
   no emitter does its own formatting.
3. **One fact scanner, one rename engine.**  Every lexical promotion
   (capture / spanning / state / cond-my / shadow) consumes
   `_scan_lex_facts` facts and rewrites via `_rewrite_var_uses` +
   `_block_captures_name`.  No pass-order coupling: each pass declares
   what it reads/writes; a new promotion is a new *policy*, never a new
   *mechanism*.  (s279–280 did ~70% of this; E5 finishes it.)
4. **Gates die hard.**  After deletion there is no fallback: every
   remaining `Parser2 TODO:` die is a hard error backed by a
   `docs/not-supported.md` entry the user has blessed.  No silent
   miscompiles by construction: anything unlowerable *dies*.
5. **Declared oracles.**  With v1 gone the parity oracle is gone; the
   perl-diff sweep + `tools/difftest-ops.pl` are the only correctness
   oracles and must be strengthened *before* deletion (E4.0), not after.

---

## 2. Phases, with calibrated estimates

### E1 — Whole-file gate burn-down (31 → ~5, the blessed residue)

> **Living worklist: `docs/e1-remainder.md`** (s283, census 89/22) — per-file
> triage of the last 22 gates grouped by clearing mechanism (M-A interp
> rewrite, M-B per-declaration span tracking, M-C/M-D shadow-aware capture
> promotion, M-E singles, M-F residue candidates), with a recommended order.
> The table below is the original s280 estimate, kept for calibration.

Current gate families (census 2026-07-10) and their fixes:

| item | files | mechanism | sessions |
|---|---|---|---|
| E1.1 container spanning (#39) + method.t cascade (#40) | 7 spanning | extend W10 rename to @/% (parked patch exists); indirect-method syntax | 2–3 |
| E1.2 capture refusals (closure, my, hash, sub, chdir, hashassign, bop, undef, aassign) | 9 | per-file triage; several likely clearable by s280's interp rewrite + per-extent facts | 2–3 |
| E1.3 singles: continue-block, foreach-aliasable-element ×2, state ×2, self-ref list init, END-block lexical, yadayada/pack interp-shadow | 8 | one commit each; interp-shadow ones may fall out of E1.2 | 3–4 |
| E1.4 postfixderef cascade (#45 items 2–3) | 1 | sub-def ordering + `postderef_qq` in StringInterpolation.pm (fixes BOTH pipelines) | 1–2 |
| E1.5 nested `package` statements (bless, index, local, magic, reset — **and real Carp / Sub::Uplevel / DBIx::Class::_Util**) | 5+ | design (b): Environment-tracked package + qualified emission — see **D1** (§3, evidence revised 2026-07-11: recommendation is now IMPLEMENT) | 2–3 |

**E1 subtotal: 10–15 sessions** (E1.5 now included per revised D1).
Acceptance: census 111 minus blessed-permanent; every de-gate at
byte-diff + `--jobs 1` parity; hidden-second-gate rule applies (each
de-gate can reveal the next gate — the census after every session is the
tracker, and these estimates already include that from observed burn-down).

### E2 — Seam re-housing (T-C(ii)) — the structural core

The census is unambiguous: ~90% of expressions lower through v1's text
emitters, and even *literals* (`quote-double`, `number-hex`) sit in the
blame frontier — per-op porting can never close this.  The plan is the
mechanical wide refactor: **ExprToCL.pm's 69 emitters return CLForm nodes
instead of strings**, becoming v2's op library (`Pl/EmitCL.pm`).

Method (the W12 playbook, which took 4 sessions for a comparable swap):
- E2.0 dual-run scaffold: every emitter conversion runs old-text vs
  new-form-printed side by side; corpus-wide byte-diff per step (1 session)
- E2.1–E2.n convert in frontier order (funcall family first — `word:*` is
  one generic emitter and covers is/ok/cmp_ok/join/…; then sym/magic
  reads, string_concat, literals, op:=/++/!, regex forms,
  `inline_lambda` body_cl → subtrees LAST, it is the hairiest)
- E2.final: delete the text printer path + `raw`/`raw_wrap`

**E2 subtotal: 8–12 sessions.**  This phase IS the simplicity payoff:
invariants 1–2 land here.  Byte-parity per step means it is also the
lowest-risk phase despite its size.

### E3 — Eval-mode on v2 (T-B)

Parser2 entry point for runtime string-eval (single anonymous segment,
capture alist pre-registered as let-bound, `p-eval-thunk` shape
bit-for-bit).  Keeps a per-eval v1 retry until E4.
**1–2 sessions**, after E1 makes gates rare.

### E4 — Deletion (T-D)

- E4.0 **strengthen the fuzzer first** (oracle handover, invariant 5):
  extend `tools/difftest-ops.pl` axes to cover what the v1-parity oracle
  was catching (promotion/rename cases, context, interp) — **1–2 sessions**
- E4.0b **external corpora re-run (user requirement, 2026-07-14)** — not
  fuzzing alone: (1) perl's own `t/` subdirs that are NOT in the sweep
  (`tools/run-perl-suite.pl` — re-run the surveyed dirs, cover t/mro and
  t/class); (2) the CPAN module suites, compared against their recorded
  baselines.  Divergences are E4-blocking fixes while v1 is still the
  oracle.  Detail in `docs/v2-opus48-execution-plan.md` §E4.
- E4.1 gates → hard errors; delete `PCL_V1`, `parse_with_fallback`, the
  pipeline cache key, v1's file-level entry/assembly/forward-decl passes;
  purge dual-dialect docs/tests; re-baseline sweeps — **1–2 sessions**.
  **RE-SCOPED (s316t review, `docs/v2-code-review.md` §4): this step kills
  the dual-pipeline PROPERTY (~600–700 lines), not `Pl/Parser.pm`.**  The
  v1 statement layer stays live through the 12 intra-file `_fallback_stmt`
  seams, ExprToCL through the expression seam, and
  preprocessing/prototypes/naming are shared (ported, not deleted).
  Pre-requisite found by reachability scan: bundle mode calls
  `Pl::Parser->parse_file` directly (`pl2cl:283`) — port it first.
  The line-count payoff moves to E5 steps 2–4 below.

### E5 — Post-deletion simplification (the finish the user asked for)

**RESTRUCTURED (s316t review, `docs/v2-code-review.md` §8) into five
independently-shippable steps, each verified by corpus-diff + full sweep.
The target they converge on is `docs/v2-target-architecture.md`.**

- **E5.1 seam object** — one `SeamSession` guard owning bucket
  save/reset/drain/restore, `_v2_embed` arming, depth counters, and
  `_let_bound_vars` scoping (replaces 8 hand-written save/restore pairs +
  two copies of the bucket dance).  Biggest fragility payoff; do first —
  every later step runs through the seam it hardens.  **1–2 sessions**
- **E5.2 embedded-block totality** — `lower_embedded_block` stops
  declining; deletes the `parse_block_*` text family + the 4 remaining
  `body_cl`/`raw_lambda` raw sites (task #78's tail).  **1–2 sessions**
- **E5.3 `_fallback_stmt` burn-down** — retire the 12 intra-file classes
  (use/require/BEGIN-END, `local`, prototype/signature subs,
  goto/next/last/redo, multi-element `return`, loop statement-modifiers,
  anon-hash-as-block) one at a time; each retires its v1 `_process_*`
  handler.  This is where `Pl/Parser.pm` actually shrinks.
  **4–8 sessions** (12 classes at the calibrated 2–3 classes/session)
- **E5.4 one expression brain** — fold ExprToCL2 into EmitCL as
  early-return branches sharing ONE binop/ctx table (the #131 argbox
  double-implementation is the cost being deleted); then delete
  `gen_node` + the string emitters + `raw`/`raw_wrap` once the two
  structural decline paths can no longer fire.  **2–3 sessions**
- **E5.5 shared predicates + shape** — the statement splitter
  (`_split_at_lowprec`, one precedence table — started pre-R1 as task
  #138), the VarAnnotator↔Parser2 `native_root_write` shared predicate,
  one context-constant set, and the Parser2 phase file-split
  (prepass/facts/promotion/lowering/seam).  Finish invariant 3.  Rewrite
  `CODEGEN_DESIGN.md` against the final architecture; update
  `docs/ir-spec.md` to single-dialect; CLAUDE.md module table.
  **1–2 sessions**

**E5 subtotal: 9–17 sessions** (was 2–4 — the deletion work E4.1 was
credited with lives here; the ~22k→14k arrives in E5.2–E5.4).

---

## 3. User decisions needed (block estimates, not work)

- **D1 — nested `package` statements (pkg-in-block residue, 5 files).**
  **RESOLVED (session 281, 2026-07-11): implemented as design (b-lite)** —
  the user asked whether D1 could be simpler if the construct were allowed
  to be slower, and inspection of v1 showed the answer is *yes, and it is
  not even slower*: v1's own working mechanism for the statement form is
  just "push the shared Environment package + emit one
  `(p-set-current-package …)`" — the qualified emission that design (b)
  budgeted 2–3 sessions for already happens wherever the Environment drives
  emission (`our`, hoisted sub names, `use overload`, 1-arg bless).  v2's
  transplant lives in `_lower_block`: on a nested statement-form
  `package X;`, push the Environment package, lower the REMAINDER of the
  enclosing block under it (Perl's block scoping falls out of the
  recursion), pop, and emit enter/restore `p-set-current-package` forms;
  the block form lowers its own block the same way, plus v1's inline
  `(p-defpackage)` + qualified `(defclass)` trio, re-executed each run —
  the "accept slower" concession, costing one defclass re-eval per
  execution of a rare construct.  Unqualified *globals* after the switch
  keep the section package — exactly v1's documented divergence, no worse.
  Supporting pieces: `_sub_name_for_emission` (hoisted subs qualify against
  the Environment package when it differs from the segment), `_effective_pkg`
  (pre-pass sub registration honours nested switches; no direct-call
  sub_info for such subs), and every `Statement::Package` namespace in the
  document joins the `%pre` p-defpackage set (also fixes a real load bug:
  BEGIN-nested packages — the Moo M::G::Accessor idiom — emitted qualified
  symbols with no p-defpackage).  Actual cost: **1 session** including the
  exposed pre-existing bugs it un-gated (delete-local-in-my restore, `$^S`,
  raw_wrap closers swallowed by comment echoes).
  **Evidence update (2026-07-11, wider-corpus PPI scan — the first-gate
  census had HIDDEN these behind earlier gates):** the construct IS used
  by real CPAN code, in two narrow static idioms:
  - **`package DB;` inside a sub** around a `caller($i)` call (the
    @DB::args protocol): real Carp, Sub::Uplevel, **DBIx::Class::_Util
    (×2 — and that file's FIRST gate is exactly this statement)**.  Note
    PCL never populates @DB::args, but these callers all detect that and
    degrade gracefully — so *statement* support alone makes them compile
    and run their fallback path.
  - **PAUSE-hidden helper package in a BEGIN** (`package\n X::_Private;`):
    Moo's Method::Generate::Accessor, DBIx::Class::_ENV_.
  Framework *class creation* (Moo/Moose/DBIC result classes) never hits
  this: it is runtime string-eval / glob assignment, where `package X;`
  is top-level in its own compilation unit.
  **Revised recommendation: implement design (b)** — Environment-tracked
  current package + qualified emission for the scope remainder (+2–3
  sessions, folded into E1) — since unshimmed Carp-family and DBIC are
  squarely in PCL's CPAN ambition.  The 5 torture files come along for
  free.
- **D2 — `postderef_qq` interpolation**: **DECIDED (user, 2026-07-11):
  implement** (~1 session, in E1.4).  Extend StringInterpolation.pm's
  arrow handling to the `->$*`/`->@*`/`->%*`/`->$#*`/slice forms,
  conditioned on the feature being enabled (it is in the `:5.24` bundle,
  so any `use v5.24;` module has it silently); fixes both pipelines,
  removes the `_check_interp_postderef` stopgap gate.
- **D3 — fuzzer timing/depth**: **DECIDED (user, 2026-07-11): after
  E1–E3, before E4.1** — fuzz hard while v1 still exists as the parity
  oracle, then delete.  Depth to be sized when we get there.
- **D4 — W15 perf items** interleave freely; not counted here.

---

## 4. Total and calibration

| phase | sessions |
|---|---|
| E1 gates (incl. E1.5 nested-package per revised D1) | 10–15 |
| E2 seam re-housing | 8–12 |
| E3 eval-mode | 1–2 |
| E4 fuzzer + deletion | 2–4 |
| E5 simplification | 2–4 |
| **total** | **23–37** (~expected 29) |

Calibration sources: capture-family burn-down ran 66→80 native across
sessions 278b–280 ≈ **2.8 files/session including mechanism-building**
(E1 uses ~2.5); the W12 annotator swap — the same "mechanical wide swap
behind a dual-run diff" shape as E2 — took **4 sessions** for ~1/4 the
surface (E2 uses 8–12).  The pessimistic tail (~40) is dominated by
hidden second gates (E1) and `inline_lambda`/interp corners (E2).

Order: E1 and E2 can interleave (different files); E3 after E1 is mostly
done; E4 strictly last; E5 immediately after.  Suggested default:
alternate E1/E2 sessions so gate wins keep landing while the structural
work proceeds.

---

## 5. Standing rules (unchanged, they are why this is safe)

Census + worktree byte-diff (strip the `;;; pcl: pipeline=` marker line
after a gen bump) + `--jobs 1` parity sweep after every item; copy v1
shapes exactly when porting; gate-don't-half-implement; one commit per
item; bump `*pcl-cache-generation*` on emission changes; v1 stays green
until E4 (it is the oracle).

---

## 6. After E5 — the two product targets (P-phase)

**Set by the user 2026-07-20 (session 301).**  E1–E5 finish the *compiler*;
these two targets define what the *product* must be.  They are acceptance
criteria, not vague aspirations — each has a scoreboard.

### Target A — general program speed must beat Perl

**Acceptance:** `perl tools/bench-exec.pl` — the general-purpose rows beat
Perl (pcl/perl < 1.0×), and the suite geomean excluding the granted-slack
rows is < 1.0×.  **Granted slack** (a couple of areas may stay much
slower): the regex *engine* (cl-ppcre; only #71 PCRE2 could close it) and
the pack/unpack *oracle* rows (transpiled pure-Perl, tracks oracle
overhead, not codegen).  Slack means "not release-blocking" — it does NOT
remove the item from the worklist.  String concatenation also has slack
(§7: S1 may ship as a documented TODO at R2), but stays high on the
worklist regardless: O(n²) append is a complexity class that breaks real
string-building programs, not a constant factor.

**State (s301 bench):** recursion/calls already beat Perl (fib 0.24×,
gcdrec 0.42×); the losses are numeric accumulation (3.4–4.0×),
aggregates (2.0×), collatz (2.0×), strcat (2525×), pack (~1180×).

**Worklist — every item MEASURED in `docs/faster-codegen-suggestions.md`
(the authoritative catalogue: per-item variant timings, preconditions,
before/after codegen shapes in its §11, priority in §12):**

| # | item | measured | moves | task |
|---|---|---:|---|---|
| A-1 | **S1** raw-string append buffer (fill-pointer `.=`) | ~2400× | strcat 2525×→~1× | #62 |
| A-2 | **N1** raw-numeric verdict incl. `+=`/`-=`/`*=` as arith writes (+ numeric-string freeze) | ~13× / ~8.5× | intloop+=/=, cfor, collatz →~1× | #62 |
| A-3 | **M1** method-dispatch monomorphic inline cache | ~15× | all OO/Moo/Moose CPAN | #73 |
| A-4 | **P1** pack/sprintf constant-template memoization (`load-time-value` plan) | oracle / ~5× | pack rows | #74 |
| A-5 | Tier 2: A3 push→`vector-push-extend`, A4 raw array elems, A5 sort-comparator idioms, A1 single-lookup hash update | 3.5–7× | arrhash + list/sort code | later |
| A-6 | Tier 2: F1 `dynamic-extent @_`, F2 real lambda lists, F3 elide `*wantarray*` bind, N2 in-place box write | GC + 1.3× | call-heavy code | later |
| A-7 | Tier 3: X1 block-compile runtime, O2 const-fold, constant-regex `load-time-value`; #71 PCRE2 (also a *correctness* play) | 1.2–2× | baseline | later |

**Measured dead ends — do NOT spend sessions here** (catalogue §12):
native `+` for `p-+` (0% — R1 already at the sound ceiling), unboxing
hash values without removing a lookup, constant-key hashing, IO codegen
micro-tuning, fixnum-typing without a range proof.

**Sequencing:** A-1..A-4 are independent of E2–E5 (runtime + v2 codegen;
no seam contact) and may interleave with them at any time — S1+N1 share
the verdict analysis and pair as one block (task #62 first).  Every item
lands behind an analysis precondition with the boxed fallback and the
checked-coercion discipline (`raw-numeric-verdict.md`), and ends with a
`bench-exec.pl` re-run — the bench is the only scoreboard.

### Target B — the generated IR must be CLEAR, with obvious macros doing the CL-specific stuff

**Acceptance:** a Perl programmer can read the emitted file and see their
program; every CL-specific mechanism hides behind an obviously-named
macro from a **closed, documented vocabulary** (`ir-spec.md` is the
normative list).  Concretely: no raw text islands (E2.final/E5 retire the
seams), no host-idiom constructors or bare special-variable binds inline
— `(p-scalar-ctx …)` not `(let ((*wantarray* nil)) …)`, `p-new-av`/`p-vlist`
not `(make-array … :adjustable t …)`, `(p-esc …)` not raw control bytes,
structured regex literals not un-parsed Perl source.

**Worklist = `docs/generated-cl-ir-review.md` §4 items 2–6** (constructor
+ context macros; structured regex literals; `p-esc`; dedupe/asymmetry
cleanups; canonical re-print), plus the §4 item 7 seam retirement that
E2.final/E5 already deliver.  Each is emission-changing: full parity
discipline + cache-gen bump; none may change what the code *does*.

**No conflict with CLAUDE.md §2 (speed wins):** the macro layer is free at
runtime — a macro *expands to* the fast shape, so clarity is bought at
macroexpansion time, never with slower code.  Where Target A introduces a
new fast shape (raw slots, fill-pointer buffers, inline-cache cells), it
must arrive **wrapped in its named macro from day one** (`p-append!`,
`p-call-cached`, …) so the two targets converge instead of fighting:
the IR stays readable *because* the CL-specific machinery is named.

**Sequencing:** doc-side items anytime; emission-changing flag-days are
cheapest AFTER E4 (one pipeline, no dual-dialect parity) — schedule the
macro-vocabulary flag-day right after E5, folding in the macro names
Target A's items introduced along the way.

---

## 7. Release roadmap (user, 2026-07-20, s301)

Two releases, each gated by one of the §6 targets; perf items may slip
past R2 when documented.

**R1 — the correctness release.**  Ships when:
1. the rewrite is FINISHED — E2 through E5 complete (one pipeline, v1
   deleted, simplification done);
2. the remaining *internal* Perl tests are re-run green-or-explained —
   the sweep + Pl/t gate as usual, PLUS the perl `t/` subdirs not in the
   sweep (task #25 / E4.0b: t/mro and t/class have never been surveyed;
   re-run the previously surveyed dirs);
3. a selection of CPAN module suites re-run against their recorded
   baselines (task #25's second half; suite list + baselines in
   `project_cpan_test_suites` / s276b numbers).
Speed is NOT an R1 gate — R1 ships with today's profile (calls/recursion
faster than perl; numeric loops and string append slower).

**R2 — the speed release.**  Ships on Target A acceptance (§6): general
program speed beats Perl, slack rows excluded.  **Explicitly permitted to
ship with one or two documented perf TODOs** — the named candidate is the
**string-concat/append class (S1)**: if it is not done by R2, it ships as
a documented TODO (`docs/todo-features.md` §Perf) rather than blocking
the release.  The numeric-loop work (N1) is NOT slippable — the general
benches can't beat perl without it.

**Post-R2.**  The documented TODOs (S1 if slipped), remaining Tier-2/3
perf items, #71 PCRE2, Target B's macro flag-day if not already folded
into E5, and the XS bridge (`pclxs`) as its own track.
