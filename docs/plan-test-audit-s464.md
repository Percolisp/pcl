# The ignored-tests audit — plan (DRAFT, s464; to be PRESENTED s465, after #964 lands)

**Why this exists.**  The USER asked (s464, 2026-09-02), after #964 was measured:
"how could such a big difference in semantics live for so long, despite all
the tests?" and "are there tests marked don't-run that haven't run for
months?"  This document is the answer's second half: an inventory of every
test row the two suites do NOT exercise or do not READ, measured on main
`a8b4043`, and a plan to go over them.  It is a DRAFT: the numbers are to be
re-measured on the merged #964 tree before the presentation, and the
decisions in §5 are the user's.

## 1. How #964 survived — three mechanisms, each with a name

The test for "a sub returns a copy" existed TWICE and was neutralised both
times.  Each mechanism below is a hole the plan closes.

| mechanism | the #964 instance | closes with |
|---|---|---|
| **an inline `ok(1, 'SKIP: …')` with a wrong diagnosis** | `perl-tests/sub.t`: the `[perl #91844]` row `isnt \sub { ()=\@_; shift }->($x), \$x` replaced on 2026-05-19 (`fddd67b`) by `ok(1, 'SKIP: @_ aliasing not supported …')` — the right row, the wrong cause | #965 (§4 phase 4): restore every inline SKIP, FIX or move to the skip-registry with a reason |
| **count-blessing** | the real suite's `op/sub.t` snapshot is `53 ok / 12 not ok`; the same row fails inside the 12 and nothing names it | I1 (§3): row-level blessing for the companion suite |
| **a vacuous guard** | `Pl/t/foreach-aliasing-01.t:103` used a sub-local `my`, the one shape where the copy is unobservable | the s377 four-conjunct rule, applied when a guard is WRITTEN: a guard row must FAIL on the tree without the fix (inverse verification) |

## 2. The inventory (main `a8b4043`, measured s464)

### 2a. perl's own t/ — the companion suite (`tools/run-perl-suite.pl`, 528 files)

| class | files | perl rows | status | since |
|---|---|---|---|---|
| QUARANTINED (never run): `op/list.t`, `op/pack.t` | 2 | unknown (never measured) | task #160 — 10 GB OOM | s320, 2026-08-02 |
| hang set, `--quick` never runs: `re/regexp.t` + 5 siblings, `re/overload.t`, `re/speed.t` | 8 | ~13,000 (6 × 2167 + 87 + 59) | task #326 — stall at TAP row ~906 | only in full `--all` runs; last full run found: s398, 2026-08-15 |
| registered allowance > 120 s, `--quick` never runs: `re/pat_advanced.t` (900 s, 936 passing), `comp/require.t` (450 s, 909 passing), `re/pat_psycho.t` (450 s) | 3 | ~2,600 (~1,850 pass) | slow, not hung | same |
| **TRANSPILE-FAIL — attempted every run, ZERO rows produced**: `op/coresubs.t` 1109, `op/lvref.t` 199, `op/svleak.t` 156, `op/for.t` 148, `op/goto.t` 132, `base/lex.t` 120, `run/runenv.t` 106, `re/reg_eval_scope.t` 48, `comp/our.t` 7, `op/taint.t` ? | 10 | ~2,030 | the file's FIRST error kills every row | unattributed as a class |
| TIMEOUT with partial rows: `uni/variables.t` (29,182 of 66,880) | 1 | 37,698 unreached | #477 quadratic pos + a code-point refusal | — |
| XDIFF (registered "measures perl internals" / not compared): incl. `re/uniprops01–10.t` with 0 PCL rows | 111 | ~325,000 (uniprops alone) | deliberate; reasons in the runner's registration | to be RE-READ once |
| DIFF — files that run and diverge; **blessed as COUNTS, no per-row list** | 273 | **53,619 failing rows** (27,934 of them `uni/variables.t`; ~25,700 elsewhere) | this is where #964's row sat | — |
| perl rows PCL never produces, all classes, excluding uniprops + uni/variables.t | — | ~48,000 (of which `uni/fold.t` 18,072, `uni/lower.t` 11,720, `re/reg_fold.t` 6,891, `uni/upper.t` 6,228 — the Unicode case-fold family) | files that abort early | — |

### 2b. `perl-tests/` — the sweep (`sweep-perl-tests.pl`, 108 files, extracted 2026-02-28)

| class | count | note |
|---|---|---|
| inline `ok(1, 'SKIP: …')` rows — the assertion REPLACED in the file | **132 rows in 11 files** (task #965) | forbidden by CLAUDE.md rule 5 since s317; every one predates it |
| blessed failing rows (`baselines/fail-baseline.tsv`) | **695** | columns are file / row / description / got / expected — **no CAUSE column**; the categorised-failures doc is a session-156 snapshot |
| PARTIAL files (abort mid-file; rows past the abort never run) | **14 files, ~400 rows**: tr.t 75, method.t 65, chop.t 52, caller.t 48, substr.t 46, ref.t 36, magic.t 19, multideref.t 13, eval.t 9, readline.t 8, postfixderef.t 7, length.t 11, bop.t 1, yadayada.t 1 | a crash is NEVER a skip (rule 5) — these are fix targets that have been PARTIAL for months |
| TRANSPILE_FAIL whole file | **state.t, 158 rows, since s415 (2026-08-20)** | one `given` block (the ruled given/when refusal) kills the file; it had 157 passing rows |
| files called OK whose PLAN is far larger than what PCL produces | lc.t **82 of 2,659**, pack.t 5,725 of 14,722, sprintf2.t 1,642 of 1,678, split.t 184 of 219, quotemeta.t 40 of 60, chdir.t 25 of 44, each.t 51 of 65 | the OK verdict checks "no previously-passing row lost", not "the plan was produced" |
| extraction STALENESS vs perl 5.40.3's t/ | 62 identical, **43 differ**, 6 have no counterpart | e.g. `while.t` is a 4-test file (33 lines) vs 223 upstream; `lex.t` 266 upstream-only lines, `warn.t` 141, `reset.t` 105, `index.t` 100, `die.t` 86 — some are OLD perl versions, some carry PCL edits; unseparated |

### 2c. What is fine

The skip-registry (58 registrations) runs every assertion and flags a stale
skip; the drop census (5 = baseline) is gated; the sweep's LOST bucket
catches a file aborting EARLIER than its baseline; the gate has 191 files.
None of those needed changing to find #964 — what was missing is READING.

## 3. Instruments first (so the audit leaves rows behind, not a one-off)

- **I1 — row-level blessing for the companion suite.**  A
  `baselines/perl-suite-fails.tsv` (file, row number, description, got /
  expected when the TAP carries them), diffed BY ROW by the runner: a new
  failing row is a NEW ROW, a fixed one a FIXED ROW, exactly the sweep's
  `sweep-diff.pl` buckets.  The count snapshot stays as the summary.  Runner
  change only (`tools/`, not product).  This is the instrument #964's row
  needed.
- **I2 — planned − produced as a COLUMN in both runners, blessed per file
  with a cause** (the drop-census shape: rows leave by edit).  lc.t's 2,577
  unproduced rows and `op/coresubs.t`'s 1,109 become visible numbers instead
  of an OK and a TRANSPILE line.
- **I3 — a `cause` column in `baselines/fail-baseline.tsv`** (task number or
  `not-supported.md` anchor).  Rows without one are the audit's queue; the
  runner prints the count of cause-less rows so it cannot silently grow.
- **I4 — the full `--all` companion at least once per ROUND** (today: "at
  most once per session, only when a row says so"), on a quiet box, and its
  NOT-RUN rows stamped with the session that last measured them.

## 4. Phases, ordered by rows-per-effort

| phase | what | rows behind it | method | size |
|---|---|---|---|---|
| **0** | I1 + I2 + I3 + I4 | (enables the rest) | runner work; verdicts compared file-by-file before/after (the runner row of the WHAT-TO-RUN-WHEN table) | 1 Opus session |
| **1** | the ZERO-row files: the 10 companion TRANSPILE-FAILs + `state.t` + the 2 quarantined | ~2,200 + 158 + unknown | per file: transpile alone, read the FIRST error, decide FIX / REGISTER / REFUSAL per the runbook.  `state.t`'s shape is the general one: a ruled refusal must be STATEMENT-level (a drop that dies at the site, the flip's shape), never file-level — 157 rows return.  `op/lvref.t` (refaliasing) and `op/coresubs.t` (`&CORE::name`) are likely feature absences: MEASURE how many rows sit behind each before deciding | 1–2 sessions |
| **2** | the ABORTING files: the sweep's 14 PARTIALs (~400 rows) and the companion's early-abort DIFF files, largest first (the Unicode case-fold family is a CLASS — decide it once, §5) | ~400 + ~48,000 (mostly one class) | find the abort point (last row + stderr), fix or file with the crash reason; never registered as a skip | 2 sessions for the sweep side; the companion side is sized by the §5 decision |
| **3** | the BLESSED FAILING ROWS, read for cause: 695 sweep rows (I3 gives each a cause) and the companion's ~25,700 non-monster rows (I1 lists them) | 695 + ~25,700 | cluster by description / got-expected TEXT (families are large: one cause typically owns tens of rows), attribute each family to a task, file the new ones.  Budgeted per session (e.g. 150 rows); ordered by rows-per-cause.  Expect a few more #964-class semantic differences — that is the point | 3–5 sessions, interleaved |
| **4** | #965: the 132 inline SKIPs restored (FIX or skip-registry with reason, never the inline form); then the stale `perl-tests/` files refreshed from 5.40.3 where the difference is UPSTREAM GROWTH (while.t, lex.t, warn.t, reset.t, index.t, die.t …), keeping the principle-9 edits (invalid-perl rows commented out) as a LISTED set | 132 + the missing upstream blocks | mechanical + a re-bless ROW BY ROW of the refreshed files | 1–2 sessions |
| **5** | the hang set + quarantine + XDIFF re-read: #326 (find the pattern at re_tests row ~906), #160 (op/list.t under MemoryMax), re/overload.t + re/speed.t, and one pass over the 111 XDIFF registrations confirming each reason still holds | ~13,000 + unknown | measurement first, per the "suspect X carries its discriminating measurement" rule | 1 session + whatever #326 turns out to be |

Ordering rationale: phase 0 is where every later phase WRITES; phase 1 has
the best rows-per-first-error ratio (one error per file hides a whole
file); phase 4a (#965) is cheap and mechanical; phase 3 is the long tail
and is where the next #964 is, so it is budgeted, not skipped.

## 5. Decisions for the USER (to be asked at the presentation)

1. **Budget and shape**: its own rounds, or interleaved with the s452 plan
   (one perf agent + 1–2 correctness agents per round) with the audit as the
   standing correctness slot?
2. **The Unicode case-fold family** (`uni/fold.t`, `uni/lower.t`,
   `uni/upper.t`, `re/reg_fold.t`, `re/uniprops*` — ~370,000 perl rows, one
   class): in scope for v0.2, or registered as a class with its own
   `not-supported.md` section and a later owner?
3. **`perl-tests/` refresh policy**: replace the 43 differing files wholesale
   from 5.40.3 (re-bless row by row), or add only the missing upstream blocks?
   Wholesale is simpler and makes the two suites agree; it moves many baseline
   rows at once.
4. **Reading budget for phase 3**: rows per session, and whether a family
   whose cause is a filed task may be blessed with that task number without
   probing every member.
5. **`state.t`-class rule**: ratify "a ruled refusal is STATEMENT-level; a
   whole-file loss to one statement is a bug" as standing (it is the
   announce→DIE flip's own shape, plan-post-s433).

## 6. Standing rules this plan adds (proposed)

- A blessed failing row carries a CAUSE; a cause-less row is queue, not
  baseline.
- A guard row is inverse-verified when written (fails on the base tree).
- An `ok(1, 'SKIP…')` in a test file is a rule-5 violation; the skip-registry
  is the only skip.
- "OK" means the plan was produced OR the shortfall is blessed with a cause.
