# Review requests — session 434 (Opus 5, 2026-08-22)

**Session Q1 of `docs/plan-post-s433.md`: the two instruments, #467 + #462,
plus the missing-snapshot-row report (s433 §A.4).  NO product change** —
`Pl/`, `cl/` and `lib/` are untouched, the generation stays **v2-177**, and no
emission can move.  Everything here is measurement machinery, which is exactly
why it is its own session: Q2 (the flip) and Q3 (#456 half (b)) are both
measured through these two instruments, and both make things DIE.

Files: `tools/run-perl-suite.pl`, `tools/drop-census.pl`,
`docs/perl-suite-run.tsv` (re-blessed), `docs/perl-suite-expected-rows.tsv`
(re-blessed), `docs/parse-error-drop-census-s399.tsv` (+12 rows),
`docs/test-infrastructure.md`, `docs/DECIDED.md`, `docs/session-log.md`.

---

## §1  #467 — the companion runner loads with recovery

One line of behaviour: `tools/run-perl-suite.pl` spawns

    sbcl --core … --eval '(pcl::p-load-with-recovery "/tmp/…/op_method.t.lisp")'

instead of `--load`, which is what `sweep-perl-tests.pl` has always done.  The
`PCL_SHOW_SBCL` before/after diff is exactly that one option; the stack size,
core placement and banner flags (`tools/lib/PCLSbcl.pm`) are untouched.

**Before/after on six named files, same tree, same session** — the four s432
cases plus two controls that do not die mid-way:

| file | before | after | Δrows |
|---|---|---|---|
| op/method.t  | 44/7   | **124/27** | +100 |
| op/sort.t    | 142/9  | **181/23** | +53 |
| op/lexsub.t  | 6/6    | **76/63**  | +127 |
| op/gmagic.t  | 1/0    | **4/2**    | +5 |
| base/rs.t    | 26/15  | 26/15      | 0 |
| op/join.t    | 39/4   | 39/4       | 0 |

The before numbers reproduce the blessed snapshot row for row, so the "before"
column is the baseline itself and not a fresh guess.  Every moved file is one
that died mid-way; a file that does not die is unchanged, which is the
property `p-load-with-recovery` promises (identical form-for-form evaluation
when nothing aborts).

**A recovered form is COUNTED and PRINTED.**  The recovery leaves no
`Unhandled …` / `debugger invoked on a …` header, so the runner's two crash
rules go blind on exactly the files that need a signature.  New rule, right
before the status decision:

    op/method.t   undef-fn:main::pl-new; aborted-forms:3: The function common-lisp:nil is undefined.
    op/gmagic.t   aborted-forms:5: Can't call method init on an undefined value

It is deliberately BEFORE the status decision (`!$sig` is one of the OK
conjuncts), so a file that could not evaluate a top-level form can never read
as OK even if its remaining TAP happens to match perl's — the opposite
placement from the #367 orphan note, which is an observation about the RUN.

## §2  The re-bless of `docs/perl-suite-run.tsv`

One measured `--all --jobs 4` pass (523 files), then the #366 discipline on top:
the runner serially re-ran 40 movers itself, said so when its own cap dropped
22, and those 22 were re-run in a second `--jobs 1` pass — so every moved row
has two measurements and the disagreements were resolved the runner's way (the
serial one is the verdict).

| | files |
|---|---:|
| GAINED rows | **52** |
| LOST rows | **0** |
| status-only change | 2 (both PRE-EXISTING, cause measured) |
| signature-only change | 116 |
| **TOTAL C_ok** | **74803 → 77128 (+2325)** |

Every one of the 52 is a file that used to die mid-way: op/filetest.t 36 → 434
rows, op/tr.t 94 → 314, op/lexsub.t 12 → 139, uni/fold.t 0 → 1160, re/subst.t
0 → 272, op/method.t 51 → 151.  The 116 signature changes are `crash:…` →
`aborted-forms:N: <condition>` — the same event, now named by the new rule.

**The scare, and how it resolved.**  The parallel pass reported six #326
hang-set files down 300–400 rows and three more moved.  Re-run ALONE, twice:
re/regexp.t 795/110 against a blessed 793/112, io/pvbm.t 23/5 exactly,
re/pat_psycho.t 11/0 exactly, op/utf8cache.t back to DIFF 2/0.  All contention
— which is worth writing down, because those six rows are STOPWATCH READINGS
(the file never finishes; C_ok is how far it got in 90 s) and a count-only
reading of them will always look like a regression under load.

**Two movers and one signature loss are PRE-EXISTING, and I measured the
cause** rather than asserting it (the s421 rule).  A `4356e77` worktree running
the OLD `--load` runner gives re/overload.t TIMEOUT 3/0, re/regexp_noamp.t
TIMEOUT 796/109 and re/pat_psycho.t 11/0-with-no-signature — exactly what the
new runner reports.  Their blessed rows were simply older than the tree.  The
same A/B attributes uni/variables.t's flood (1248/319 → 1248/27934, DIFF →
TIMEOUT) **to this change**: the old runner still gives 1248/319.

**`docs/perl-suite-expected-rows.tsv` had to be re-blessed too, for six files
and no others.**  io/shm.t, mro/basic.t, mro/basic_utf8.t, mro/next_skip.t,
mro/next_skip_utf8.t and op/current_sub.t are registered XDIFF files that now
REACH rows they used to die before, so their #185 row multisets stopped
matching and the files fell to DIFF.  `--bless-rows` was run over exactly those
six (1952 → 1956 rows; io/shm.t 1 → 22, op/current_sub.t 1 → 15, next_skip
10 → 11 ×2, and mro/basic.t 54 → 33 + mro/basic_utf8.t 40 → 28, which SHRANK
because rows that used to diverge now agree with perl).  Re-measured after the
bless: all six back to XDIFF, twice, alone.

**Ask 2: is that bless the right call?**  It is what `--bless-rows` exists for
after a measurement change, and leaving it undone would park six registered
files at DIFF forever.  But it does mean ~35 newly-reachable diverging rows are
now excused by their file's existing reason without anyone having read them one
by one.  I think that is the right trade for an instruments session and the
wrong thing to do silently — hence this ask.  The five registered files that
were ALREADY DIFF before this session (comp/hints.t, mro/inconsistent_c3.t,
mro/next_ineval_utf8.t, mro/package_aliases.t, re/pat_re_eval.t) were
deliberately NOT blessed.

## §3  #462 — the census's module blind spot

`tools/drop-census.pl` grew from three populations to five, and a `.pm` is now
transpiled with `--module` (the emission the runtime caches, and therefore the
one that runs):

    drop-census: perl-tests   111 files    4 with drops     5 drops
    drop-census: perl-t       527 files   23 with drops    77 drops
    drop-census: lib           22 files    0 with drops     0 drops
    drop-census: cpan-tests    94 files    9 with drops    15 drops
    drop-census: board         28 files    3 with drops     5 drops   (--board only)
    drop-census: 39 files carry a PARSE ERROR drop, 102 drops total

The 27 pre-existing rows come back **byte-identical**, and the 12 new rows are
the s431 hand measurement number for number (9 cpan-tests modules / 15 drops,
3 board modules / 5).  The blessed census gained them with the s431 causes;
its TOTAL line and the row sum both read **39 files / 102 drops**.

Two decisions inside it worth a look:

1. **`--board` is opt-in and argument-free** (`--board-dir DIR` for a build
   root that is not `$PCL_CPAN_BUILD` / `$HOME/.cpan/build`).  The board lives
   outside the checkout, so a plain run cannot refresh those rows; a run
   without the flag says so in its last line, and the census header says it
   too.  The 14 dists are read from `docs/cpan-board14-s343.tsv` (the board's
   own definition) rather than written down again.
2. **The shipped `lib/` shims moved from program mode to `--module`, and the
   glob became recursive.**  `lib/*.pm` + `lib/*/*.pm` had never included
   `lib/File/Spec/Functions.pm` or `lib/Math/BigInt/Calc.pm`.  Both measure
   zero drops in either mode, so no blessed row moves — but the population had
   a two-level hole in it since s399.

## §4  The snapshot's own holes (s433 §A.4), and a second one

Every run now ends with a `SNAPSHOT:` line naming the files it measured that
have **no row** in `docs/perl-suite-run.tsv` — the s431 hole, which was found
by counting 523 against 528 by hand.

Measuring it turned up **the same hole from the other side**: the five rows
s431 spliced in are all `BEGIN`-`@INC` files, which the dir scan filters out,
so `--all` never runs them.  s431 gave them rows so a regression could read as
a mover; nothing refreshes those rows, so it still cannot.  A full-scan run
now also prints them:

    SNAPSHOT: 5 row(s) for files this --all scan does not run (need-harness, or gone from t/):
      never-refreshed  comp/line_debug.t
      never-refreshed  op/goto.t
      never-refreshed  op/lex.t
      never-refreshed  op/require_errors.t
      never-refreshed  run/dtrace.t

Only on `--all` (on a `--dir` run every other row is trivially uncovered).
Printed, never fatal — it is a fact about the BASELINE, not about the run.

**Ask 1: is naming them enough, or should the population change?**  The two
alternatives are (a) leave it as reported and re-measure by naming the file,
(b) narrow the need-harness filter so files that actually run (op/lex.t
produces 13/39 today) join the scan.  (b) changes what `--all` means and would
move the file count from 523 to 528, so I did not take it in an
instruments-only session.

## §5  What I did NOT do

* **No `tools/gate-set-scan.pl` change.**  It scans the two `.t` populations
  (perl-tests + perl's t/) only, so after this session it and the census no
  longer share a population set; its module blind spot is the same one.
  Nothing in Q1's bar reads it,
  and widening it is a change to a second instrument in a session whose whole
  point is that the instruments are trustworthy.  Filed as a note here rather
  than done silently.
* **No unit test for the new signature rule.**  The abort regex lives inline
  in `run_one` like the crash-subgroup rule beside it; factoring both out to
  `tools/lib/` for a `tools/t/` row is real work and belongs with a session
  that touches that code for its own reasons.
* **No product change of any kind**, so no gate-SET scan, no corpus-diff, no
  sweep (the WHAT-TO-RUN table's `docs/**` + runner rows).  The gate ran
  anyway (always, per change).

## §6  The bar, run

| leg | result |
|---|---|
| gate (`tools/prove-core`) | **160 files / 5705 rows**, identical to s433; the only failures are the 13 pclxs xs rows (xs-01 ×5, xs-02 ×4, xs-03 ×4 — `~/pclxs` is at abi 8, the pin says 6; user-deferred) |
| runner before/after, same tree | six named files; the four that die mid-way gain rows, the two that do not are unchanged to the row |
| `PCL_SHOW_SBCL=1` before/after | one option differs: `--load F` → `--eval (pcl::p-load-with-recovery "F")`.  Stack size, core placement and banner flags (`tools/lib/PCLSbcl.pm`) untouched |
| full `--all --jobs 4` + the #366 serial discipline | 52 gained / 0 lost / 2 pre-existing status movers / 116 signature changes; TOTAL C_ok +2325 |
| A/B vs a `4356e77` worktree | the three "pre-existing" claims measured, not asserted; uni/variables.t attributed TO this change |
| census tool | five populations printed separately; 27 pre-existing rows byte-identical; row total (39/102) equals the blessed TOTAL line |
| new report lines | both halves verified live: "0 of 523 … every file is covered" on the full pass, and the five never-refreshed rows named on a `--all` run |

No sweep, no corpus-diff, no gate-SET scan: nothing under `Pl/`, `cl/` or `lib/`
changed, so there is no emission to compare (the WHAT-TO-RUN table's `docs/**`
and runner rows).
