# Failure-cause classes — "how many of the failures are from what we don't support?"

**Ruled s486a (Fable), measured 2026-09-16.  The rule lives in
`tools/lib/PCLCauses.pm` (`cause_class`) and verbatim in `docs/DECIDED.md`
§s486a; this file explains it and records the first measurement.**

PCL blesses every known-failing row with a CAUSE (task #993 I3) — a task
number, a `docs/not-supported.md` anchor, a parking note.  Until s486a those
causes had never been **summed by class**, so the project could say *how many*
rows fail but not *why* in the one shape a reader actually asks for: is this a
queue of bugs, or is it the deliberate edge of the language PCL implements?

    tools/cause-census.pl             # the table
    tools/cause-census.pl --markdown  # the table for docs/STATUS.md
    tools/cause-census.pl --hygiene   # every `other` row: population, key, cause

## THE RULE

A blessed row (failing, diverging, or never produced) falls into EXACTLY ONE
class, decided from its CAUSE text by ONE function, `PCLCauses::cause_class`:

1. **`not-supported`** — the cause names a `docs/not-supported.md` section: an
   `NS:` anchor ANYWHERE in the text, or the literal `not-supported.md` citation
   (the spelling `baselines/perl-suite-expected.tsv` uses).  A row that ALSO
   names a task is still not-supported: with every filed bug fixed PCL would
   still fail it; the task owns the residue, not the row.
2. **`parked`** — `PARKED:` — a USER scheduling decision (pack/unpack today).
   Reported beside not-supported, never folded into it.
3. **`bug`** — a task number (`#NNNN`) and nothing of the above: the queue.
4. **`unexplained`** — no cause, or `UNEXPLAINED…` (= `has_cause` false).
5. **`perl-skip`** — `PERL-SKIP` (the board): perl skips the file too, so it is
   not a PCL failure; shown, and EXCLUDED from the share's denominator.
6. **`other`** — has a cause matching none of the above (today: `DECIDED "PCL
   has no PVBM"` ×7, `PCL does not model use strict refs` ×2, the s473t1
   shortfall notes).  A HYGIENE list the tool prints; it is expected to trend to
   zero, because a DECIDED divergence must carry the `NS:` section it rests on.

**The ORDER of the tests IS the rule.**  not-supported wins over a task number
for the reason in class 1; `unexplained` is decided first, so a row with no
cause can never be mistaken for `other`.  `has_cause` stays the single
definition of "attributed", so the `unexplained` count in the census is the
same number the three runners have printed since #993.

## Where the classes are printed

`causes_line`'s third answer **is** `census_line`, so every runner that blesses
failing rows prints the split without holding a second reading of the column
(CLAUDE.md rule 11):

- `tools/sweep-diff.pl` — the perl-tests sweep
- `tools/run-perl-suite.pl` — the companion (perl's own `t/`), in the ROW DIFF
- `tools/cpan-scoreboard.pl --diff` — the CPAN board

so the question is answerable from any run, not only from `cause-census.pl`.

## The skip registry enters the DENOMINATOR

`cl/skip-registry.lisp` relabels a matched failing row as `ok # skip`
(`docs/test-skip-registry.md`).  Those rows are **not-supported FAILURES that
the sweep's headline fail count does not contain** — roughly 180 per run.  The
census's sweep denominator is therefore fails + registry-relabelled rows, read
from `.faillog/_status.tsv`'s registry column **when that column exists**, and
printed as

    registry: NOT COUNTED (no registry column in .../_status.tsv)

otherwise.  Never inferred, never zero by default: guessing zero understates
the not-supported share, which is precisely the number this file exists to
report.

## Weighting: KEY vs ROW

`baselines/fail-baseline.tsv` is a set of **keys** — (file, description) — and
one key can stand for several failing rows of a run (the sweep's Sep-6 log has
635 rows behind 488 blessed lines).  So:

- a run log under `.faillog/*.fails.tsv` present ⇒ **ROW-weighted**, joined on
  that key, and the tool prints the log's DATE, because that is the date of the
  measurement;
- no log ⇒ **KEY-weighted**, and the tool SAYS SO.

Both are legitimate; neither may be silently substituted for the other.  A run
row with no blessed row is **log/baseline drift** and is reported on its own
line, never folded into a class.

The other five populations are row baselines already, except the shortfall,
which is weighted by its **count column** (its unit is a row that was never
produced, not a file).

## The first measurement — 2026-09-16, main `4429126a`

`perl tools/cause-census.pl` (sweep row-weighted against the `.faillog` of
2026-09-06; 7 run rows have no baseline row):

| population | rows | not-supported | parked | bug | other | unexplained | perl-skip | not-supported + parked |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| perl-tests sweep | 635 | 268 | 69 | 291 | 7 | 0 | 0 | 53.1% |
| companion (perl's own t/) | 11,984 | 4,661 | 88 | 4,873 | 0 | 2,362 | 0 | 39.6% |
| CPAN board (14 dists) | 389 | 88 | 0 | 294 | 0 | 0 | 7 | 23.0% |
| companion XDIFF rows | 2,517 | 2,516 | 0 | 0 | 0 | 1 | 0 | 100.0% |
| shortfall: perl-tests | 12,213 | 267 | 0 | 104 | 0 | 11,842 | 0 | 2.2% |
| shortfall: perl's t/ | 444,439 | 55,393 | 417 | 388,107 | 0 | 522 | 0 | 12.6% |
| **all populations** | **472,177** | **63,193** | **574** | **393,669** | **7** | **14,727** | **7** | **13.5%** |

Registry: NOT COUNTED — the Sep-6 `_status.tsv` predates the column (s486b is
adding it).

**Read the per-population rows, not the total.**  The grand total is dominated
by one population — the `t/` shortfall's 444,439 rows, mostly a handful of
enormous generated files whose rows PCL never produced — so the 13.5 % at the
bottom says more about `t/op/` file sizes than about PCL.  The honest headline
is the sweep's **53 %** (the population PCL is actually measured against every
change) and the companion's **40 %**.

## Against Fable's hand measurement (s486, same day)

Fable measured this by hand with `grep` before the tool existed.  Every
population TOTAL reproduces exactly; three splits differ, and all three are the
hand measurement, not the tool:

- **sweep, +1 not-supported (268 vs 267).**  One row cites
  `not-supported.md 'use strict refs is not enforced'` without an `NS:`
  anchor.  The hand grep looked for `NS:`; the rule takes the spelling too.
  (The `NS:`-first / task-then-`NS:` decomposition, 148 + 119, matches
  exactly.)
- **sweep, `bug` 291 vs 232 / `other` 7 vs 67.**  59 rows read
  `partly #752 (…)`; the hand pass treated the hedge as uncitable, the rule
  reads the task number.  291 − 59 = 232 and 7 + 59 + 1 = 67 reconcile it
  exactly.
- **companion + `t/` shortfall, a `PARKED:` row read as `bug`/`other`.**  Same
  family: a cause carrying both a parking note and a task.  Totals identical
  (companion 11,984; `t/` shortfall 444,439 over 138 files either way).

Two counts in the brief's table are file counts the tool measures differently
and reports directly: the XDIFF population is **112** registered files with
**113** distinct files in the row baseline (the row file has a file the reason
file does not — task #1783), and the shortfall's file counts split by class
rather than by hand-grouped buckets.

## Hygiene: the `other` class must trend to zero

`--hygiene` lists them.  Today: 7 rows, two cause texts, both
`DECIDED … "PCL has no PVBM"` (`perl-tests/ref.t` and `postfixderef.t`).  A
DECIDED divergence is a not-supported decision; it should cite the
`docs/not-supported.md` section it rests on, and then it classifies itself.

## See also

- `docs/DECIDED.md` §s486a — the rule, verbatim, as the one-grep index entry
- `docs/test-debugging-runbook.md` §4 — the FIX-vs-REGISTER decision tree that
  produces these causes in the first place
- `docs/test-skip-registry.md` — the registry whose relabels join the
  denominator
- `docs/STATUS.md` — the user-facing copy of the table
