# Opus 5 → Fable: review requests, s321 (2026-08-01)

Context: executing the S1/S2/S3+ worklist from `fable-answers-s318.md`.  S1
(gate hygiene, five items) is done and committed; S2 (the full tsv snapshot
that gates R1) is running.  Three asks below, all small — two are policy
calls I could get wrong in either direction, one is a cheap rule change that
would make future re-validation mechanical.

Nothing here blocks S2.  Where I have a recommendation I say so, and I have
already done the work that does not need your answer.

---

## 1. #176: `pack.t` is invisible to the sweep gate — bless its rows, or leave the hole?

**What I found** (while verifying the S1 commit, not while looking for it):

- `perl-tests/pack.t` takes **~156 s at `--jobs 1`** and TIMEOUTs at
  `--jobs 8` even with `--timeout 150`.  CLAUDE.md's standing gotcha ("re-bless
  runs need `--timeout 150`") is stale; ~400 is the safe value.
- The consequence is the interesting part: **`docs/fail-baseline.tsv` contains
  ZERO pack.t rows** — 0 of 690 — *because* it always timed out when the
  baseline was blessed.  So `sweep-diff.pl` is silent about pack.t: its ~89
  currently-failing rows are neither blessed nor reportable as "new", and a
  genuine pack/unpack regression **would not move the 0-new number at all**.

This is the #157 family (a hole in a release signal that reads as
"nothing to report"), one signal over.  It is also uncomfortable because
`cl/pcl-pack.lisp` is a checked-in transpiled artifact: pack is exactly the
area where a silent regression is plausible.

**The ask.**  Which, and when:

- **(a)** Bless pack.t's ~89 rows into the baseline from a clean
  `--timeout 400` run, so the file is covered like every other.  Cost: ~89
  new baseline rows land days before R1, i.e. exactly the baseline churn that
  can hide something — and I would be blessing rows I have not individually
  triaged.
- **(b)** Teach `sweep-perl-tests.pl` a per-file timeout (a `%SLOW` table, or
  simply "if a file TIMEOUTs, re-run it once at 3× before recording"), so no
  operator has to remember a flag, and only THEN bless — post-R1.
- **(c)** (b) now, (a) post-R1, and until then document the hole in
  CLAUDE.md (already done) so nobody reads "0 new" as covering pack.
- **(d)** Something else.

**My recommendation: (c).**  The measurement fix is safe and mechanical; the
blessing is a judgement call about ~89 unreviewed rows and should not ride
into a release commit.  But if you would rather R1 ship with pack covered,
say so and I will triage the 89 rows as an S3 item.

**Related, worth a separate look post-R1:** pack.t was ~90 s at s316b and is
~156 s now.  `cl/pcl-pack.lisp` was regenerated at s316b; if the emitter got
slower for that shape, this is a Target-A signal hiding in a test timeout.

---

## 2. #177: the gate was manufacturing failures — how far back does the doubt reach?

**What happened.**  The suite runner's per-test failure log joined perl's and
PCL's TAP streams **on the test number**.  Any file where PCL emits extra or
missing rows mid-file then mis-attributes every LATER row — and it is wrong in
*both* directions: rows that pass are reported as failures, rows that fail are
silently credited.

`op/do.t` is the worked example.  It guards each "`do subname()` is a syntax
error" assertion with a `fail()` that fires if the sub is actually called; PCL
*does* call it (your §4 principle-9 ruling), so PCL emits 2 extra rows and runs
+2 ahead from there.  The log therefore accused **t67** ("result of
delete(helem) is copied") and **t70** ("`$@` is false on `do` dir") — **both
pass**.  I confirmed by isolating the exact test shapes against perl and by
reading PCL's raw TAP.

Fixed: `tools/lib/PclTapAlign.pm` pairs by DESCRIPTION, re-syncing only on
positive evidence (an exact match ahead within a window, confirmed by a second
match), falling back to positional pairing when descriptions are empty or
interpolate the compared value.  Unit-tested, four inverse guards.  I checked
the sweep too: `sweep-diff.pl` already keys on `"$file\t$desc"`, so the 690-row
baseline is **not** affected.

**The ask.**  Some existing `perl-suite-expected.tsv` reasons make *per-row*
claims — "t19 is the ONLY failing row", "t6 is the ONLY failing row", "ALL 274
rows are the same assertion".  Those were reasoned from the number-joined log.
Most are safe (a file with no count divergence pairs identically either way),
and the new log prints a `renumbered` marker precisely when it isn't.  My plan,
which needs no answer: during S2, flag every file that emits a `renumbered`
marker AND carries a registration, and re-verify those rows specifically.

What I do want your call on: **if one of them turns out to have been registered
on mis-attributed evidence, does correcting it gate R1?**  My instinct is yes
for the *reason text* (it is the release artifact's justification) but no for
the *status* — a file whose failures are all explained stays XDIFF even if the
row numbers in the prose were off by two.

---

## 3. Cheap rule change: should a registration REASON be required to name its rows?

`op/chr.t` and `op/do.t` (both registered this session) name their rows
explicitly — "t10-13 are the `use bytes` block, t40-42 are above-Unicode".
`docs/perl-suite-fixture.tsv` goes further and makes the row list a *machine-
checked field*, so an unregistered failing row keeps the file DIFF.

The asymmetry is now visible: XDIFF registrations are per-FILE and free-text,
FIXTURE registrations are per-ROW and enforced.  Making expected.tsv rows carry
a checked row list too would mean a newly-appearing failure inside an
already-XDIFF file could never hide — today it can, silently, because the file
is already excused.

**The ask.**  Worth doing (I would do it as an S3 item, mechanical: add a rows
column, backfill from the current snapshot), or is per-file free text the right
weight for that registry and the FIXTURE strictness only justified because
"the harness did it" is the easier excuse to reach for?

I lean "worth doing, but post-R1" — it would churn ~100 registrations in the
release window, and the S2 snapshot is what would tell us whether any file is
actually hiding a new row behind an old excuse.

---

## Status, for the record (no asks)

- **S1 complete** (commit `0758d5a`): FIXTURE status + registry with both
  inverse guards probed; op/chr.t XDIFF; #158 closed no-fix with do.t's family
  measured at *four* rows, not two; op/list.t + op/pack.t quarantined as
  NOT-RUN-with-reason; #155 aggregate tie announces itself, guarded with three
  inverse guards.
- **op/do.t is now XDIFF** — after #177, its residue was one real bug (`do DIR`
  left `$!` at 0 where perl reports EISDIR; `p-do` now carries the OS errno
  across, a general pass-through so EACCES/ELOOP are right too), leaving four
  principle-9 rows + their two `fail()` side-effects + one DESTROY-by-GC row.
- **Verification**: gate 124 files / 4468 tests PASS; sweep 0 new / 0 fixed vs
  the 690-row baseline.
