# A verdict register for the CPAN board (and the other test populations) -- PROPOSAL

Status: **proposal** (s530, 2026-10-31, Fable; the USER asked for a register of CPAN test failures that says which ones perl skips too and which ones come from features PCL deliberately does not support).  Not built.  Queued for s531 (see the end).

## The problem, measured

The CPAN board's file verdicts are PASS / PARTIAL / FAIL, and FAIL mixes three different things.  At s530 (30 dists, 896 files) **138 files read FAIL**, and a review had to join three files by hand to find that only **41** of them are PCL bugs:

| what the FAIL really is | files |
|---|---|
| perl SKIPS the file too (`1..0 # SKIP`: a missing optional module, an author test) | 69 |
| fixed after the run started (stale) | 4 |
| perl itself fails the rows (the dist's own TODO) | 2 |
| a feature PCL ruled out (XS 14, `caller` fidelity 4, source filters, B::Deparse, given/when, >U+10FFFF) | 22 |
| **a PCL bug** | **41** |

The row record that should answer this is the CAUSE column of `baselines/cpan-board14-fails.tsv` (2,233 rows), but it is free text:

| how the cause is written today | rows |
|---|---|
| a task number (`#3282 (...)`) | 1,368 |
| `NS:...` (not supported) -- in **18 different spellings** for the same rulings | 761 |
| `PERL-SKIP (...)` | 69 |
| a task AND an NS reason in one cell | 21 |
| "perl itself TODO-fails this row" as prose | 11 |
| "cause not isolated" | 2 |

Consequences seen this session: a cause can go **stale** silently (13 JSON-PP rows still cite #1575, fixed in s516b; the real cause is #3282); the cost of a ruling ("what does not supporting `caller` fidelity cost?") has to be re-derived by hand every time (the s521 review built that table once); and the headline number cannot say "assertions PCL gets wrong that perl gets right, in features we support" -- the number that matters.

perl's own test suite already solved half of this: `cl/skip-registry.lisp` (`docs/test-skip-registry.md`) keys a not-supported row on its description, cites a `docs/not-supported.md` reason, still runs the assertion, and flags itself stale when the row starts passing.  The CPAN board never got the same discipline.

## The proposal

### 1. A closed vocabulary for the CAUSE column

Every row's cause is exactly one CLASS with one KEY, plus an optional free-text note in a new last column:

| class | key | meaning | counts as |
|---|---|---|---|
| `PERL-SKIP` | -- | perl's own run skips this file / row too | **not a failure** |
| `PERL-FAILS` | -- | perl fails or TODO-fails the row too | **not a failure** |
| `NS` | an id from `docs/not-supported.md` | a feature PCL deliberately does not support | ruled out |
| `BUG` | `#NNNN`, an OPEN task | a PCL bug | **ours** |
| `UNREDUCED` | `#NNNN`, an open task holding the hunt | not isolated yet | **ours** |
| `HARNESS` | `#NNNN` | the board / runner, not PCL | instrument |

A row that today names a task AND a ruling (`#233 + NS:caller`) gets the ruling as its class when the ruling is the reason it stays failing, and the task in the note.

### 2. Stable ids for the rulings

Each section of `docs/not-supported.md` (107 today) gets a short stable anchor (`ns-xs`, `ns-caller-fidelity`, `ns-destroy-scope-exit`, `ns-lvalue-sub`, `ns-source-filters`, ...).  The CPAN register, `cl/skip-registry.lisp` and the companion suite's baselines all cite THE SAME ids -- one ruling, one name, countable across every population.

### 3. A validator in the gate (`Pl/t/`, cheap, no SBCL)

- every cause parses into the vocabulary;
- every `NS` id exists in `docs/not-supported.md`;
- every `BUG` / `UNREDUCED` / `HARNESS` task exists and is NOT completed -- **a cause pointing at a closed task fails the gate** (the stale #1575 rows would have been caught the day #1575 closed);
- `PERL-SKIP` / `PERL-FAILS` are not taken on trust: the board already runs perl as its oracle, so the scoreboard checks them against perl's own TAP for that file at every board run and reports a mismatch.

### 4. The scoreboard reports by class

- File verdicts gain **SKIP** (#4160): a file perl also skips is never FAIL.
- The per-dist line and the totals split the not-ok rows: **ours** (BUG + UNREDUCED) / **ruled** (NS) / **perl's** (PERL-*) / harness.
- The headline becomes: *"N assertions PCL gets wrong that perl gets right, in supported features"* -- with the ruled-out cost beside it per NS id (the s521 "NOT IN THE PLAN" table becomes a query, not a review).

### 5. Migration (one batch, rows BY EDIT)

A one-shot script maps the 2,233 causes: the 1,368 task causes and the 69 PERL-SKIP mechanically (checking each task's status -- closed ones are surfaced for re-attribution, never auto-mapped); the 761 NS rows by mapping the 18 spellings onto the new ids; the 21 mixed and 13 prose rows by reading them.  The baseline's blessing rule does not change: rows leave by EDIT, never by re-blessing a run.

## Size and order

Tools + baselines + one docs pass; **no compiler or runtime change**.  About one agent session.  It replaces #4160 (the SKIP verdict is its item 4) as **s531b's member 0**; the companion suite's alignment to the same ids (item 2) can follow in a later batch.

Open question for the USER: whether the companion suite's (perl's own `t/`) failure baselines should move to the same vocabulary in the same batch, or after the CPAN board has used it for a round.
