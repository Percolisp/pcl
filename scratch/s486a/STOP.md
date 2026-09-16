# s486a — STOP file (failure-cause CLASS census, task #1782)

Worktree `/home/bernt/pcl/.claude/worktrees/agent-ad5c8bcbffc408b9a`,
branch `worktree-agent-ad5c8bcbffc408b9a`, based on main `4429126a` (rebased,
up to date).  **Do not merge into main — Fable merges after review.**

**MERGE-READY = the branch TIP** (this commit, whose only content is this
file).  The code + docs are complete at `3cd96da7`; this commit adds only
`scratch/s486a/STOP.md`.  Working tree clean.

## DONE — the work is COMPLETE

- `79f0ead8` **step 1** — `PCLCauses::cause_class` / `class_census` /
  `census_line` + `@CLASSES`; THE RULE verbatim in the module header.
  `causes_line`'s third answer now IS `census_line`, so **all three runners**
  (`tools/sweep-diff.pl`, `tools/run-perl-suite.pl`, `tools/cpan-scoreboard.pl`)
  print the split with no second reading (rule 11); the cause-less count they
  always printed moved to a continuation line.  Three existing tests' CAUSES
  assertions updated to the new shape.
- `010e47e4` **step 2** — `tools/cause-census.pl` + `tools/t/cause-census.t`
  (25 rows, fixture baselines in a tempdir, every class once).
- `3cd96da7` **step 3** — `docs/failure-cause-classes.md`, `docs/DECIDED.md`
  `## s486a`, the `docs/STATUS.md` table, the `docs/test-debugging-runbook.md`
  §4f pointer, `docs/session-log.md`.
- Task JSON written: **#1782** (completed, with the measurements) and **#1783**
  (the XDIFF finding).  `~/.claude/tasks/pcl/`.

## MEASUREMENT — reproduces the brief's table

`perl tools/cause-census.pl --faillog /home/bernt/pcl/.faillog`
(the `.faillog` lives in the MAIN checkout, not here — hence `--faillog`):

| population | rows | not-supported | parked | bug | other | unexplained | perl-skip | ns+parked |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| perl-tests sweep (row-weighted, log of 2026-09-06, 7 orphan rows) | 635 | 268 | 69 | 291 | 7 | 0 | 0 | 53.1% |
| companion `perl-suite-fails.tsv` | 11,984 | 4,661 | 88 | 4,873 | 0 | 2,362 | 0 | 39.6% |
| CPAN board (353 assert + 36 `*FILE*`) | 389 | 88 | 0 | 294 | 0 | 0 | 7 | 23.0% |
| companion XDIFF rows | 2,517 | 2,516 | 0 | 0 | 0 | 1 | 0 | 100.0% |
| shortfall perl-tests | 12,213 | 267 | 0 | 104 | 0 | 11,842 | 0 | 2.2% |
| shortfall perl's t/ | 444,439 | 55,393 | 417 | 388,107 | 0 | 522 | 0 | 12.6% |
| **all** | **472,177** | **63,193** | **574** | **393,669** | **7** | **14,727** | **7** | **13.5%** |

**Differences from the brief's table — every population TOTAL identical; three
splits differ and all three are the hand grep, not the tool.**  Written up in
`docs/failure-cause-classes.md` §"Against Fable's hand measurement":
(a) one sweep row cites `not-supported.md 'use strict refs …'` with no `NS:`
anchor, which THE RULE takes (+1 → 268); (b) 59 rows read `partly #752 (…)` —
a task number behind a hedge — so `bug` 291 vs 232 and `other` 7 vs 67
(291−59 = 232, 7+59+1 = 67, exact); (c) one `PARKED:`+task row in the companion
and in the `t/` shortfall.  The `NS:`-first / task-then-`NS:` decomposition
(148 + 119) matches to the row.  Two file counts are reported directly instead
of the brief's: XDIFF is 112 registered files / **113** distinct files in the
row baseline (→ **#1783**), and the shortfall's file counts split by class.

## BARS — all green

| bar | result |
|---|---|
| `prove tools/t/cause-census.t perl-suite-causes.t audit-instruments.t cpan-scoreboard.t tap-align.t` | **PASS**, 148 rows, 5 files |
| `prove Pl/t/license-tag-01.t no-hardcoded-paths-01.t core-deps-01.t` | **PASS**, 21 rows |
| `perl tools/cause-census.pl` on the real baselines | **GREEN** — the table above |
| `tools/sweep-diff.pl diff baselines/fail-baseline.tsv <main>/.faillog` | **GREEN** — prints `CAUSES: 478 of 478 — not-supported 200, parked 47, bug 223, other 8, unexplained 0` (the KEY-weighted view of the same population); the diff verdict itself is unchanged by this work |
| `tools/cpan-scoreboard.pl --diff <baseline> <baseline>` | **GREEN** — `0 NEW / 0 FIXED / 0 LOST` + `CAUSES: 317 of 317 — not-supported 81, parked 0, bug 229, other 0, perl-skip 7, unexplained 0`.  Its 317 is the scoreboard's KEY-collapsed count (the #1041 unnamed-row rule); the census's 389 is blessed LINES — different units, both correct |
| `tools/run-perl-suite.pl`'s ROW-DIFF line | covered by its unit test (`tools/t/perl-suite-causes.t`) — no cheap invocation prints the ROW DIFF without a full companion run, and three agents share this box |
| `tools/prove-core` | **PASS** — see the tail below |

Nothing under `Pl/` or `cl/` changed, so per the WHAT-TO-RUN-WHEN table
(`docs/**`, `tools/t/**` row) there is **no sweep and no companion run**.

## HYGIENE (brief item 6) — LOOKED UP, LEFT ALONE, as instructed

The sweep's 7 `other` rows are **two cause texts**, both
`DECIDED … "PCL has no PVBM"` (`perl-tests/ref.t` "PVBM ref is not a GLOB ref"
and `postfixderef.t` 72/73).  `grep -n -i "pvbm\|boyer\|study" docs/not-supported.md`
finds **no PVBM section** — only an unrelated `study.t` mention at line 914.
Per the brief ("if none exists, leave them … do not write new not-supported
sections this hour") they were **left untouched**; `baselines/fail-baseline.tsv`
is unmodified.  Whoever writes the PVBM section should add the `NS:` anchor to
those two cause texts BY EDIT and the class drops to zero — `tools/cause-census.pl --hygiene`
lists them.

## RESUME / re-verify

```
cd /home/bernt/pcl/.claude/worktrees/agent-ad5c8bcbffc408b9a
git log --oneline -4
prove tools/t/cause-census.t tools/t/perl-suite-causes.t tools/t/audit-instruments.t tools/t/cpan-scoreboard.t tools/t/tap-align.t
perl tools/cause-census.pl --faillog /home/bernt/pcl/.faillog
perl tools/cause-census.pl --faillog /home/bernt/pcl/.faillog --markdown
perl tools/cause-census.pl --faillog /home/bernt/pcl/.faillog --hygiene
```
