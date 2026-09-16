# s486a — STOP file (failure-cause CLASS census, task #1782)

Worktree `/home/bernt/pcl/.claude/worktrees/agent-ad5c8bcbffc408b9a`,
branch `worktree-agent-ad5c8bcbffc408b9a`, based on main `4429126a` (rebased,
up to date).  **Do not merge into main — Fable merges after review.**

## DONE (each commit green before the next)

- `79f0ead8` **step 1** — `PCLCauses::cause_class` / `class_census` /
  `census_line` + `@CLASSES`; THE RULE verbatim in the module header.
  `causes_line`'s third answer now IS `census_line`, so **all three runners**
  (`tools/sweep-diff.pl`, `tools/run-perl-suite.pl`, `tools/cpan-scoreboard.pl`)
  print the split with no second reading (rule 11); the cause-less count they
  always printed moved to a continuation line.  Three existing tests' CAUSES
  assertions updated to the new shape.
- `010e47e4` **step 2** — `tools/cause-census.pl` + `tools/t/cause-census.t`
  (25 rows, fixture baselines in a tempdir).

## MEASUREMENT (the bar: reproduce Fable's table)

`perl tools/cause-census.pl --faillog /home/bernt/pcl/.faillog`
(the `.faillog` lives in the MAIN checkout, not here — hence `--faillog`):

| population | rows | not-supported | parked | bug | other | unexplained |
|---|---:|---:|---:|---:|---:|---:|
| perl-tests sweep (row-weighted, log of 2026-09-06, 7 orphan rows) | 635 | 268 | 69 | 291 | 7 | 0 |
| companion `perl-suite-fails.tsv` | 11,984 | 4,661 | 88 | 4,873 | 0 | 2,362 |
| CPAN board | 389 (353 assert + 36 `*FILE*`) | 88 | 0 | 294 | 0 | 0 (+7 perl-skip) |
| companion XDIFF rows | 2,517 | 2,516 | 0 | 0 | 0 | 1 |
| shortfall perl-tests | 12,213 | 267 | 0 | 104 | 0 | 11,842 |
| shortfall perl's t/ | 444,439 | 55,393 | 417 | 388,107 | 0 | 522 |

**Differences from the brief's table, all explained, none a tool bug** — see
`docs/failure-cause-classes.md` §"Against Fable's hand measurement".

## OWED (in order)

1. `docs/failure-cause-classes.md` (the rule, six classes, registry, command,
   today's table + the reconciliation).
2. `docs/DECIDED.md` `## s486a` (THE RULE verbatim + pointer).
3. `docs/test-debugging-runbook.md` one pointer where causes are described.
4. `docs/STATUS.md` — the `--markdown` table under "Failures are tracked row
   by row", with its date and the command.
5. `docs/session-log.md` entry (newest first); task `#1782` JSON.
6. Findings to file: **#1783** (`mro/inconsistent_c3_utf8.t` has a blessed
   XDIFF row in `perl-suite-expected-rows.tsv` but NO reason row in
   `perl-suite-expected.tsv` — its sibling `mro/inconsistent_c3.t` has one).
7. Hygiene (only if time): the 7 sweep `other` rows are TWO cause texts, both
   `DECIDED … "PCL has no PVBM"` (postfixderef.t 72/73 + ref.t).  `grep -a`
   `docs/not-supported.md` for a PVBM section; if one exists add the `NS:`
   anchor to those causes BY EDIT in `baselines/fail-baseline.tsv` with a
   header note naming s486a; if none exists LEAVE THEM and say so here.
   **Not done — status: not yet looked up.**

## BARS

| bar | result |
|---|---|
| `prove tools/t/cause-census.t tools/t/perl-suite-causes.t tools/t/audit-instruments.t tools/t/cpan-scoreboard.t tools/t/tap-align.t` | NOT YET RUN AS A SET (the first four ran green individually) |
| `prove Pl/t/license-tag-01.t Pl/t/no-hardcoded-paths-01.t Pl/t/core-deps-01.t` | NOT YET RUN |
| `perl tools/cause-census.pl` on the real baselines | GREEN — see the table above |
| `tools/sweep-diff.pl diff baselines/fail-baseline.tsv .faillog` | NOT YET RUN |
| `tools/cpan-scoreboard.pl --diff` | NOT YET RUN |
| `tools/prove-core` (ONCE, at the end) | NOT YET RUN |

## RESUME

```
cd /home/bernt/pcl/.claude/worktrees/agent-ad5c8bcbffc408b9a
git log --oneline -3
prove tools/t/cause-census.t tools/t/perl-suite-causes.t tools/t/audit-instruments.t tools/t/cpan-scoreboard.t tools/t/tap-align.t
perl tools/cause-census.pl --faillog /home/bernt/pcl/.faillog --markdown
perl tools/cause-census.pl --faillog /home/bernt/pcl/.faillog --hygiene
```
Nothing under `Pl/` or `cl/` changes here, so no sweep and no companion run.
