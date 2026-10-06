# RESUME s508a (s510) — the s509 resume brief is still the job, unchanged: it was written and never run.  This page says only what is different now.

You are an EXECUTION agent (Opus 5.5) on the PCL project (Perl -> Common Lisp transpiler), a FRESH agent resuming a batch another agent checkpointed.  Read, in this order, through main's checkout:
1. `/home/bernt/pcl/briefs-and-rules-for-claude-subagents/s510/SHARED-BOX.md` (this session's protocol; its FIRST ACTION — your model id into `$W/scratch/s508a/MODEL.txt` — comes before anything else; heavy legs and benches go through `~/pcl-agent-scratch/s510/heavy.sh`),
2. `/home/bernt/pcl/briefs-and-rules-for-claude-subagents/s473/COMMON.md` (the rulebook),
3. **`s509/resume-s508a.md` — the job: what is RULED (#2740 stays open; #2632 is flagged until benched, 3 % stop rule), what is owed and in which order, the reviewing session's probe result, the form of the final report.**  Where it says `~/pcl-agent-scratch/s509/heavy.sh`, use the `s510` one,
4. `s508/s508a-prompt.md` (the batch: members and bars),
5. `$W/scratch/s508a/STOP.md` — its top block ("RESUME RECIPE" …) is the state.

`W=/home/bernt/pcl/.claude/worktrees/agent-a587431876e18282b`, HEAD `cfbbcd34`, 14 commits on `16cf9642`.  Task IDs 2780–2799 (2780, 2781 used).  Generation **v2-4380**.  You run NOTHING in `/home/bernt/pcl`; every command is `env -C "$W" …` / `git -C "$W" …`.

## What is different from the s509 brief
1. **Your first rebase crosses CODE.**  s507c PART ONE was merged in s509 (`~/pcl-agent-scratch/s509/MAIN-READY-1`; main's last code commit is `ce6aa437`, gen v2-4280, gate 279 files / 9,770 rows, sweep 18731, EVERYDAY 114 of 122).  It touches `Pl/Parser.pm`'s feature callback, `%p-load-unit` in the runtime, `pcl` / `pl2cl` (`--`), `Pl/t/feature-pragma-01.t` and three companion baseline rows.  Keep BOTH sides, keep v2-4380, regenerate the three artifacts and `tools/ir-inventory.pl` (`Pl/t/ir-inventory-01.t` and `Pl/t/artifact-staleness-01.t` must pass), re-run your guard files, then go on.  After the rebase `corpus-diff` / `emission-ab` take **`ce6aa437`** as their reference, and your probe base is a fresh extraction of main (your `scratch/s508a/base` is `fee16466`: keep it for the bisect's history, make `scratch/s508a/base-main` for everything new).
2. **A second rebase will come while you work:** s507c PART TWO (the ARGV family — `eof()`, `local *ARGV`, `${^GLOBAL_PHASE}`, `$^X` at start, the value of a sub's last statement, gen v2-4281) merges first; the reviewing session writes `~/pcl-agent-scratch/s510/MAIN-READY-1` and tells you.  Rebase across it at your next step boundary (SHARED-BOX says what is re-taken).  Your #2764 and its #2689 both sit near the loader, in different functions.
3. **Two companion rows are already explained, do not spend time on them:** `re/regex_sets_compat.t` 1692/337 (#2646, pre-existing, left unblessed on purpose) and `mro/method_caching_utf8.t` 17/4 (your own base run measured base == tree; the snapshot row is stale — splice it with that cause when you splice your movers).
4. **CI's perl is 5.38.2** (SHARED-BOX): check your new gate rows for 5.40-only spellings in the perl ORACLE and say in STOP.md that you did.
5. The two suspected regressions (`io/pipe.t`, `op/fork.t`) are STILL the first thing after the rebase — bisect on the REBASED member commits, three runs each, as the s509 brief rules.

## Beside you
**s507c PART TWO** runs now (a gate, a start-up bench, then it merges); **s507p** (perf round 39, runtime only: scalar store paths, the in-memory filehandle, in-place `.=`) starts when PART TWO is merged and will hold the lock for a whole-table bench — while `HEAVY.holder` says `bench` for another label, keep your light work to ONE process.

## Final report (SHORT)
As `s509/resume-s508a.md` says.
