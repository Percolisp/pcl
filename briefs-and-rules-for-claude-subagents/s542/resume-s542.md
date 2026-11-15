# s542 RESUME (the reviewing session, 2026-11-15 18:5x) -- s540c, s540d, s539a continue in their worktrees

Read this AFTER `s541/resume-after-stop.md` (what each batch left at the stop of 2026-11-14: which logs are VALID, what is OWED, what the merge review will read) and `s540/resume-s540.md` (its rules still hold: rule 25, the END-line rule, MODEL.txt first, `timeout 120` on a one-file run, a leg idle at load ~0 for five minutes is HUNG); where they differ, this file wins.

## What changed since `s541/resume-after-stop.md`

- **The box was REBOOTED** (up since 2026-11-15 18:47).  `/tmp` is EMPTY: any `/tmp/pcl-*` ref-tree, extraction or fifo a script of yours expected is gone (`git worktree prune` if a nested worktree of yours lived there).  The worktrees, `scratch/<label>/`, `~/.pcl-cache` and `~/pcl-agent-scratch` are intact.  Checked 18:5x: no heavy leg, no SBCL, `HEAVY.holder` empty, 10 GB available, load 0.4.
- **Main = `6a8ab355`** = MAIN-READY-92 `212d4782` + records only (CODE sha `24988e35`, generation **v2-14400**; the gate on main is 372 files since `Pl/t/claude-md-size-01.t`; sweep 18,803; EVERYDAY 115 of 122).  origin/main = the same; CI green on it.  Rebase first; a records-only move re-takes nothing.  **Before you conclude that main moved in CODE, check `git log -1 --format=%h main -- Pl cl lib vendor`.**
- **THREE agents run, THREE heavy slots** (`~/pcl-agent-scratch/s513/heavy.sh`).  Model + effort: Opus 5.5 (`claude-opus-5-5`) at xhigh; MODEL.txt first.
- **Generations (unchanged):** s540c **v2-14500**, s540d **v2-14600**, s539a **v2-14800**.  Keep yours across a rebase; regenerate the three artifacts when main moves in CODE.
- **Never edit `CLAUDE.md`**: a pointer your batch needs there goes into STOP.md under "CLAUDE.md pointers for the reviewing session".  A doc you add gets its description in `docs/claude-md/REFERENCE.md` "Semantic Deep-Dives" by you.

## THE EXCLUSIVE-LEG ORDER (the s541 lesson: three batches each queued an exclusive leg and stood in line for an hour)

An EXCLUSIVE leg (KIND `bench`, or a command containing `tools/run-perl-suite.pl`, `tools/bench-exec.pl` or `tools/t/install-container.t`) takes the WHOLE box, and while one WAITS no bounded leg starts.  So:

1. **Only ONE exclusive leg is queued on the box at a time.**  Before you queue one, read `~/pcl-agent-scratch/s513/HEAVY.holder` and the tail of `HEAVY.queue`: if a sibling's exclusive leg is waiting or running, do your bounded legs / probes / records first and look again when it has ended.
2. **Starting order** (the box is quiet now, which is what a timing needs):
   - **s540d FIRST**: bench1, bench2, comptr -- queue the first one as soon as your rebase is done; chain them, they are one turn.
   - **s540c**: the rebase, then the BOUNDED legs (the targeted gate, the full sweep on the final runtime); its exclusive legs (the companion one-file runs, the bench, the SBCL 2.5.2 floor leg ALONE, #5006) come after s540d's timing legs.
   - **s539a**: the BOUNDED legs first (everyday before -> after, Pod-Simple `corpus.t` one-file + its 14 row edits, `tools/t/install-pcl.t`); its companion `--all --quick` is the LAST exclusive leg of the three -- and if a sibling has merged CODE by then, take it on the rebased tree, once.
3. Rule 25 still holds: nobody waits for a sibling's MERGE.  The order above is about slots only.

## Merge order and what a code move costs

First MERGE-READY first.  s540d and s539a change emission; s540c does not.  When the reviewing session tells you main moved in CODE: rebase, keep your generation (renumber above main's only if main passed it), regenerate the three artifacts, and re-take what the move can change (the full gate, corpus-diff, the A/Bs; the sweep per the WHAT-TO-RUN-WHEN table) -- say in STOP.md which logs the move made VOID.

## Reports

`STOP.md` in `scratch/<label>/` is the handover; keep it current after every leg.  `MERGE-READY: <sha>` as its first line when done; the FINAL REPORT is your last message (short: what shipped, the number per bar, tasks filed / closed, HEAD sha, the commit count over main, the review points the reviewing session should probe, and anything you could NOT do).

## Added 20:4x (two lessons of this sitting; they bind every batch from now on)

1. **An exclusive leg holds ONE measurement.**  s539a's single exclusive leg carried everyday before/after, two bench A/Bs, the companion `--all --quick` and 33 mover re-runs; it held all three slots for 52 minutes, the last 20 of them with two `--jobs 1` runs on an otherwise idle box.  The companion's mover re-runs (each file ALONE, base and tree) are `--jobs 1` pass/fail work: queue them as their OWN leg with `HEAVY_BOUNDED=1 heavy.sh LABEL leg LOG /abs/SCRIPT` (new in `heavy.sh` since 20:4x: one slot, the 4G fence, although the script names the companion runner; never for a timing, never for `--jobs` above 1).  Everyday is a bounded leg, always.
2. **`heavy.sh` reads only its FIRST command word as a script.**  `heavy.sh LABEL leg LOG bash SCRIPT` is NOT recognised as exclusive even when SCRIPT runs the companion or a bench (s540c's first companion queue ran 14 s beside a sibling's gate).  Fixed 20:4x: every argument that is a file is now read.  Still: pass the script path itself as the command, the script executable.
3. **The companion snapshot is many merges old** (the last `--all --quick` on main was at MAIN-READY-63): a batch's `--all --quick` now shows dozens of "movers" that read the same on the base.  Until the reviewing session has run the ROUND, a batch that owes `--all --quick` compares TREE against BASE (both run), never against the snapshot alone, and reports only rows that differ between the two.

## Main moves in CODE during this sitting
s540c is MERGE-READY (`d7e44ddf`, a `cl/pcl-runtime.lisp` + `Pl/Parser2.pm` change: dropped filehandles, `local` on a file-level lexical's element; generation v2-14500).  It merges first; the reviewing session tells each running batch when main has moved.  Then: rebase, keep your generation, regenerate the three artifacts if yours changes emission, re-take what the move can change.

## Added 23:4x -- FOUR heavy slots for this sitting
The USER: everything else on the box is turned off.  `~/pcl-agent-scratch/s513/heavy.sh` is the FOUR-slot script (= the s536 four-slot script + today's changes; the three-slot one is `heavy.sh.three-slot-s542` beside it and goes back in at the next session start unless the USER says the box is idle again).  A waiter started before 23:4x runs the old script and only ever gets slots 1-3; slot 4 is not taken while such a leg holds the whole box.  Watch `free -g`: 12 GB, four legs peak ~1.8 GB each under their 4G fence.
