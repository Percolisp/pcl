# s543 RESUME after the crash of 2026-11-16 22:34 (the reviewing session, 22:39) -- s543a and s543b continue in their worktrees

You are a RESUMED batch.  Your brief is unchanged (`scratch/<label>/prompt.md` in your worktree), the rulebook is unchanged
(`briefs-and-rules-for-claude-subagents/s473/COMMON.md`), and your predecessor's handover is `scratch/<label>/STOP.md` + its WIP
commits.  Read those three first, then this file; where they differ, this file wins.

| label | worktree `W` | HEAD at the crash | generation | task IDs |
|---|---|---|---|---|
| s543a | `/home/bernt/pcl/.claude/worktrees/agent-a9d6af55959b1f0c0` | `cd04e308` (WIP 5), tree clean | v2-15100 | 5100-5119 |
| s543b | `/home/bernt/pcl/.claude/worktrees/agent-aca67256b5619cf0d` | `f014f5e2` (WIP 6/7) + an UNCOMMITTED member-7 edit | v2-15200 | 5120-5139 |

You were started in the main checkout, NOT in your worktree: `cd "$W"` first and use absolute paths.  `/home/bernt/pcl` (main) is
read-only for you, as always; tasks go through `~/.claude/tasks/pcl`.  Do NOT create a new worktree.  Append a line
`resumed <time> <your model id>` to `$W/scratch/<label>/MODEL.txt` before anything else (execution agents are `claude-opus-5-5`).

## What happened, and what it did to your files

- **The box went down at 22:34 and was rebooted** (the previous boot's journal ends in `amdgpu` display errors; no OOM kill, no
  thermal event -- not a memory overrun of ours).  `/tmp` is EMPTY.  Nothing runs: no SBCL, no leg; `HEAVY.holder` was cleared by
  the reviewing session (the `flock`s died with their holders).
- **Main did not move**: `05e9867f` (CODE `c5f618f2`, v2-15000).  No rebase.  The git object store was checked (`git fsck` clean);
  the tasks your predecessor filed (5101 5102 5104 5105; 5120-5125; the appends to #398 and #1291) are intact and valid JSON.
- **A file written in the last minutes before a crash can be EMPTY or SHORT although its name is final.**  That is what the module
  cache showed: a zero-size core, a zero-size extension fasl and ~40 zero-size `proto/*.json`.  The reviewing session removed every
  cache file written from 22:28 on, every zero-size one, and the sibling files of each removed entry (239 files).  If a leg still
  shows a load failure that makes no sense (end of file in a fasl, a core that will not start, unparsable JSON under
  `~/.pcl-cache`), SUSPECT A CRASH-DAMAGED CACHE FILE FIRST: note the path in STOP.md, remove that one entry, re-run -- do not
  chase it as a bug of your batch.
- **The same holds for YOUR scratch and working tree.**  Before you trust it, look at every file under `$W` with an mtime from
  22:28 on (`find "$W" -newermt '2026-11-16 22:28' -type f -not -path '*/.git/*'`): a script is re-read, an extraction is
  re-extracted (s543b: `scratch/s543b/snap-pt-f014f5e2` was being written at the crash -- delete and redo; s543a: `board/`),
  a zero-size file is deleted.  Committed content is safe.
- **A log without the `=== heavy.sh ... END rc=` line is VOID** (the standing rule).  That makes VOID, to be re-run:
  - s543a: `board.log` (the board-both leg, base half only begun), `gss.log`, `sweep.log` (it had reached the closure.t retry, but
    it never wrote its END line or its verdict: re-run it whole).  VALID: eab-bar, eab-member, irconf, companion, gate-discovery.
  - s543b: `pt-after.log` (empty); the member-6 gate (`m6-gate.sh`) never started.  VALID: m1..m5 gates, comp-before,
    comp-after12, gss-base.
- **s543b's uncommitted edit** (`Pl/Binder.pm` `outer_bindings_used`, `Pl/Parser2.pm` `_assemble_eval_mode`, 17 lines, member 7 =
  #4796) survived and compiles.  It is UNMEASURED: no probe, no guard row, no gate.  Treat it as a first draft -- run the
  measurement the brief asks for (perl -> base -> tree) before you keep it, then commit WIP 7/7 by named files.

## The box

- **Two batches, THREE heavy slots** (`~/pcl-agent-scratch/s513/heavy.sh`, the three-slot script; the USER: nothing else runs on
  the box).  12 GB; watch `free -g`.
- **A batch has at most TWO bounded legs queued or running at a time.**  At the crash s543a held slots 2 and 3 with a third leg
  just ended and s543b's legs had stood in the queue for up to 20 minutes; with two per batch nobody starves.
- **Exclusive legs** (KIND `bench`, the companion runner, `bench-exec`, the container test) take the whole box: only ONE queued on
  the box at a time (read `HEAVY.holder` and the tail of `HEAVY.queue` first), ONE measurement per leg.  s543a owes three
  (`legs/bench.sh noise`, `legs/bench.sh tree`, `legs/firstrun.sh`): they come AFTER its bounded legs, one at a time, and it looks
  for a waiting sibling leg between them.  A one-file companion or board re-run is `HEAVY_BOUNDED=1`.
- A leg idle at load ~0 for five minutes is HUNG: kill it, mark the log VOID, diagnose.  A one-file run inside a leg takes
  `timeout 120`.

## Merge order

First MERGE-READY first; the other rebases, keeps its generation, regenerates the three artifacts and re-takes what the code move
can change (say in STOP.md which logs the move made VOID).  Both batches change emission.  Nobody waits for a sibling's merge.

## Reports

Keep `STOP.md` current after every leg and commit a WIP checkpoint after every member -- this box has now gone down under a
running batch three times.  `MERGE-READY: <sha>` as the first line of STOP.md when done; the FINAL REPORT is your last message
(short: what shipped, the number per bar, tasks filed / closed, HEAD sha, the commit count over main, the review points the
reviewing session should probe, and anything you could NOT do).
