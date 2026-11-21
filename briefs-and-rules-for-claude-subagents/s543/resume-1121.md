# s543 RESUME of 2026-11-21 (the reviewing session, 14:4x) -- s543a and s543b finish in their worktrees

You are a RESUMED batch (the third agent of yours).  Your brief is unchanged (`scratch/<label>/prompt.md` in your worktree), the
rulebook is unchanged (`briefs-and-rules-for-claude-subagents/s473/COMMON.md`), your predecessor's handover is
`scratch/<label>/STOP.md` + its WIP commits.  Read those three, then `s543/resume-s543.md` (the box rules: the three-slot
`~/pcl-agent-scratch/s513/heavy.sh`, at most TWO bounded legs per batch, exclusive legs one at a time on the box, the END-line
rule, the crash lesson about the cache), then this file.  Where they differ, this file wins.

| label | worktree `W` | HEAD | over `05e9867f` | generation | task IDs |
|---|---|---|---|---|---|
| s543a | `/home/bernt/pcl/.claude/worktrees/agent-a9d6af55959b1f0c0` | `bd8c819d` (WIP 13), clean | 13 | v2-15100 | 5100-5119 |
| s543b | `/home/bernt/pcl/.claude/worktrees/agent-aca67256b5619cf0d` | `f99be16c`, clean | 14 | v2-15200 | 5120-5139 |

You were started in the main checkout, NOT in your worktree: `cd "$W"` first and use absolute paths.  `/home/bernt/pcl` (main) is
read-only for you; tasks go through `~/.claude/tasks/pcl`.  Do NOT create a new worktree.  Append a line
`resumed <date time> <your model id>` to `$W/scratch/<label>/MODEL.txt` before anything else (execution agents are
`claude-opus-5-5`).

## What is different from the stop order of 2026-11-16

- **Five days passed; the box was shut down in order and booted today** (no crash: the cache's newest file is from 23:30 on the
  16th and no cache file except the `.lock` files is empty).  `/tmp` is EMPTY.  Nothing runs; `HEAVY.holder` is empty; the tail of
  `HEAVY.queue` is stale text from the 16th.
- **Main moved in RECORDS only**: `05e9867f` -> `ef0e30e6` (docs, briefs, tasks, memory, `tools/task-triage.pl`; CODE is still
  `c5f618f2`, v2-15000).  **Rebase onto `ef0e30e6` FIRST** (`git -C "$W" rebase ef0e30e6`, alone in its command, no pipe after it;
  check its exit status).  Main edited `docs/DECIDED.md`, `docs/session-log.md`, `docs/supported-subset.md`, `CLAUDE.md` and
  `docs/claude-md/REFERENCE.md`: a conflict there is resolved by keeping BOTH sides (your section and main's).  The rebase moves no
  code, so it voids no log; your extraction `scratch/<label>/base` (CODE `c5f618f2`) stays the base.  After it:
  `git diff ef0e30e6 --stat -- Pl cl lib vendor` must show exactly your batch.
- **Your STOP.md's OWED / NEXT STEPS list is the work.**  Nothing is added to it, nothing is dropped from it.
- **The exclusive legs go FIRST on the box, s543a's three timing legs before anything else** (a quiet box is their whole point):
  s543a queues `bench noise` at once; s543b starts with what needs no SBCL (the artifact commands are light: run them through
  `heavy.sh` as a bounded leg all the same) and with its bounded legs -- `heavy.sh` holds an exclusive leg back until the slots
  are free and then gives it the box.  An exclusive leg of s543b (its everyday legs) queues only when no other exclusive leg waits.
- **Merge order**: the first `MERGE-READY` is merged first.  The reviewing session then sends the other batch ONE message
  ("main is now <sha>"): it rebases, keeps its generation (s543b renumbers only if main passed v2-15200), regenerates the three
  artifacts on the rebased tree and re-takes the full gate + corpus-diff + emission-ab against the new main, and the full sweep
  (s543a touches every file's emission, s543b name resolution).  So: after your MERGE-READY report, your worktree stays as it is.
  s543b: no guard row of yours may depend on the absence of `(p-line N)` forms -- check that now, on your own tree, by reading
  your guard rows' expectations (a row that matches emitted TEXT is the suspect; a row that RUNS is safe).

## Reports

`STOP.md` current after every leg; a WIP commit (named files, never `git add -A`) after every step that changes the tree.
`MERGE-READY: <sha>` as the first line of STOP.md when done.  The FINAL REPORT is your last message: what shipped, the number per
bar (each with its log's name), tasks filed / appended, HEAD sha, the commit count over `ef0e30e6`, the review points the
reviewing session should probe, the CLAUDE.md / REFERENCE.md pointers it should apply, and anything you could NOT do.
