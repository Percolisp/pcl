# s543 RESUME of 2026-11-22 (the reviewing session, 14:3x) -- s543f (PPI batch 11) finishes in its worktree; two NEW batches run beside it

You are the RESUMED s543f (the second agent of that batch).  Your brief is unchanged (`s543/s543f-prompt.md`; your copy is
`scratch/s543f/prompt.md` if your predecessor made one), the rulebook is unchanged
(`briefs-and-rules-for-claude-subagents/s473/COMMON.md`), your predecessor's handover is `scratch/s543f/STOP.md` + its WIP
commits.  Read those three, then `s543/resume-s543.md` (the box rules, the END-line rule, the crash lesson about the cache) and
`s543/resume-1121.md` (how a resume works), then this file.  Where they differ, this file wins.

| label | worktree `W` | HEAD | generation | task IDs | PPI clone |
|---|---|---|---|---|---|
| s543f | `/home/bernt/pcl/.claude/worktrees/agent-a64e9ad186336c772` | `f83ea7da` (WIP 4), clean; 5 commits over the OLD base `ffc7eb72` | v2-15500 | 5230-5249 (5230-5233 used) | `scratch/s543f/PPI`, branch `pcl-s543f` = `72d2bd9e` (eight commits over `842e6bbd`); the BARE MIRROR is NOT moved |

You were started in the main checkout, NOT in your worktree: `cd "$W"` first and use absolute paths.  `/home/bernt/pcl` (main) is
read-only for you; tasks go through `~/.claude/tasks/pcl`.  Do NOT create a new worktree.  Do not spawn subagents.  Append a line
`resumed 2026-11-22 <time> <your model id>` to `$W/scratch/s543f/MODEL.txt` before anything else (execution agents are
`claude-opus-5-5`; anything else: write it, stop, report only that).

## What is different from the stop order of 2026-11-21

- **The box was shut down in order and booted today at 14:22** (no crash: no zero-size cache file, the cache's newest file is
  from 21:51 on the 21st).  `/tmp` is EMPTY.  Nothing ran when you were started; `HEAVY.holder` is empty; everything in
  `HEAVY.queue` above the `2026-11-22` marker line is stale text.
- **Main moved in CODE since your base**: `ffc7eb72` -> main's head (MAIN-READY-104 `8959afae` = s543e, v2-15400, + record
  commits).  **Rebase onto main's head FIRST** (`git -C "$W" rebase main`, alone in its command, no pipe after it; check its exit
  status).  Records conflicts (`docs/DECIDED.md`, `docs/session-log.md`, baselines): keep BOTH sides.  Keep **v2-15500**.
- **Your STOP.md's OWED list (items 1-8) is the work.**  Nothing is added, nothing dropped -- except the three points below.
- **THE USER (2026-11-22): the box is otherwise idle -- `heavy.sh` is the FOUR-SLOT script.**  Three batches run: you, **s543g**
  (the `local` nested-element family #540 #1190 #3415 #1339, #691's raw-slot verdict, the dropped-handle flush #4794 #5128
  #5160; `Pl/Parser2.pm`'s `_local_*` / `_fh_drop_*` functions, `Pl/Stmt.pm` `local_shape`, `Pl/VarAnnotator.pm`,
  `cl/pcl-runtime.lisp`'s `p-local-*` and `p-flush-dropped-handle`; v2-15600) and **s543h** (string-eval name resolution #2091
  #2557 #2630 #4821; `Pl/Binder.pm`, Parser2's `_assemble_eval_mode` / `_binding_at` / state prepass, `Pl/ExprToCL.pm`
  `_eval_lexical_alist`, the runtime's eval entry; v2-15700).  You touch none of those functions; they touch none of yours
  (`vendor/PPI`, `Pl/PPIBugs.pm`, `docs/ppi-*`, the `_repair_*` passes, PExpr's `_term_extent`).  A batch has at most TWO
  bounded legs queued or running at a time; ONE exclusive leg (`bench`, everyday, the companion runner) queued on the box at a
  time -- read `HEAVY.holder` and the tail of `HEAVY.queue` first.  You are the batch nearest its end: your full gate and sweep
  go into the queue as soon as the rebase and the artifacts are committed.
- **You will most likely merge FIRST.**  If the reviewing session tells you instead that main moved in CODE: rebase, keep
  v2-15500, regenerate the three artifacts, re-take the full gate + corpus-diff + the wide A/B against the new main, and the
  sweep.

## At MERGE-READY (as your brief and STOP.md item 8 say)
The bare-mirror fast-forward (`842e6bbd` -> your branch head; nothing else is pushed, never to GitHub), `--record` LAST on the
clean committed tree, `MERGE-READY: <sha>` as the first line of STOP.md.  After the report your worktree stays as it is.

## Final report (SHORT; your last message)
`MERGE-READY: <sha>` first (or `NOT MERGE-READY` + reason); model id; per member one line; every bar QUOTED with its log's name
(full gate files / rows, sweep TOTAL and GATE line, the wide A/B's SAME / DIFF / RCDIFF with each DIFF's cause, corpus-diff's
SILENT-DROP line, gate-set scan, drop census, ir-conform, ir-host-leak, the expect checker, EVERYDAY base -> tree, the PPI
suite's counts); the PPI branch head and the mirror's; tasks filed / appended with their class word; what PPI batch 12 (the LAST)
should hold as you now see it (`docs/ppi-fix-at-source-s527.md` section 10 as you corrected it, and whether the branch can
decide section 11's user half); the review points the reviewing session should probe; HEAD sha, `main..HEAD` count; anything you
could NOT do.
