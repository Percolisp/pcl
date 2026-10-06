# RESUME s507c PART TWO (s510) — committed and reviewed; owed are ONE gate, the start-up measurement, the last light bars and MERGE-READY.  Nothing else.

You are an EXECUTION agent (Opus 5.5) on the PCL project (Perl -> Common Lisp transpiler), a FRESH agent resuming a batch that earlier agents checkpointed.  Read, in this order, through main's checkout:
1. `/home/bernt/pcl/briefs-and-rules-for-claude-subagents/s510/SHARED-BOX.md` (this session's protocol; its FIRST ACTION — your model id into `$W/scratch/s507c/MODEL.txt` — comes before anything else; heavy legs go through `~/pcl-agent-scratch/s510/heavy.sh`),
2. `/home/bernt/pcl/briefs-and-rules-for-claude-subagents/s473/COMMON.md` (the rulebook),
3. `s509/resume-s507c.md` (STEP 2 is the definition of PART TWO and its bars; the #2702 ruling in it STANDS) and, only if you need a member's rule, `s507/s507c-prompt.md`,
4. `$W/scratch/s507c/STOP.md` — the block from "PART TWO (s509, in progress)" to "(s508 checkpoint follows)" is the state.  Its first line still reads `MERGE-READY (PART ONE)`: PART ONE is MERGED (main `ce6aa437`), so that line is history — replace it when you write the new one.

`W=/home/bernt/pcl/.claude/worktrees/agent-a5c8a09980aa19ae8`, HEAD `81860ac2`, 12 commits on main `82e2cab3`; last CODE commit `329a041e` (08:48:03).  Generation **v2-4281**.  Task IDs 2740–2759.  You run NOTHING in `/home/bernt/pcl`.

## What is DONE and STANDS (do not repeat it)
On the code-final tree, logs in `$W/scratch/s507c/`: `sweep5.log` (GATE clean, TOTAL 18731 +0, drops 5), `corpus-diff5.log` (identical 111 vs `ce6aa437`), `emission-ab5.log` (29 SAME / 0 DIFF / 0 RCDIFF), `ir-host-leak5.log` (31 = before), `ir-conform5.log` (326/0/19/0), `companion5.log` (movers spliced in `5eadc3a4`), `everyday5.log` (114 → 114, every bucket 0), `ph-tree3.log` (66/90, no `same` → DIFF), the records (`a5f21117`, `0137ea0a`).  The reviewing session read the code diff and re-ran its probes on `f4a2cf78`: nothing open.

## What happened last and what is OWED, in order
`gate5.log` (09:45) reads `Result: FAIL`, 279 files / 9,782 rows, ONE file: `Pl/t/ir-inventory-01.t` rows 2–3 (the inventory was stale after two exports).  `81860ac2` regenerated `docs/ir-op-inventory.*` and that file passes alone (`ir-inventory5.log`).  So:
1. `git -C "$W" rebase main` (docs / tasks / briefs only since `82e2cab3`; keep both sides of the records), then `env -C "$W" prove Pl/t/pcl-doc-examples-01.t Pl/t/ir-inventory-01.t Pl/t/artifact-staleness-01.t`.
2. The FULL gate ONCE on that tree: `env -C "$W" PCLXS_DIR="$HOME/pclxs" ~/pcl-agent-scratch/s510/heavy.sh s507c leg "$W/scratch/s507c/gate6.log" tools/prove-core` — `Result: PASS`; say the file / row count (main: 279 / 9,770).  `Pl/t/glob-01.t` rows 29–30 are the known #2384 flake: re-run that file alone.
3. The start-up measurement #2689 owes (`$^X` is now looked up at every start): `pcl -e 1` and a warm `pcl hi.pl` (a one-line `print "hi\n"` script, run once first), 10 runs each, main's checkout vs your tree, interleaved, as ONE script file run through `heavy.sh s507c bench "$W/scratch/s507c/startup6.log" <script>`.  Report median and best for each; FLAG anything above 2 % and say where the time goes.  (Your `leg-startup.sh` was queued and never ran — reuse it if it does this.)
4. `tools/tag-license --check` (light); `git -C "$W" status --short` shows only `scratch/`.
5. CI's perl is 5.38.2: you already checked PART TWO's new rows (STOP.md 08:46) — re-check only rows added after that, and say so.
6. `tools/everyday-smoke.pl --record` LAST, from the clean tree, through heavy.sh; commit the history row.  Add the final bars line (gate6, startup6) to `## Session s507c` in `docs/session-log.md` in the same or the next commit.
7. `MERGE-READY: <sha>` as the FIRST line of `$W/scratch/s507c/STOP.md`, every bar's log path under it (each log newer than `329a041e`'s time, the gate and `--record` newer than your rebase).  If you have a SendMessage tool, send one line to `main`: `s507c PART TWO MERGE-READY <sha>`.

If the gate shows anything other than PASS: fix a defect of the batch with a guard row and re-take the bars the fix touches (the table in CLAUDE.md, WHAT TO RUN WHEN); a failure that is not yours is said so with the base's result for the same file.

## Beside you
**s508a** (silent wrongs: constants, a handle's numeric identity, bareword arguments, `&name;`, the text load of a module) runs now and has many legs to take — the lock script serializes you.  **s507p** (perf round 39) starts when you are merged.

## Final report (SHORT)
STOP.md's first line; your model id; the gate's count; the start-up table and whether anything is flagged; anything you could NOT do, said plainly.
