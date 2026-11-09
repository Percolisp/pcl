# s537 STOPPED EARLY at the USER's word (2026-11-10 01:1x) -- how s536i, s536j, s537a and the round board resume

All three agents and the round board were STOPPED at 01:17-01:18; their detached legs were then killed (`systemctl --user stop run-p*.scope` + the stray `pl2cl --xserver` listeners).  Checked after: no heavy leg, no SBCL, `HEAVY.holder*` removed.  **A log is a RESULT only if its last line is `=== heavy.sh <label> ... END rc=...`.**  Read this AFTER `s537/resume-s537.md` and `s536/resume-after-oom.md` (rules 1-8 still hold); where they differ, this file wins.

**Main = MAIN-READY-85 `bdd36d06`** (s536e + s536h merged this session) + records; CODE sha `4a39a1c7`, generation v2-13300; gate 369 / 11,998; sweep 18,795; EVERYDAY 116 of 122.  Pushed.  Heavy slots: THREE.  The USER (s537): three subtasks at a time, Opus 5.5 at xhigh.

## s536i -- `W=/home/bernt/pcl/.claude/worktrees/agent-a1028de06f5b0a914` -- v2-13400
* HEAD `88108e97`, 13 commits over main `28c13bca` (= MAIN-READY-85 + records), tree CLEAN.  #4741 (`_subscript_form`) is FINISHED and committed with its guard rows and records.
* **VALID on `88108e97`:** `sweep.log` (END rc=0 01:16: TOTAL 18,795 +0, GATE clean).
* **VOID (killed 01:17):** `gate.log`, `ir.log`, `ab.log` (corpus-diff + wide A/B vs `7284c977`).
* OWED: the FULL gate, ir (host-leak + conform + the YAML-PP one-file run), `ab.sh` with every diff explained; then its STOP.md's "Owed after the legs" (verdict-register, bench leg + everyday `--record` LAST, session-log section, task DONE notes incl. #4741, the history row), MERGE-READY.  It is the nearest of the three.

## s536j -- `W=/home/bernt/pcl/.claude/worktrees/agent-a0264c6384ce7096d` -- v2-13500
* HEAD `094108ec`, 5 commits over main `28c13bca`, tree CLEAN.  Its STOP.md "RESUMED s537" section is current to about 01:0x and is the recipe (the REAL gate script is `gate2.sh`: an extraction OUTSIDE any git tree, because of #4763).  PPI clone `scratch/s536j/PPI` branch `pcl-s536j` = `c2f21706`, NOT pushed to the bare repo (push at MERGE-READY only; never the GitHub fork).
* Logs with an END line at the stop: `final.log` (01:16), `ab-final.log` (01:07), `sweep.log` inside final (18,795 +0, GATE clean), `install.log` (PASS).  **Which HEAD each measured is for the resuming agent to establish from STOP.md and the log headers** -- two commits (`vendor/PPI` re-stamp, packaging) landed around them; a records / stamp-only commit re-takes nothing, a `vendor/PPI/lib` or `Pl/` change does.
* OWED at least: the FULL gate through `gate2.sh` on the final HEAD (it was stopped before it started), then the rest of its STOP.md's list (tag-license, everyday bench + `--record` LAST, the bare-repo push at MERGE-READY, the format-patch series, PR-AV..AX.md).  The reviewing session runs `prove tools/t/install-container.t` at the merge.

## s537a -- `W=/home/bernt/pcl/.claude/worktrees/agent-a0bbdf9eda757fb47` -- v2-13600 -- brief `s537/s537a-prompt.md`
* HEAD `1749c555`, 3 commits over main `7284c977` (2 records commits behind: rebase first).  Members 1-3 COMMITTED: #4782 (`3e323d6d`), #4281 (`430d2a75`; an EMISSION CHANGE at every `until` loop -- `p-until` its own head; residue filed #4801), #4762 (`1749c555`).
* **UNCOMMITTED, UNVERIFIED:** member 4 (#4703) in `Pl/PExpr.pm`, `Pl/Parser2.pm`, `Pl/t/runtime-name-hygiene-01.t`; its targeted gate `tg4.log` was killed mid-run (VOID).  Read the diff, re-run the targeted gate, then commit or drop.
* OWED: members 4-6 (#4703, #4664 + #4325, #4264), then every bar of the brief ("Owed: everything" in its STOP.md).  Tasks #4801 #4802 were filed by it (in the store, committed by the reviewing session).

## The ROUND BOARD (mine) -- `~/pcl-agent-scratch/s537/round/` on main `7284c977`
* Chunk **a** (the 13 small dists) is DONE and valid: `board-a.tsv`, `board-a.rows.tsv` (164 failing rows), `board-a.log` END rc=0 01:05.  Chunk **b** was killed (VOID); c-f never started.
* Resume: edit `round.sh`'s loop to `b c d e f` and run it in the background (each chunk is its own bounded leg).  The extraction `main-tree` is at `7284c977`; if main has moved in CODE by then, a board on -85 is still the round's measurement -- finish it on the same extraction, then read `--diff` (recipe in PAUSE-s537.md "ROUND BOARD QUEUED").
* Still owed after it: the companion `--all --quick` (last on MAIN-READY-63).

## Next launches
* Relaunch s536i, s536j, s537a in their EXISTING worktrees (no isolation), model opus / effort xhigh, with this file + the two older resume files + their STOP.md + their briefs.  First MERGE-READY merges first (rule 25).
* `s537/s537b-prompt.md` (state containers + the container span mangle; IDs 4821-4839; v2-13700) is written and NOT launched: it takes the first freed slot (a NEW isolated worktree).
