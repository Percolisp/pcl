# RESUME after the 21:23 memory kill (s536, the reviewing session, 2026-11-08 21:3x) -- four batches: s536e, s536h, s536i, s536j

**What happened.** At 21:23:21 `systemd-oomd` killed GNOME Shell for memory pressure (four heavy legs had started in the same second when an exclusive bench released its slots -- load 55 -- beside a browser that had peaked at 7.9 GB on this 12 GB machine).  The terminal, the reviewing session, every agent and every heavy leg died with it.  The box did NOT reboot: `/tmp`, the worktrees and `~/.pcl-cache` are intact.  Your earlier conversation is gone; your `STOP.md` plus this file are the whole handover.  Nothing committed was lost.

You are an EXECUTION agent (Opus 5.5) RESUMING your batch in its EXISTING worktree.  Do not spawn subagents.  Every command is `env -C "$W" CMD`, `git -C "$W" ...` or an absolute path.

## Rules for all four

1. **FIRST ACTION:** write the exact model id you run as into `$W/scratch/<label>/MODEL.txt` (the USER requires `claude-opus-5-5`; anything else: write it, stop, report only that).
2. **READ, in this order:** your section below; `$W/scratch/<label>/STOP.md` (your resume recipe; it was accurate at the kill except where your section corrects it); your original brief `briefs-and-rules-for-claude-subagents/s536/<label>-prompt.md`; `s536/SHARED-BOX.md` rules 19-40; `s473/COMMON.md`.
3. **A log is a RESULT only if its last line is `=== heavy.sh <label> ... END rc=...`.**  A log without that line was killed: it is VOID, whatever it says.  Your section names which logs are valid.  Do not re-take a valid leg; the tree it measured has not changed.
4. **Heavy slots are THREE again** (rule 40).  Legs go through `~/pcl-agent-scratch/s513/heavy.sh <label> leg|bench ABS-LOG CMD` in the BACKGROUND.  Expect waits.  An exit-75 leg is not a failure: re-queue it (up to twice), then report.  Never edit `Pl/` / `cl/` while a leg reads the tree.  Everyday is taken at a 1-minute load below 4 (rule 26; use the `bench` kind).
5. **Rebase first:** commit anything uncommitted that belongs to your batch, then `git -C "$W" rebase main`.  Main moved only by records since you last rebased (its CODE sha is still `3139fcb0`, generation v2-13100): a records-only move re-takes nothing.
6. **Nobody waits for a sibling** (rule 25).  First MERGE-READY merges first.  If the reviewing session tells you main moved in code, rebase, keep your reserved generation, regenerate the three artifacts if you have one, and re-take only what the move can change (a `cl/` move re-takes the gate and the sweep; a `Pl/` move also corpus-diff + the wide A/B).
7. **Task store:** tasks are read and written through `~/.claude/tasks/pcl` (the main checkout's copy).  Notes you wrote before the kill are already there, uncommitted in the main checkout; the reviewing session commits them.  Read a task before appending so a DONE note is not written twice.  Agents never set a task `completed`.
8. **Finish:** `MERGE-READY: <sha>` as the first line of `STOP.md`, and the FINAL REPORT: the bars BEFORE -> AFTER and which tree each measured, the HEAD sha, the `main..HEAD` count, anything not done.  Budget: checkpoint at ~200k tokens.

## s536e -- `W=/home/bernt/pcl/.claude/worktrees/agent-a7512e76832ceacb1` -- generation v2-13300

HEAD `68a75660`, the final code (the one-walk refactor of the rename rewrites).
- **VALID on this HEAD vs `3139fcb0`:** `cdiff2.log` (3 / 111: each.t magic.t my.t, as rounds 1 and 2), `ab-wide.log` (2,053 files, 11 DIFF, RCDIFF 0).
- **VOID:** `sweep.log` (killed at 98 of 108).
- **NEVER RAN** (still queued at the kill; no log exists): `refactor-check.sh`, `gate.sh`.
- **OWED:** `refactor-check.sh` (must be IDENTICAL vs `85a59b1a`), the FULL gate, the FULL sweep; then the rest of your STOP.md's "Owed after" (bench leg = everyday `--record` + compile time, the history row, the numbers in the session log and DECIDED), MERGE-READY.

## s536h -- `W=/home/bernt/pcl/.claude/worktrees/agent-a5077c5dee2338f64` -- `cl/` + one non-emitting accessor: no generation bump

HEAD `0d2c7d39` (`*p-eof-seen*` a plain `eq` table) **plus three uncommitted edits of yours**: `docs/DECIDED.md`, `docs/ir-spec.md`, `docs/session-log.md`.  Read them, keep them, finish the session-log Bars line and commit.
- **VALID:** `bench4.log` (END rc=0, 21:13, on `0d2c7d39`; read the fhread / fhprint rows base vs tree and state the result).  `corpus-diff3.log` (identical to `3139fcb0`) and `eab3.log` (2,053 SAME) stand: `0d2c7d39` changed `cl/` only.
- **SUPERSEDED by `0d2c7d39`:** `gate3.log` (369 / 11,983) and `sweep3.log` (18,793 +0) measured the commit before it.
- **NEVER RAN** (queued 21:14, killed while waiting): the FULL gate, the FULL sweep and ir-conform on the final tree.
- **OWED:** those three legs, the `comp2.log` reading if not already done, everyday `--record` LAST on the clean tree (`bench` kind), MERGE-READY.

## s536i -- `W=/home/bernt/pcl/.claude/worktrees/agent-a1028de06f5b0a914` -- generation v2-13400

HEAD `d7020373` = `835651db` (the reworked #4283, final code) + one baseline row by edit.  Based on main `a2fbe1bb`.
- **VALID on `835651db`:** `gate.log` (END rc=0 21:02, PASS 369 / 11,981), `ir.log` (ir-host-leak 31, ir-conform 328 / 0 / 17 / 0, YAML-PP 32.cyclic-refs.t 8 / 8).  Run `prove Pl/t/verdict-register-01.t` once for the baseline edit in `d7020373`.
- **VOID:** `sweep.log` + `sweep.out` (killed near 100 of 108).
- **NEVER RAN:** `ab.sh`.  There is no `ab.log`.  **`corpus-diff.out` and `eab-wide.out` on disk are from 20:34, the FIRST #4283 version -- not a result for this tree;** `ab.sh` overwrites them.
- **OWED:** `ab.sh` (corpus-diff + wide A/B vs `3139fcb0`; every diff explained), the FULL sweep, then your STOP.md's "Owed after the legs" (bench leg, session-log section updated for the rework, task DONE notes, history row), MERGE-READY.

## s536j -- `W=/home/bernt/pcl/.claude/worktrees/agent-a0264c6384ce7096d` -- generation v2-13500

HEAD `ea8b244c` (the 19b arm deleted: `ab-inst` showed it never fired beyond the Math::Complex diff you already explained -- confirm from `ab-inst.log` / `eab-all-inst.log`).  Based on main `a2fbe1bb`.  The catalogue §62-§65, the report twins, the PR texts and the artifacts at v2-13500 are committed.  The task notes `n*.txt` (#3694 #3697 #4302 #4482 #4542 #4701) were written at 21:11-21:12; read the tasks before you write them again.
- **VOID:** `gate-final.log`, `gate-final.out`, `final.log`, `corpus-final.log` (killed at 21:23, no END line).  `x-final` is a dead extraction; `gate.sh` recreates it.
- **NEVER RAN:** the final `ab.sh`, `board1.sh`.
- **OWED:** `gate.sh final <HEAD sha>`, `final.sh` (corpus-diff vs `3139fcb0`, expect AFTER, gate-SET AFTER, drop census, sweep, install-pcl.t, verdict register plain + STRICT), the wide A/B final, `board1.sh` (TT parser.t row 35 stays #4302; Math::Complex), then your STOP.md's "Owed" remainder: tag-license, everyday bench + `--record` LAST, push groups to the BARE repo's `pcl` at MERGE-READY only (never the GitHub fork), vendor from `pcl`, the format-patch series, PR-AV..AX.md.  **The rebase above is a records-only move; do not push before MERGE-READY.**
