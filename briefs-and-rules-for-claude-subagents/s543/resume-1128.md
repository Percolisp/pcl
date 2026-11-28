# s543 RESUME of 2026-11-28, afternoon (the reviewing session, 15:3x) -- s543t2 and s543x finish in their worktrees, STACKED in merge order t2 -> x

You are a RESUMED batch.  Your brief is unchanged (`scratch/<label>/prompt.md` in your worktree; s543x also
`briefs-and-rules-for-claude-subagents/s543/s543x-prompt.md`), the rulebook is unchanged
(`briefs-and-rules-for-claude-subagents/s473/COMMON.md`, HEAVY SLOTS (1)-(7) and "ORDINARY OR RARE" included), your
predecessor's handover is `scratch/<label>/STOP.md` in your worktree + its WIP commits.  Read those three, then
`s543/resume-s543.md` (the box rules, the END-line rule, the crash lesson about the cache) and `s543/resume-1122b.md` (how a
resume and a STACKED merge work: the `REBASED` file, a base that is not main, `--record` only at your own merge turn), then
this file.  Where they differ, this file wins.

| label | worktree `W` (under `/home/bernt/pcl/.claude/worktrees/`) | HEAD at the stop | generation | task IDs | merges |
|---|---|---|---|---|---|
| s543t2 | `agent-a086ae292a8fd6503` | `f67ea907` on `96bf714c`, clean | **v2-17300** (kept) | 5546-5559 | FIRST = MAIN-READY-119 |
| s543x | `agent-a19e17bde52967185` | `28253c74` on `96bf714c`, clean; PPI clone `scratch/s543x/PPI` branch `pcl-s543x` = `a8178635` | **v2-17350** (WAS v2-17200: renumbered, below) | 5620-5639 | SECOND, rebased onto s543t2's `REBASED` sha |

You were started in the main checkout, NOT in your worktree: `cd "$W"` first and use absolute paths.  `/home/bernt/pcl`
(main) is read-only for you; tasks go through `~/.claude/tasks/pcl`.  Do NOT create a new worktree.  Do not spawn subagents.
Append a line `resumed 2026-11-28 <time> <your model id>` to `$W/scratch/<label>/MODEL.txt` before anything else (execution
agents are `claude-opus-5-5`; anything else: write it, stop, report only that).

## What is different from the stop order of 10:5x

- **No crash**: the session was ended by the USER at 10:5x; the box was shut down in order at 15:19 and booted at 15:20 (the
  previous boot's journal ends in systemd's shutdown; no cache file is newer than 10:54).  `/tmp` is EMPTY.  Nothing ran when
  you were started; `HEAVY.holder` is empty; everything in `HEAVY.queue` above the `s543 RESUMED 2026-11-28` marker line is
  stale text.  The cores under `~/.pcl-cache/core/` and your scratch extractions are as you left them.
- **Main moved in RECORDS only**: `097db06f` -> `e07077b4` + this file's commit (docs, briefs, tasks, memory; CODE is still
  MAIN-READY-118 `96bf714c`'s, v2-17100).  The sha to rebase onto is in your start message.
- **TWO batches run (the USER's default).**  `heavy.sh` is the FOUR-SLOT script.  A batch has at most TWO bounded legs queued
  or running at a time; ONE exclusive leg (`bench` kind, everyday, the timing leg, the companion runner at `--jobs` > 1)
  queued on the box at a time -- read `HEAVY.holder` and the tail of `HEAVY.queue` first.  `chmod +x SCRIPT && bash -n SCRIPT`
  before a leg is queued.  Never `nohup` a leg.  12 GB: watch `free -g`.
- **The reviewing session does not commit on main between s543t2's rebase and its merge.**  Both reviews are done by the
  reviewing session while you run (probes perl -> main -> your tree); a finding reaches you as a message.
- **Nothing new is begun in a resume**: the OWED list and what a re-taken bar forces, no more.  A neighbour your probes find
  is FILED with `frequency` + `frequency_reason` (the ORDINARY-OR-RARE ruling).
- **A gate row that fails under load** is run alone three times AND the gate is re-run before it is called a flake; the
  gate's WHOLE output is kept in its log (`Pl/t/use-base-phase-01.t` row 2: #4904).

## s543t2 (FIRST)
- FIRST ACTION `git -C "$W" rebase <the sha of your start message>` (alone in its command, no pipe after it; check its exit
  status; a records conflict in `docs/DECIDED.md` / `docs/session-log.md`: keep BOTH sides, newest first).  It moves no code
  and voids no log: `git diff <sha> --stat -- Pl cl lib vendor tools` must show exactly your batch.  Then
  `prove Pl/t/match-scope-01.t Pl/t/parser2-02.t Pl/t/decl-facts-01.t Pl/t/artifact-staleness-01.t` ONCE as a bounded leg (it
  proves the worktree's core still builds and runs after the reboot), and write the new HEAD's sha as the only line of
  `$W/scratch/s543t2/REBASED` -- **s543x is rebased onto that sha.  From then on you commit RECORDS only**; if a CODE change
  turns out to be needed: make it, re-take what it voids, and put `CODE MOVED AFTER REBASED: <sha> <why>` on line 2 of
  STOP.md and first in your report.
- Then STOP.md's OWED 1-4 in its order, nothing added and nothing dropped: **THE TIMING LEG, queued AT ONCE after `REBASED`
  is written** (the box is quiet now and will not be in twenty minutes; `bench.sh`: pre `394a0012`, base `96bf714c`, tree;
  the bar is a sub WITHOUT a match back in the noise band against `394a0012`; say what a matching sub still pays, and FLAG
  every row outside the noise band of `docs/caller-implementation.md` "What it costs"), the session-log section and #5545's
  DONE note with the numbers, a warm everyday run, `tools/everyday-smoke.pl --record` LAST on the clean committed tree (it is
  your merge turn already: no "main is now" message comes before it), `MERGE-READY: <sha>` as the first line of STOP.md.
- In your report: the sentence for CLAUDE.md "Test Status" about what is left of "every sub call +3..8 %".

## s543x (SECOND; you are started when `scratch/s543t2/REBASED` exists -- the sha is in your start message)
- FIRST ACTION `git -C "$W" rebase <that sha>` (alone; check its exit status): it is main's head + s543t2's CODE (its
  functions: `Pl/Manifest.pm` `%MATCH_STATE_HEAD` + `_may_write_match_state`, `Pl/Parser2.pm` the `:needs` / `:match` fact and
  `_lower_embedded_anon`'s `(p-sub-frame :no-match ...)`, `cl/pcl-runtime.lisp` `p-sub-frame`, `%p-sub-facts-no-match-p`,
  `p-sub`; `Pl/t/match-scope-01.t`, two rows of `Pl/t/parser2-02.t`, one of `Pl/t/decl-facts-01.t`).  Expect conflicts in the
  generation line, the three artifacts and the records (`docs/DECIDED.md`, `docs/ir-spec.md`: keep both); none in the code
  you edited.  **Renumber to v2-17350** (ruled now, so nobody renumbers twice: s543t2 merges first at v2-17300; s543y and
  s543z hold 17400 and 17500), regenerate the three artifacts (`scratch/s543x/regen.sh`) in the same commit,
  `sbcl --script tools/check-parens.lisp cl/pcl-runtime.lisp`, extract `scratch/s543x/base3` from THAT sha -- **it is the BASE
  of every bar from now on**; say in each bar's line which sha was the base.
- **One probe BEFORE any leg (the two batches meet here):** s543t2 gives a sub's frame the capture scope only where its body
  can reach a match writer.  Your members change what a body lowers to.  For each shape you fixed that can hold a match --
  `sort $s =~ /(..)/g` (the `ExtUtils/Embed.pm` find), `print $fh +(/(a)(b)/)[0]`, `map split, LIST` and a `map`/`grep` whose
  EXPR is a match, `f state => $x =~ /(.)/` -- write a named sub and an anonymous sub whose ONLY match sits in that shape,
  call it after the caller matched, and print the caller's `$1` after the call: perl -> base3 -> tree must agree, and the
  emitted frame must NOT carry `:no-match`.  Put the probe file and its three outputs in `scratch/s543x/probes/`; a
  difference is a finding for both batches: report it at once (STOP.md line 2), do not fix `Pl/Manifest.pm` yourself.
- Then, on the stacked tree, in this order: the targeted gate (`scratch/s543x/tgate-list.txt` + `Pl/t/match-scope-01.t
  parser2-02.t decl-facts-01.t artifact-staleness-01.t vendor-ppi-01.t`); **the FULL gate** (the code moved under
  `gate-full-1.log`: re-take; expect s543t2's 381 files + your rows, `Result: PASS`); corpus-diff + the lib A/B against that
  sha (expect IDENTICAL / SAME); **the wide A/B against that sha: the DIFF set must be the same nine files** (name any other
  with its cause); the gate-set scan and the drop census (cheap); then STOP.md's "BARS OWED" 2 and 4-8 in full: the FULL sweep
  (TOTAL = 18,809 + rows named by edit, GATE clean, drops <= the census) + ir-conform + ir-host-leak, the companion BEFORE on
  `base3` and AFTER on the tree, ROSETTA (expect 596, buckets 0), EVERYDAY (exclusive; read the SECOND run), the bench leg
  (exclusive, <= 10 min; FLAG every row outside the noise band -- `sort $x LIST`, a `print $fh` row and a `map` row are the
  ones your predicates sit on), the records (session-log `## Session s543x` above `## Session s543t2`, the plan's line, the
  DONE notes, `tools/task-triage.pl` AFTER).
- **The PPI half** (STOP.md "PPI"): at MERGE-READY fast-forward the bare mirror `~/pcl-agent-scratch/ppi/PPI.git` `pcl`
  `eca93377` -> `a8178635` (a fast-forward only: check `git merge-base --is-ancestor`), then `tools/vendor-ppi <clone> pcl`
  so the stamp says `branch: pcl`, `prove Pl/t/vendor-ppi-01.t` and the canary `PCL_SYSTEM_PPI=1 prove
  Pl/t/ppi-bugs-registry-01.t`; PPI's own suite green on the clone is quoted in the report.  Never push to GitHub.
- `--record` is NOT run before your own merge turn: after s543t2's merge you get ONE message "main is now <sha>" --
  `git rebase <sha>` is then a records-only move and voids nothing; a warm everyday run, `--record` LAST on that tree, then
  `MERGE-READY: <sha>` as the first line of STOP.md.  If you are done before the message comes: write `WAITING FOR MAIN` as
  STOP.md's first line and end your turn with the report so far; the message continues you.
- The `Hub.t` find (`tests state => sub {...}`): STOP.md says the dist runner's invocation was wrong on both trees.  Find the
  right one (`tools/run-dist-t.pl --rows Test-Simple t/Test2/modules/Hub.t`; the dist names are in
  `baselines/cpan-board-dists.tsv`) and quote BEFORE -> AFTER rows; if the dist is not on the board, say so in one line.

## At MERGE-READY
`MERGE-READY: <sha>` as the first line of STOP.md.  After the report your worktree stays as it is.

## Final report (SHORT; your last message)
`MERGE-READY: <sha>` first (or `NOT MERGE-READY` + reason); model id; per member one line; every bar QUOTED with its log's
name and the base sha it compared against (full gate files / rows, sweep TOTAL and GATE line, the wide A/B's SAME / DIFF /
RCDIFF with each DIFF's cause, corpus-diff's SILENT-DROP line, gate-set scan, drop census, ir-conform, ir-host-leak, the
companion BEFORE -> AFTER per moved file, ROSETTA, EVERYDAY base -> tree, the timing rows with every FLAGGED number); the
ordinary count BEFORE -> AFTER (`tools/task-triage.pl`); tasks filed / appended with their class word and `frequency`; the
CLOSE-WITH-ROWS candidates; the review points the reviewing session should probe; HEAD sha, the commit count over the sha
you were rebased onto; the CLAUDE.md / REFERENCE.md pointers the reviewing session should apply; anything you could NOT do.
