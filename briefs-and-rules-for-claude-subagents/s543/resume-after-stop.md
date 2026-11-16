# s543 -- HAND-OVER at the session's end (2026-11-16 23:3x; the USER ended the session) -- NEXT ACTION for the next reviewing session

Nothing runs.  `HEAVY.holder` is empty, no SBCL.  Main = `origin/main`, records only since MAIN-READY-98 (CODE `c5f618f2`, v2-15000);
gate on main 379 files / 12,303 rows PASS (`~/pcl-agent-scratch/s543/review/gate-c0bc5e88.log`).  Read `s543/PAUSE-s543.md` bottom-up
for the day; `s543/resume-s543.md` for what the 22:34 crash did (its cache lesson holds for any later crash).

## The two batches: both stopped ON ORDER at a clean checkpoint, both close to MERGE-READY

| label | worktree | HEAD | over main | generation | what is left |
|---|---|---|---|---|---|
| s543a (line tracking on by default) | `.claude/worktrees/agent-a9d6af55959b1f0c0` | `bd8c819d` (WIP 13), clean | 13 | v2-15100 | the three EXCLUSIVE timing legs (bench noise, bench tree, first run) one at a time -> the cost placeholders (caller-implementation, supported-subset 6b, DECIDED, session-log, tasks) + #5103 filed; then everyday `--record` (115 -> 116 expected); task DONE notes; MERGE-READY |
| s543b (seven silent wrongs) | `.claude/worktrees/agent-aca67256b5619cf0d` | `f99be16c`, clean | 14 | v2-15200 | re-run the three artifact commands on the final tree; companion one-file BEFORE (op/chdir.t op/readdir.t) -> AFTER (12 files), movers spliced; everyday base -> tree; the inverse leg; the FULL gate; the session-log Bars line; MERGE-READY |

* Each batch's `scratch/<label>/STOP.md` is current to the stop order: valid logs (END line), void logs, the exact next steps with their
  scripts.  RESUME = a new Opus 5.5 / xhigh agent per batch IN THE SAME WORKTREE (no new worktree), the prompt of `s543/resume-s543.md`'s
  kind ("read STOP.md, the brief, the WIP commits; finish the OWED list"); MODEL.txt gets a `resumed` line -- check it.
* **s543a's bars already met** (logs with END lines): full gate 379 files / 12,309 rows PASS; full sweep GATE clean, TOTAL 18,803 -> 18,808,
  drops 5 = census; gate-set scan base = tree (638 files); ir-conform 335 / 0 / 10 known / 0 stale; ir-host-leak 31; companion 10 files
  ROW DIFF 0; install-pcl.t 61 rows; the emission bar 2,053 files 0 DIFF both ways.  It FOUND AND FIXED one bug of its own (a string eval
  that fails to compile reported the caller's line; `p-eval` starts the line register at 0).  Board: 16 rows leave, 55 Log4perl rows
  re-noted under the new #5106.  Its timing legs want a QUIET box -- run them first, alone.
* **s543b's bars already met**: all seven members' code committed (member 7 #4796 = `Binder::shadowed_capture`, measured, 9 guard rows
  fail on the base) + two regressions of its own found by its sweep and fixed (WIP 8: `chdir $dh`, a `local` tail's context); sweep on the
  final code TOTAL 18,803 +0, GATE clean, drops 5; ir-conform 328 / 0; gate-set scan diff empty; corpus-diff 15 of 111 explained,
  SILENT-DROP 5; wide A/B RCDIFF 0 (8 files' attribution owed a diff by eye).  Filed #5120-#5126 (#5126: `our` in an eval, SILENT WRONG).
* **Merge order**: first MERGE-READY first; BOTH change emission, so the second rebases, keeps its generation (s543b renumbers only if
  main passed it), regenerates the three artifacts and re-takes the gate + corpus-diff / emission-ab against the new main (+ the sweep:
  s543a touches every file's emission, s543b name resolution).  s543b's guard rows must not depend on the absence of `(p-line N)`.
* **Review probes owed at each merge** (perl -> BASE -> tree): s543a -- `caller` / `croak` / a runtime die in a module, a string eval,
  a BEGIN block (#5101 is filed residue), `PCL_OPT=-line-track` and `=none`, a warm vs cold module cache; s543b -- each member's
  breaking shape (STOP.md names them), the `p-dirhandle` struct in `ref` / stringify / `chdir`, the `local` tail's value in list and
  scalar context, `undef $fh` cost, eval `my $x = $x` at two sites with and without an outer `$x` (the eval cache key).

## The third brief is WRITTEN, NOT LAUNCHED: `s543/s543c-prompt.md` (the VERIFICATION batch)
139 tasks (`s543/s543c-tasks.tsv`); no compiler / runtime change; IDs 5140-5159.  Launch it in a NEW isolated worktree when a slot is
free (two subtasks at a time unless the USER says otherwise).  It does not need the two merges first, but its verdicts name the CODE sha.

## Owed by the reviewing session
1. Resume s543a + s543b, review, merge, push (CLAUDE.md "Test Status" + `docs/STATUS.md` + HISTORY.md at each merge).
2. **Triage fields for the tasks the batches own** -- rows exist in `s543/triage-s543.tsv` for the 22 listed in PAUSE-s543.md; #5103
   (when filed), #5106, #5126 and anything later have NO row yet: classify at the merge (`tools/task-triage.pl --list untriaged`).
3. The ROUND for MAIN-READY-96..-98 + this session's merges (the board tree half + the companion `--all --quick` on merged main).
4. For the USER, when there is time: the 36 `decision` tasks (`tools/task-triage.pl --list decision`; 21 are constructs proposed for
   section 4 of `docs/supported-subset.md`), and 6c / 6d / 6e of that file.
5. After a crash: remove every `~/.pcl-cache` file written in the last minutes and every zero-size one BEFORE any leg (a task for a
   reader that treats a zero-size core / fasl / proto file as a miss is worth filing if it happens again).
