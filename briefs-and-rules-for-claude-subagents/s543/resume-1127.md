# s543 RESUME of 2026-11-27, evening (the reviewing session, 21:3x) -- s543n, s543m, s543l finish in their worktrees, STACKED in merge order n -> m -> l

You are a RESUMED batch.  Your brief is unchanged (`briefs-and-rules-for-claude-subagents/s543/<label>-prompt.md`), the rulebook
is unchanged (`briefs-and-rules-for-claude-subagents/s473/COMMON.md`, HEAVY SLOTS (1)-(7) and "ORDINARY OR RARE" included), your
predecessor's handover is `scratch/<label>/STOP.md` in your worktree + its WIP commits.  Read those three, then
`s543/resume-s543.md` (the box rules, the END-line rule, the crash lesson about the cache) and `s543/resume-1122b.md` (how a
resume and a STACKED merge work: the `REBASED` file, a base that is not main, `--record` only at your own merge turn), then this
file.  Where they differ, this file wins.

| label | worktree `W` (under `/home/bernt/pcl/.claude/worktrees/`) | HEAD at the stop | generation | task IDs | merges |
|---|---|---|---|---|---|
| s543n | `agent-a1fb5119fa665c62c` | `572a46dc` (code `0d5a13e9`), clean | **v2-16300** (kept) | 5420-5439 | FIRST = MAIN-READY-111 |
| s543m | `agent-ac39a725d1d338842` | `bfa405a5` (code `d6b84d2f`), clean | **v2-16400** (WAS v2-16200: renumbered, below) | 5400-5419 | SECOND, rebased onto s543n's `REBASED` sha |
| s543l | `agent-a7d0dcd17f38738f0` | `e3591625` (code `f7706b9a`), clean | **v2-16500** (WAS v2-16100: renumbered, below) | 5380-5399 | THIRD, rebased onto s543m's `REBASED` sha |

You were started in the main checkout, NOT in your worktree: `cd "$W"` first and use absolute paths.  `/home/bernt/pcl` (main) is
read-only for you; tasks go through `~/.claude/tasks/pcl`.  Do NOT create a new worktree.  Do not spawn subagents.  Append a line
`resumed 2026-11-27 <time> <your model id>` to `$W/scratch/<label>/MODEL.txt` before anything else (execution agents are
`claude-opus-5-5`; anything else: write it, stop, report only that).

## What is different from the stop order of 2026-11-25 00:1x

- **Three days passed; the box was shut down in order and booted today at 21:24** (no crash: the previous boot's journal ends
  in systemd's shutdown; no cache file is newer than the stop and none except the `.lock` files is empty).  `/tmp` is EMPTY
  (anything you kept there is gone).  Nothing ran when you were started; `HEAVY.holder` is empty; everything in `HEAVY.queue`
  above the `s543 RESUMED 2026-11-27` marker line is stale text.  The cores under `~/.pcl-cache/core/` and your scratch
  extractions are as you left them.
- **Main moved in RECORDS only**: `09e60830` -> `17f6420f` + this file's commit (docs, briefs, tasks, memory; CODE is still
  MAIN-READY-110 `26d0dac4`'s, v2-16000).  CI is green on `17f6420f`.
- **THREE batches run at a time (the USER's word of 2026-11-27 21:3x, for this session)**: s543n now, s543m as soon as
  `scratch/s543n/REBASED` exists, s543l as soon as `scratch/s543m/REBASED` exists.  `heavy.sh` is the FOUR-SLOT script.  A batch has at most TWO bounded legs queued or running
  at a time; ONE exclusive leg (`bench` kind, everyday, the timing leg) queued on the box at a time -- read `HEAVY.holder` and
  the tail of `HEAVY.queue` first (rules (6) and (7)).  `chmod +x SCRIPT && bash -n SCRIPT` before a leg is queued.  Never
  `nohup` a leg (s543n's F-recheck went through `nohup` by mistake: not again).
- **GENERATIONS (the rulebook: "renumber ABOVE main's current string if main moved past yours").**  The merge order n -> m -> l
  runs AGAINST the numbers reserved at the launches (n 16300, m 16200, l 16100), so after s543n's merge main is at v2-16300 and
  both later batches are below it.  Ruled now, so nobody renumbers twice: **s543m takes v2-16400, s543l takes v2-16500**, each
  at its rebase, in the same commit as the three regenerated artifacts.
- **The reviewing session does not commit on main between s543n's rebase and its merge.**  The reviews of s543m and s543l are
  done by the reviewing session while you run (probes perl -> main -> your tree at the stop sha); a finding reaches you as a
  message.
- **The ORDINARY-OR-RARE ruling stands** (COMMON.md, last section): a neighbour your probes find is FILED with `frequency` +
  `frequency_reason`, it does not extend the batch unless it is ordinary.  Nothing new is begun in a resume: the OWED list and
  what a re-taken bar forces, no more.

## s543n (FIRST)
- FIRST ACTION `git -C "$W" rebase main` (alone in its command, no pipe after it; check its exit status; a records conflict in
  `docs/DECIDED.md` / `docs/session-log.md` / a baseline: keep BOTH sides).  It moves no code and voids no log: `git diff main
  --stat -- Pl cl lib vendor tools` must show exactly your batch (14 files, `cl/pcl-runtime.lisp` + `lib/autodie.pm` + three
  `Pl/*.pm` + six `Pl/t` files + the three artifacts).  Then the targeted gate ONCE as a bounded leg (`Pl/t/glob-defined-01.t
  diamond-01.t open-modes-01.t fileio-02.t match-vars-01.t system-block-01.t autodie-01.t artifact-staleness-01.t`: it proves
  the worktree's core still builds and runs after three days), and write the new HEAD's sha as the only line of
  `$W/scratch/s543n/REBASED` -- **s543m is rebased onto that sha.  From then on you commit RECORDS only**; if a CODE change turns
  out to be needed: make it, re-take what it voids, and put `CODE MOVED AFTER REBASED: <sha> <why>` on line 2 of STOP.md and
  first in your report.
- Then STOP.md's "BARS OWED" 2-5, in its order, nothing added and nothing dropped: the exclusive TIMING leg (`legs/bench.sh`,
  ten minutes at most; queue it AT ONCE after `REBASED` is written -- the box is quiet now and will not be in twenty minutes),
  the bench rows into `docs/session-log.md` `## Session s543n` (FLAG every row outside the noise band of
  `docs/caller-implementation.md` "What it costs"; name b6's two numbers), `tools/task-triage.pl` AFTER,
  `tools/everyday-smoke.pl --record` LAST on the clean committed tree, `MERGE-READY: <sha>` as the first line of STOP.md.
- **RULED (your flag, #3451): the remembered-path route is NOT platform-touching.**  It adds no foreign call and no OS
  interface: `sb-posix:fstat`, `stat-dev` and `stat-ino` are in the runtime on the base already (`%p-stat-try`, the size and
  regular-file reads).  No install-matrix or macOS leg is owed for it; say so in one line of the session-log section.

## s543m (SECOND; you are started when `scratch/s543n/REBASED` exists -- the sha is in your start message)
- FIRST ACTION `git -C "$W" rebase <that sha>` (alone; check its exit status): it is main's head + s543n's CODE.  **Renumber to
  v2-16400**, regenerate the three artifacts, `sbcl --script tools/check-parens.lisp` on every `.lisp` you touched, re-extract
  `scratch/s543m/base` from THAT sha -- it is the BASE of every bar from now on.  Expect textual meetings in
  `cl/pcl-runtime.lisp` only where a hunk is adjacent: s543n changed `%p-resolve-fh`, `p-get-stream`, `%p-install-fh`,
  `%p-forget-fh`, `%p-close-previous-stream`, `%p-close-impl`, `%p-argv-advance`, `%p-exec-become`, the heads of
  `%p-open-impl` / `%p-opendir-impl`, `%p-dh-key`, `p-utime`, `p-select`'s name function, `p-method-call`'s handle-name arm,
  `%p-fh-place-check`, `%p-glob-io-key`, `%p-fh-arg`; in `Pl/` `_fh_sym`, `_bareword_fh_p`, `_readline_bareword_name_p`
  (`Pl/Environment.pm`, `Pl/ExprToCL.pm`, `Pl/PExpr.pm`).  Your `p-scalar` handle arm (member 4) and the drop flush (member 7)
  work on handle objects: s543n did NOT change what a handle object is, but it changed WHERE a name's handle is found -- after
  the rebase re-run your member 4 and member 7 probes (perl -> the new base -> tree) BEFORE any leg, and `Pl/t/fh-collect*.t`
  with the targeted gate.  `docs/ir-op-inventory.{tsv,md}` is regenerated on the rebased tree (the third gate's two rows).
- As soon as the rebase, the renumbering and the artifacts are committed and the targeted gate (`Pl/t/requalify-binder-01.t
  eval-capture-01.t return-copy-01.t symref-package-01.t prefix-incr-deref-01.t regex-interp-01.t fh-collect*.t
  ir-inventory-01.t artifact-staleness-01.t` + s543n's six guard files above) is green, write that HEAD's sha as the only line
  of `$W/scratch/s543m/REBASED` -- **s543l is rebased onto it; from then on RECORDS only**, with the same `CODE MOVED AFTER
  REBASED` rule.
- Then STOP.md's "Bars OWED" 2-5 in full on the rebased tree: the FULL gate (it must end `Result: PASS`: the last one failed
  two `ir-inventory-01.t` rows), the FULL sweep, the wide A/B against the sha you were rebased onto (if the DIFF set is the same
  304 files by `tools/abattr.pl`, instr / gate-set scan / companion stand; if it is not, say which files and re-take what they
  void), EVERYDAY (exclusive; BEFORE = the new base, AFTER twice, read the warm run), the timing leg (exclusive, ~9 min; **FLAG
  every number of member 7 and #5402**: the sub-tail flush is IN unless its cost is outside noise, and the reviewing session
  decides that from your numbers), the session-log's `BARS_LINE`.  `--record` is NOT run before your own merge turn: after
  s543n's merge you get ONE message "main is now <sha>" -- `git rebase <sha>` is then a records-only move and voids nothing;
  `--record` LAST on that tree, then `MERGE-READY: <sha>`.
- The task notes cite PRE-REBASE shas: at MERGE-READY append ONE line per fixed task with the final sha (do not rewrite the
  notes).

## s543l (THIRD; you are started when `scratch/s543m/REBASED` exists -- the sha is in your start message)
- FIRST ACTION `git -C "$W" rebase <that sha>` (alone; check its exit status): main's head + s543n + s543m.  **Renumber to
  v2-16500**, regenerate the three artifacts, check-parens, re-extract `scratch/s543l/base` from THAT sha (the BASE of every
  bar).  EXPECT CONFLICTS as your STOP.md's OWED 1 lists them: `cl/pcl-runtime.lisp` `%p-read-record` and the read helpers
  (s543n's region; s543m's member 4 touches `p-scalar` and the frame exits, member 7 adds `p-flush-dropped-store`) -- keep BOTH
  changes: yours is only the `((or p-exception p-die-error) (e) (error e))` arm in each of the eight arms of your rule-12
  list, plus `p-local-store` and `%p-inc-elem-local`; `docs/DECIDED.md` and `docs/ir-spec.md` §8 (all three batches write
  there: keep all, newest first); `docs/ir-op-inventory.*` (regenerate, do not merge by hand).
- Then STOP.md's "BARS OWED" 2-8 in its order on the rebased tree (the m6 / m7 re-probe FIRST: 18 of 18 blocking primitives
  must still be perl's with s543n's read path under them; the full gate; the full sweep + ir-conform; corpus-diff + the wide
  A/B against the sha you were rebased onto -- the DIFF set must equal your 16; the companion AFTER with its BEFORE re-taken
  on an extraction of the NEW base for every `io/` file s543n moved (io/argv.t, io/pipe.t, op/fh.t, op/magic.t) and re/pat.t
  (s543m); everyday; the timing leg, l7's number named; the session-log section).  `--record` only at your own merge turn,
  after "main is now <sha>" (you get the message twice: after s543n's merge and after s543m's; each rebase is records-only),
  then `MERGE-READY: <sha>`.
- With three batches on the box the exclusive legs queue: yours come LAST of the three batches' (s543n's timing leg, then
  s543m's two, then yours); if your exclusive leg has waited more than 15 minutes, write the `waiting since` time and the
  holders into STOP.md and go on with what needs no SBCL.

## At MERGE-READY
`MERGE-READY: <sha>` as the first line of STOP.md.  After the report your worktree stays as it is.

## Final report (SHORT; your last message)
`MERGE-READY: <sha>` first (or `NOT MERGE-READY` + reason); model id; per member one line; every bar QUOTED with its log's name
and the base sha it compared against (full gate files / rows, sweep TOTAL and GATE line, the wide A/B's SAME / DIFF / RCDIFF with
each DIFF's cause, corpus-diff's SILENT-DROP line, gate-set scan, drop census, ir-conform, ir-host-leak, the companion BEFORE ->
AFTER per moved file, EVERYDAY base -> tree, the timing rows with every FLAGGED number); the ordinary count BEFORE -> AFTER
(`tools/task-triage.pl`); tasks filed / appended with their class word and `frequency`; the CLOSE-WITH-ROWS candidates; the
review points the reviewing session should probe; HEAD sha, the commit count over the sha you were rebased onto; the CLAUDE.md /
REFERENCE.md pointers the reviewing session should apply; anything you could NOT do.
