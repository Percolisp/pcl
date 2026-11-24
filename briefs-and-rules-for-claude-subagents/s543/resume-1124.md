# s543 RESUME of 2026-11-24, evening (the reviewing session, 20:2x) -- s543k and the round s543s finish in their worktrees; s543k merges FIRST, the round's full companion run is taken on the merged main AFTER it

You are a RESUMED batch.  Your brief is unchanged (`briefs-and-rules-for-claude-subagents/s543/<label>-prompt.md`), the rulebook
is unchanged (`briefs-and-rules-for-claude-subagents/s473/COMMON.md`, HEAVY SLOTS (1)-(7) included), your predecessor's
handover is `scratch/<label>/STOP.md` in your worktree + its WIP commits.  Read those three, then `s543/resume-s543.md` (the box
rules, the END-line rule, the crash lesson about the cache) and `s543/resume-1122b.md` (how a resume and a stacked merge work),
then this file.  Where they differ, this file wins.

| label | worktree `W` (under `/home/bernt/pcl/.claude/worktrees/`) | HEAD at the stop | generation | task IDs | merges |
|---|---|---|---|---|---|
| s543k | `agent-adecfff7d8bd8bed7` | `3b20d5ab`, clean, on `cbcc9165` (MAIN-READY-109) | v2-16000 | 5350-5369 | FIRST = MAIN-READY-110 |
| s543s (THE ROUND) | `agent-a4c3c56a4ac05669b` | `7cc32739` = `f2873358` + 2 baseline commits, clean | none | 5310-5329 | SECOND, records only |

You were started in the main checkout, NOT in your worktree: `cd "$W"` first and use absolute paths.  `/home/bernt/pcl` (main) is
read-only for you; tasks go through `~/.claude/tasks/pcl`.  Do NOT create a new worktree.  Do not spawn subagents.  Append a line
`resumed 2026-11-24 <time> <your model id>` to `$W/scratch/<label>/MODEL.txt` before anything else (execution agents are
`claude-opus-5-5`; anything else: write it, stop, report only that).

## What is different from the stop order of 2026-11-23 01:4x

- **Two days passed; the box was shut down in order at 01:50 and booted today at 20:11** (no crash: the previous boot's journal
  ends in systemd's shutdown; no cache file is newer than the stop and none except the `.lock` files is empty).  `/tmp` is EMPTY
  (anything you kept there is gone; `git worktree prune` is the reviewing session's).  Nothing ran when you were started;
  `HEAVY.holder` is empty; everything in `HEAVY.queue` above the `01:47 s543 ENDED` marker line is stale text.  The cores under
  `~/.pcl-cache/core/` and your scratch extractions are as you left them.
- **Main moved in RECORDS only**: `f2873358` -> `8ee15299` + this file's commit (docs, briefs, tasks, memory; CODE is still
  `cbcc9165`'s, v2-15900).  CI is green on `8ee15299` (both workflows).
- **THE USER (2026-11-24 20:1x): both batches are resumed.**  The box is ours: `heavy.sh` is the FOUR-SLOT script.  A batch has
  at most TWO bounded legs queued or running at a time; ONE exclusive leg queued on the box at a time (read `HEAVY.holder` and
  the tail of `HEAVY.queue` first; rules (6) and (7)).  `chmod +x SCRIPT && bash -n SCRIPT` before a leg is queued.
- **The reviewing session does not commit on main between s543k's rebase and its MERGE-READY.**

## s543k
- FIRST ACTION `git -C "$W" rebase main` (alone in its command, no pipe after it; check its exit status; a records conflict in
  `docs/DECIDED.md` / `docs/session-log.md`: keep BOTH sides).  It moves no code and voids no log: `git diff main --stat -- Pl cl
  lib vendor tools` must show exactly your batch.
- Then STOP.md's "Bars OWED" list, items 2-5, in its order, nothing added and nothing dropped: the drop census + ir-conform; the
  SECOND full gate on the final tree (it must end `Result: PASS`: the first failed two `ir-inventory-01.t` rows, since
  regenerated); the companion BEFORE / AFTER (bounded, `--jobs 1`), movers spliced by hand with their cause; then the exclusive
  legs ONE AT A TIME -- everyday (BEFORE base, AFTER twice, read the warm run), then the timing leg (ten minutes at most;
  #5272's large-region row ONCE per side) -- and between them look for a waiting leg of the round; the session-log section;
  `--record` LAST on the clean committed tree; `MERGE-READY: <sha>` as the first line of STOP.md.
- If a CODE change turns out to be needed: make it, re-take what it voids, and put `CODE MOVED: <sha> <why>` on line 2 of STOP.md
  and first in your report.

## s543s (the round)
- FIRST ACTION `git -C "$W" rebase main` (records only; your baseline rows and s543j's both kept).
- **RULING (the reviewing session, 2026-11-24): the board and the `--all --quick` run STAND as taken on MAIN-READY-108
  `4a0fd0ec`; THE FULL COMPANION RUN IS TAKEN ON THE MERGED MAIN AFTER s543k's MERGE (MAIN-READY-110), not on -108.**  Reasons:
  the full run is exclusive and long, so nothing of s543k may wait behind it; and a blessed stamp of a tree two merges behind
  main would be stale on the day it is written.  The full run then also checks s543j's and s543k's hand-edited companion rows.
- **PHASE 1, now, bounded legs only (`HEAVY_BOUNDED=1`, `--jobs 1`, `timeout 120` per one-file run), while s543k takes its bars:**
  1. STOP.md OWED 1: reduce the six NEW rows of `op/decl-refs.t` (moved at MAIN-READY-107, s543h).  The probes cut out of the
     file do not reach the move (the `state` iteration refuses on -106 too), so do not cut: BISECT THE FILE -- run the whole
     file on `round2/trees/MR-106` and `MR-107`, diff the two TAP streams row by row around the six rows, then shrink the FILE
     from the end and from the front (a copy under `round2/`, never `perl-tests/` or the companion tree) while the six rows
     still differ between the two trees; the aim is a reproducer of at most ~15 lines that differs perl -> MR-106 -> MR-107,
     or the plain statement of what the six rows were passing ON before (the standing rule: a fix that makes values real exposes
     rows that were passing on nothing -- a `state \($b, $c)` that refused on -106 can have left the test comparing two
     undefs).  If they PASSED ON NOTHING before: say so with the two values the row compared on each tree, key the rows on the
     task that owns the refusal (grep the task store for `state` + `refs` / `declared_refs`; `docs/not-supported.md`), no new
     task.  If they are a real regression: FILE it (5311-5329, the class word first, `triage` + `triage_reason`, the reproducer
     perl -> MR-106 -> MR-107) -- it goes FIRST into the next fix brief beside #5310.  Either way: the six rows into
     `perl-suite-fails.tsv` by edit with that key, the snapshot row `op/decl-refs.t` 74/151 -> 68/157, the 2 LOST explained.
  2. The record DRAFTS that need no SBCL (STOP.md OWED 3), with the full run's lines left open.
  3. Then write `WAITING FOR MAIN-READY-110` as the first line of STOP.md, commit your baseline edit as a WIP commit, and END
     YOUR TURN with a short interim report (the decl-refs finding first, with the reproducer or the two values).  Do not poll,
     do not wait in a loop: you are continued with ONE message "main is now <sha>".
- **PHASE 2, on that message:** `git -C "$W" rebase <sha>` (records only on your side; s543k's companion rows and yours both
  kept); extract `git archive <your rebased HEAD>` to a fresh directory under `round2/` (it holds main's code AND every edited
  baseline, so `comp-edits.patch` is not needed: adapt `comp-full.sh` to that extraction and re-read every path in it); queue
  the FULL companion `PCL_SESSION=s543s tools/run-perl-suite.pl --all --jobs 4 --bless-stamps` as the ONE exclusive leg on the
  box (expected 45-60 minutes: write the expected end into STOP.md).  The target is **0 NEW / 0 FIXED / 0 LOST against the
  edited baselines**.  A mover is attributed with one-file runs on `round2/main-tree` (-108), an extraction of `cbcc9165`
  (-109) and the new main (-110): s543j's and s543k's own companion movers are already spliced by them (read their session-log
  sections for the list) -- a row they did not name is a finding, filed or keyed with its cause.  Then the blessed stamps into
  the worktree by edit, the records (STATUS, session-log `## Session s543s`, DECIDED `## s543s`, the board review's dated line:
  the board and `--quick` numbers "as measured on MAIN-READY-108", the full run "on MAIN-READY-110"), the bars of STOP.md OWED 4,
  the #5163 number, and `MERGE-READY: <sha>` as the first line of STOP.md.
- You change baselines, docs and task files only -- never `Pl/`, `cl/`, `lib/`, `vendor/`, `tools/` or a test.

## Final report (SHORT; your last message)
`MERGE-READY: <sha>` first (or `NOT MERGE-READY` + reason; the round's phase 1 ends with `WAITING FOR MAIN-READY-110`); model
id; s543k: per member one line and every bar QUOTED with its log's name and the base sha it compared against (the second full
gate's files / rows, drop census, ir-conform, the companion BEFORE -> AFTER per file, EVERYDAY base -> tree, the timing rows with
#5272's two numbers); the round: the decl-refs finding, the full run's counts and its NEW / FIXED / LOST with each cause, the
baseline row counts before -> after; tasks filed / appended with their class word; the CLOSE-WITH-ROWS candidates; HEAD sha and
the commit count over main; the CLAUDE.md / REFERENCE.md pointers the reviewing session should apply; anything you could NOT do.
