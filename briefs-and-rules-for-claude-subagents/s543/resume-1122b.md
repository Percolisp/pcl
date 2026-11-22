# s543 RESUME of 2026-11-22, evening (the reviewing session, 19:4x) -- s543g, s543h, s543i finish in their worktrees, STACKED in merge order

You are a RESUMED batch.  Your brief is unchanged (`scratch/<label>/prompt.md` in your worktree), the rulebook is unchanged
(`briefs-and-rules-for-claude-subagents/s473/COMMON.md`), your predecessor's handover is `scratch/<label>/STOP.md` + its WIP
commits.  Read those three, then `s543/resume-s543.md` (the box rules, the END-line rule, the crash lesson about the cache) and
`s543/resume-1121.md` (how a resume works), then this file.  Where they differ, this file wins.

| label | worktree `W` (under `/home/bernt/pcl/.claude/worktrees/`) | HEAD at the stop | generation | merges |
|---|---|---|---|---|
| s543g | `agent-ae97f94b7e7baf573` | `fc80a722`, clean, 9 commits over `8ef15927` | v2-15600 | FIRST |
| s543h | `agent-af89e9edfd5c62db1` | `49d54f93` (code `452f674b`), clean, 9 over `8aaf1a29` | v2-15700 | SECOND, rebased onto s543g |
| s543i | `agent-a308d057d94d30a6c` | `76437561`, clean, 7 over `8aaf1a29`; PPI clone `scratch/s543i/PPI` branch `pcl-s543i` = `7fd02b75` | v2-15800 | THIRD, rebased onto s543h |

You were started in the main checkout, NOT in your worktree: `cd "$W"` first and use absolute paths.  `/home/bernt/pcl` (main) is
read-only for you; tasks go through `~/.claude/tasks/pcl`.  Do NOT create a new worktree.  Do not spawn subagents.  Append a line
`resumed 2026-11-22 <time> <your model id>` to `$W/scratch/<label>/MODEL.txt` before anything else (execution agents are
`claude-opus-5-5`; anything else: write it, stop, report only that).

## What is different from the stop order of 17:2x

- **No crash, no reboot**: the box has been up since 14:22, the session was ended by the USER and is continued the same evening.
  Nothing ran when you were started; `HEAVY.holder` is empty; everything in `HEAVY.queue` above the `17:2x s543 ENDED` marker
  line is stale text.  Your scratch files and extractions are as you left them.
- **Main moved in RECORDS only**: `8ef15927` / `8aaf1a29` -> `381967e6` + this file's commit (docs, briefs, tasks, memory; CODE is
  still `0fca1399`, v2-15500).
- **THE USER (19:2x): the whole box is ours again -- `heavy.sh` is the FOUR-SLOT script.**  A batch has at most TWO bounded legs
  queued or running at a time; ONE exclusive leg (`bench` kind, everyday, the companion runner) queued on the box at a time --
  read `HEAVY.holder` and the tail of `HEAVY.queue` first.  **A leg script is made executable (`chmod +x`) BEFORE it is queued**
  (#5191: 36 minutes of queue were spent to learn `Permission denied`).  A tooling agent is repairing `heavy.sh`'s starvation of
  an exclusive leg (#5191) in a COPY under its own `HEAVY_DIR`; the reviewing session installs it between legs -- same command
  line, you change nothing.  If your exclusive leg has waited more than 15 minutes, write the `waiting since` time and the
  holders into STOP.md and go on with what needs no SBCL.
- **THE BATCHES ARE STACKED IN MERGE ORDER, so that nobody takes its bars twice.**  The three reviews are done by the reviewing
  session while you run (probes perl -> main -> your tree at the stop sha); a finding reaches you as a message.
  - **s543g**: FIRST ACTION `git -C "$W" rebase main` (alone in its command, no pipe after it; check its exit status; records
    conflicts in `docs/DECIDED.md` / `docs/session-log.md` / `docs/supported-subset.md`: keep BOTH sides).  It moves no code and
    voids no log (`git diff main --stat -- Pl cl lib vendor tools` must show exactly your batch).  Then write the new HEAD's sha
    as the only line of `$W/scratch/s543g/REBASED` -- **s543h is rebased onto that sha.  From then on you commit RECORDS only**
    (docs, session-log, baselines' history row, STOP.md is untracked anyway).  If a CODE change turns out to be needed after
    that: make it, re-take what it voids, and put `CODE MOVED AFTER REBASED: <sha> <why>` on line 2 of STOP.md and first in your
    report.  Then your STOP.md's OWED list, items 2-4 (the everyday leg FIRST, queued at once: the box is quiet now and will not
    be in twenty minutes).  `baselines/cpan-board-dists.tsv`'s Pod-Simple line stays as it is: the round edits it.
  - **s543h**: you are started when `scratch/s543g/REBASED` exists; the sha is in your start message.  **Rebase onto THAT sha**
    (`git -C "$W" rebase <sha>`): it is main's head + s543g's code.  Keep **v2-15700**, regenerate the three artifacts, re-extract
    `scratch/s543h/base` from that sha (it is the BASE of every bar: your A/B, corpus-diff, gate-set scan, companion BEFORE,
    everyday BEFORE and the bench all compare `<that sha>` -> your tree).  Expect textual meetings in `Pl/Parser2.pm` (s543g:
    the `_local_*` and `_fh_drop_*` functions, the void-`delete` wrap) and `Pl/ExprToCL.pm` (`_push_item_is_scalar`); s543g's
    report of what it changed is `scratch/s543g/STOP.md` + `scratch/s543g/sessionlog.txt` in ITS worktree (read-only for you).
    As soon as the rebase and the artifacts are committed and your targeted gate (`Pl/t/eval-capture-01.t state-eval-01.t
    state-01.t` + the artifact-staleness guard) is green, write that HEAD's sha as the only line of `$W/scratch/s543h/REBASED`
    -- **s543i is rebased onto it; from then on RECORDS only**, with the same `CODE MOVED AFTER REBASED` rule.  Then STOP.md's
    OWED list, item 1's re-takes in full (gate, instr, wide A/B, gate-set scan, sweep -- name resolution: the sweep IS your
    gate), then 2-5.  When s543g has merged you get ONE message "main is now <sha>": `git rebase main` is then a records-only
    move and voids nothing.
  - **s543i**: you are started when `scratch/s543h/REBASED` exists; the sha is in your start message.  Rebase onto it (main's
    head + s543g + s543h), keep **v2-15800**, regenerate the three artifacts, re-extract your base from that sha, re-take the
    full gate + corpus-diff + the wide A/B + the sweep against it, then STOP.md's OWED list 1-7 on the rebased tree (the records
    of item 6 -- §11, the session-log section, #3697 -- need no SBCL: write them while legs run).  Expect textual meetings in
    `Pl/Parser2.pm` (s543h: `_publish_eval_cell`, the state prepass, `_requalify_eval_text`; s543g as above), `Pl/ExprToCL.pm`
    (s543h `_eval_lexical_alist`; s543g `_push_item_is_scalar`; yours: the digit-named element read).  **Member 5 (#1876) stays a
    design**: the USER has not ruled; do not start it.  The bare mirror is fast-forwarded (`72d2bd9e` -> `7fd02b75`) at
    MERGE-READY and nothing else is pushed.  After the merges before you, you get "main is now <sha>": a records-only rebase.
- **A base that is not main**: every instrument that takes `--ref <sha>` or a base extraction takes the sha you were rebased
  onto.  `tools/corpus-diff.pl` compares against a ref too -- give it that sha, and SAY in the bar's line which sha was the base.
- **`tools/everyday-smoke.pl --record` is NOT run by s543h and s543i before their own merge turn**: the history row is taken on
  the tree that merges.  s543g runs it as its STOP.md says (LAST, on the clean committed tree).  s543h and s543i run it after
  their "main is now <sha>" rebase, as the last step before `MERGE-READY`.

## At MERGE-READY
`MERGE-READY: <sha>` as the first line of STOP.md.  After the report your worktree stays as it is.

## Final report (SHORT; your last message)
`MERGE-READY: <sha>` first (or `NOT MERGE-READY` + reason); model id; per member one line; every bar QUOTED with its log's name
and the base sha it compared against (full gate files / rows, sweep TOTAL and GATE line, the wide A/B's SAME / DIFF / RCDIFF with
each DIFF's cause, corpus-diff's SILENT-DROP line, gate-set scan, drop census, ir-conform, ir-host-leak, EVERYDAY base -> tree,
the bench line; s543i: the PPI suite's counts, the branch head and the mirror's); tasks filed / appended with their class word;
the review points the reviewing session should probe; HEAD sha, the commit count over the sha you were rebased onto; the CLAUDE.md
/ REFERENCE.md pointers the reviewing session should apply; anything you could NOT do.
