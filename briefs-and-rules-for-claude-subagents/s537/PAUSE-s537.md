# PAUSE-s537.md -- the reviewing session's running notes (s537, started 2026-11-10 00:1x)

RESUME RECIPE after a cut: read this file bottom-up, then `s537/resume-s537.md`, `git worktree list`, each batch's `scratch/<label>/STOP.md` first line and its newest logs' END lines.

## 00:2x -- START.  The USER: continue; THREE subtasks at a time, Opus 5.5 at xhigh.
* The box was rebooted (up 00:14); `/tmp` empty; no heavy leg, no SBCL; 11 GB available.  heavy.sh = three slots.  `git worktree prune` removed the three dead `/tmp` ref-trees.
* Main `75496b08` = MAIN-READY-83 `f5ecb8fc` + records (code `3139fcb0`, v2-13100).  origin/main = `4b1f7493`; CI + install-matrix GREEN on it (read through the public API, 00:2x).
* Written + committed `efaa4159`: `s537/resume-s537.md` (what changed since `s536/resume-after-oom.md` UPDATE 2; s536i's uncommitted `_subscript_form` edit (#4741) RULED = FINISH, with the breaking-shape probes named).
* LAUNCHED 00:2x, each in its existing worktree (no isolation), model opus / effort xhigh: **s536e** (owes ir + bench + records), **s536h** (owes gate + sweep + everyday), **s536i** (finish #4741, then every leg again).  **s536j** takes the slot of the first one merged.
* ROUND DUTY (board + companion `--all --quick`; -75..-83 unmeasured by the board, -64..-83 by the companion): taken on the merged main once these batches are in and the box is quiet -- a board leg beside three agents ran 2 h in s536 and stalled every bench behind it (PAUSE-s536 11:5x).
* Owed by me, light: s534a's AUTOLOAD probe rows 37-38.

## 00:5x -- s536e PRE-REVIEWED while it takes its legs (tree code is final).
* `Pl/t` diff read: +13 span-binder rows, 2 while-my-closure blocks, misc-fixes-01 row 132 leaves its TODO with the assertion unchanged.  Nothing weakened.
* Probes perl -> main -> tree (`~/pcl-agent-scratch/s537/review/e/p1..p10.pl`): tree = perl on 8 of 10, main wrong on 6 of those (p1 `$a`/`$b` across sections, p3 list-`my` containers, p5 chained-`my` per iteration, p6 an embedded `my` re-mentioned, p10 a closure over `my $a`).  The two the tree misses are PRE-EXISTING and not the batch's: p2 = #314 family F-F (file-level `state @arr` refuses the file; note appended), p4 = **#4782 FILED** (`sub f ($n = @d)` binds the array, not its count -- silent wrong).
* Left for its merge: the bars in its report, ff, CLAUDE.md + STATUS numbers, tasks #4547 #4541 #4543 #4545 #4641 #4642 COMPLETED by me.

## 01:0x -- s536h PRE-REVIEWED (tree code final; it owes gate + sweep + everyday).
* `Pl/t` diff: only the four plan counts go up; rows added to errno-magic-01, forced-main-sub-01, open-modes-01, system-block-01.
* Probes perl -> main -> tree (`~/pcl-agent-scratch/s537/review/h/p1..p5.pl`): tree = perl on all five, main wrong on all five (p1 the `-` opens + empty file name; p2 `wait` / `waitpid` -- main leaves `$?` stale and a WNOHANG poll stores GARBAGE, 628211684; p3 `$!` across EOF / read / getc / truncate; p4 `open(*{"STDERR"}, ...)` inside a package; p5 `do FILE` clears `$!`).
