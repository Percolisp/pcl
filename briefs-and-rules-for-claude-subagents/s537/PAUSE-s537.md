# PAUSE-s537.md -- the reviewing session's running notes (s537, started 2026-11-10 00:1x)

RESUME RECIPE after a cut: read this file bottom-up, then `s537/resume-s537.md`, `git worktree list`, each batch's `scratch/<label>/STOP.md` first line and its newest logs' END lines.

## 00:2x -- START.  The USER: continue; THREE subtasks at a time, Opus 5.5 at xhigh.
* The box was rebooted (up 00:14); `/tmp` empty; no heavy leg, no SBCL; 11 GB available.  heavy.sh = three slots.  `git worktree prune` removed the three dead `/tmp` ref-trees.
* Main `75496b08` = MAIN-READY-83 `f5ecb8fc` + records (code `3139fcb0`, v2-13100).  origin/main = `4b1f7493`; CI + install-matrix GREEN on it (read through the public API, 00:2x).
* Written + committed `efaa4159`: `s537/resume-s537.md` (what changed since `s536/resume-after-oom.md` UPDATE 2; s536i's uncommitted `_subscript_form` edit (#4741) RULED = FINISH, with the breaking-shape probes named).
* LAUNCHED 00:2x, each in its existing worktree (no isolation), model opus / effort xhigh: **s536e** (owes ir + bench + records), **s536h** (owes gate + sweep + everyday), **s536i** (finish #4741, then every leg again).  **s536j** takes the slot of the first one merged.
* ROUND DUTY (board + companion `--all --quick`; -75..-83 unmeasured by the board, -64..-83 by the companion): taken on the merged main once these batches are in and the box is quiet -- a board leg beside three agents ran 2 h in s536 and stalled every bench behind it (PAUSE-s536 11:5x).
* Owed by me, light: s534a's AUTOLOAD probe rows 37-38.

## 00:2x (2) -- s536e PRE-REVIEWED while it takes its legs (tree code is final).
* `Pl/t` diff read: +13 span-binder rows, 2 while-my-closure blocks, misc-fixes-01 row 132 leaves its TODO with the assertion unchanged.  Nothing weakened.
* Probes perl -> main -> tree (`~/pcl-agent-scratch/s537/review/e/p1..p10.pl`): tree = perl on 8 of 10, main wrong on 6 of those (p1 `$a`/`$b` across sections, p3 list-`my` containers, p5 chained-`my` per iteration, p6 an embedded `my` re-mentioned, p10 a closure over `my $a`).  The two the tree misses are PRE-EXISTING and not the batch's: p2 = #314 family F-F (file-level `state @arr` refuses the file; note appended), p4 = **#4782 FILED** (`sub f ($n = @d)` binds the array, not its count -- silent wrong).
* Left for its merge: the bars in its report, ff, CLAUDE.md + STATUS numbers, tasks #4547 #4541 #4543 #4545 #4641 #4642 COMPLETED by me.

## 00:2x (3) -- s536h PRE-REVIEWED (tree code final; it owes gate + sweep + everyday).
* `Pl/t` diff: only the four plan counts go up; rows added to errno-magic-01, forced-main-sub-01, open-modes-01, system-block-01.
* Probes perl -> main -> tree (`~/pcl-agent-scratch/s537/review/h/p1..p5.pl`): tree = perl on all five, main wrong on all five (p1 the `-` opens + empty file name; p2 `wait` / `waitpid` -- main leaves `$?` stale and a WNOHANG poll stores GARBAGE, 628211684; p3 `$!` across EOF / read / getc / truncate; p4 `open(*{"STDERR"}, ...)` inside a package; p5 `do FILE` clears `$!`).

## 00:2x (4) -- OWED ITEM DONE: s534a's probe matrix re-run on main `3139fcb0` (`~/pcl-agent-scratch/s537/review/matrix/`): the AUTOLOAD rows 37-38 = perl; row 49 (`use parent -norequire` with `::` names) = perl now.  Still differing, all known before: 30-31 (undef of a sub), 39-40 (c3 / next::method, ns-mro), 56 (stash alias), 60 (inherited overload fallback).  Input for the method-lookup batch (#4523-#4525) when the USER rules on it.

## 00:3x -- **s536e MERGED = MAIN-READY-84 `9c425a19`** (code = its tree, v2-13300)
* ff 13 commits.  Its bars (report): gate 369 / 11,990 PASS, sweep 18,795 +0 (each.t 64 65 by edit), corpus-diff 3 / 111, wide 11 DIFF / RCDIFF 0, refactor check identical, ir 31 / 328-0-17-0, everyday 116 + `--record`, Calc.pm -1.4 %.  On merged main I ran the three guard files + artifact-staleness (199 rows PASS) and the six probes main had failed: all = perl.
* COMPLETED by me: #4547 #4541 #4543 #4545 #4641 #4642 #2401 #1027 #1964.  #338 stays open (half b1 = #4662).  CLAUDE.md + STATUS numbers.
* s536h reported MERGE-READY `71a4b827` one minute later, on the PRE-merge base: told to rebase and re-take gate + sweep on the combined tree.  s536i told main moved (rule 6).  s536j LAUNCHED in the freed slot.

## 00:4x -- brief `s537/s537a-prompt.md` WRITTEN for the slot s536h frees (not launched yet).
* s537a = CALL AND SUB-BODY SHAPES: #4782 #4281 #4762 #4703 #4664+#4325 #4264; IDs 4801-4819; generation v2-13600.  All six re-probed wrong on merged main today (`~/pcl-agent-scratch/s537/review/a/m1..m5.pl`, `review/e/p4b.pl`).  Regions disjoint from s536i (ExprToCL aliasing / argument loop / element generators) and s536j (PPI, `_repair_*`).
* NEXT brief idea (s537b, for the slot after that): the file-lexical / state follow-ups of s536e -- #314 F-F plain shapes (file-level `state @arr`, #1579), #4661, #4662 (= #338 b1), #4663, #4461.
* s536h is re-taking gate + sweep + corpus-diff on the combined tree `1426433e` (queued behind s536i / s536j on the three slots).

## 00:5x -- **s536h MERGED = MAIN-READY-85 `bdd36d06`** (`cl/` + `Pl/Environment.pm` accessor; generation v2-13300 unchanged)
* ff 12 commits.  Bars on the combined tree `1426433e`: gate 369 / 11,998 PASS, sweep 18,795 +0 GATE clean, corpus-diff vs e7205dde IDENTICAL; wide A/B / ir-conform / bench / everyday from the pre-merge tree (a `Pl/`-only move cannot change a `cl/` batch's A/B).  On merged main: its four guard files + artifact-staleness 81 rows PASS, check-parens balanced, my five probes = perl.
* COMPLETED by me: #4263 #4262 #4224 #4284 #4310.  #4286 measured, open.  Filed by it: #4721 #4722.
* s536i + s536j told (a `cl/` move: gate + sweep re-take).  **s537a LAUNCHED** in the freed slot (new isolated worktree).
