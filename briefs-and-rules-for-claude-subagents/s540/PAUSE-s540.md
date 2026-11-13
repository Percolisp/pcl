# PAUSE-s540.md -- the reviewing session's running notes (s540, started 2026-11-13 11:3x)

RESUME RECIPE after a cut: read this file bottom-up, then `s540/resume-s540.md`, `s539/resume-after-stop.md`, `git worktree list`, each batch's `scratch/<label>/STOP.md` first line and its newest logs' END lines.

## 11:3x -- START.  The USER: continue.
* The box was rebooted (up 11:25); `/tmp` empty; no heavy leg, no SBCL, no `HEAVY.holder*`; 8 GB available.  heavy.sh = the THREE-slot script.
* Main `2dce68ef` = MAIN-READY-87 `5ca8e976` + records (code `23f0f917`, v2-13700).  origin/main = the same; CI GREEN on it (public API, 11:3x).  All four batch trees clean at their `resume-after-stop.md` HEADs.
* Written: `s540/resume-s540.md` (the delta: reboot, three agents, the two timeout rules from the s539 standstill).
* LAUNCHED, model opus / effort xhigh, each in its existing worktree (no isolation): **s537a**, **s537b**, **s538a**.  **s538b** when the first of them is merged (three at a time = the standing USER word; four was for the s539 sitting).
* ASKED the USER; ANSWERED 11:4x: **the written briefs MAY START as slots free** (the s539 "no new subtasks" is lifted), **THREE at a time**.  Order for freed slots: s538b (a stopped batch) -> s539d -> s539a -> s539c; s539b only after s537b is merged.
* NEXT: merge in MERGE-READY order with the usual review (s537b's two RCDIFF pairs; s538a's pipe / locked-append probes + its `use integer` regions in `Pl/Parser2.pm`; s538b's README sentence against measurement A, and #4864); the 13 `*PLAN*` row texts + CLAUDE.md / STATUS board numbers after s538a; the companion `--all --quick` on a quiet box; the written briefs when the USER allows, s539d first.

## 11:4x -- s538b PRE-REVIEWED while it waits for a slot (tree `2f834ded`; probes perl -> main -> tree, `~/pcl-agent-scratch/s540/review/d/p1..p4.pl`, runner `s539/review/pv.sh`).
* p1 (a global, a file lexical, a hash / array member, an inherited DESTROY calling SUPER::DESTROY, a closure-held object, a class with no DESTROY, an END block), p2 (`exit 3` inside a sub, a caught die, an object graph root + leaf, re-bless away from / into a DESTROY class), p3 (a DESTROY installed late through a glob, an AUTOLOAD-only class, an empty DESTROY, `undef $t`, `@objs = ()`): the tree makes EVERY call perl makes, each once, rc equal; main makes none.  The differences are WHEN only: perl destroys file lexicals at phase RUN before the END blocks, the tree all of them at DESTRUCT after END (so an END block sees `live=6` where perl sees 1, and "END sees status" prints before the destroys).  As designed and as the README sentence says.
* p4 (File::Temp object + `tempdir(CLEANUP => 1)` + a forked child): perl = main = tree (file and directory removed).
* The README / STATUS / CHANGELOG wording read against measurement A: it says late, says an object the collector reclaimed gets none, quotes the 9,727 of 100,000 and names the guard-object case.  True to the measurement.
* **#4864 (the keep-alive proposal) RULED: stays filed, unscheduled, under #2370** -- it is the first step of the parked Stage 2 licence walk (an emission change for a timing guarantee the USER parked in s539); the runtime ring alternative saves the newest N only and is not built.
* Left for its merge: the bars (gate, sweep, corpus-diff + ir-conform, bench B = the `bless` cost with a DESTROY class loaded, C quiet, companion), everyday, the Try-Tiny finally.t newly reached rows 17 18 23 keyed #2370.
