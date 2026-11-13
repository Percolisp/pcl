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

## 11:5x -- `docs/STATUS.md` board row = the s539 round (641 / 161 / 27 / 67 of 896; 109,797 ok / 1,740 not ok, the twelve load timeouts counted by their one-file re-runs).  CLAUDE.md's CURRENT NUMBERS gets the same at the first merge.
* STATE: all three slots held (s537a gate-SET scan, s537b #4461 scan, s538a `leg660b`); s537a's exclusive bench queued behind its scan.

## 12:0x -- s537a's agent DIED on an API login error (~11:57); the USER logged in again; RESUMED by message.  On disk at the cut: `gss.log` END rc=0 (gate-SET IDENTICAL, 638 x 2), `bench3.log` END rc=0 11:56 (taken at load 1-2).  It owes only reading the bench + records.  s537b (legs A B running, D E queued -- it re-takes them: ask why in its report) and s538a (the hang DIAGNOSED per its STOP.md; `legsweep` running at HEAD `1b524cb2`) were not cut, as far as their slots show.

## 12:1x -- THE USER asked for TWO SPEED TASKS; FILED + BRIEFS WRITTEN (not launched; no free slot):
* **#4787** = `s540/s540a-prompt.md`: the CPAN board timed under perl and PCL file by file (a checked-in `tools/board-timing.pl`; a coarse CPU-time pass in bounded chunks, a precise exclusive pass over the top ~40), the excess split transpile / load / run, the heaviest files profiled, ONE ranked list of things that should be faster, each filed with its measurement.  IDs 4961-4979.  No code change.
* **#4788** = `s540/s540b-prompt.md`: the rest of PCL read for low-hanging speed fruit in five parts (emitted shapes, runtime hot helpers, shims vs XS, the compile pipeline, start-up re-measured only), each suspicion measured on a scratch copy; the list + a "not low-hanging" table + the status of the older worklists' items.  IDs 4981-4999.  No code change.
* #4621 stays the umbrella (noted).
* **ORDER FOR FREED SLOTS now: s538b -> s540a (the USER's newest ask) -> s539d -> s540b -> s539a -> s539c; s539b only after s537b's merge.**  s540a's precise pass is exclusive in 15-minute chunks; s540b reads s540a's list if it is on main by then.

## 12:1x -- **s537a MERGED = MAIN-READY-88 `a435a391`** (code sha `53dd73da`, generation v2-13750 on main)
* ff 19 commits (rebased over the s540 records first).  Its bars on the combined tree (report; logs in its `scratch/s537a/`): gate 369 / 12,046 + tr-01.t row 9 = 12,047 PASS, sweep 18,796 (+1 tr.t, by edit) GATE clean, corpus-diff 3 / 111, wide 29 DIFF explained / RCDIFF 0, ir 31 / 328-0, gate-SET 638 identical (`gss.log`, s540), everyday 116 + `--record`, Calc.pm 5.52 -> 5.51 s, `op/filetest.t` 374/62 -> 375/61.
* REVIEW: code since the s539 pre-review (`e7ffae33`) = `Pl/VarAnnotator.pm` +34 only (`_loop_cond_uctx`: the condition of a while / until loop that ends its block is a value read -- a superset rule that costs a raw-string verdict, read and accepted) + two guard rows; `Pl/t` diff vs main = plan counts + added rows, nothing removed.  Probes perl -> main -> tree (`~/pcl-agent-scratch/s540/review/a/p1..p4.pl`; p4 new: twelve loop-value shapes incl. `do {} while`, the modifiers, list context, a bool-only string flag; #1493's count-only `tr` on a reference; the read-only `tr` / `s///` / `s///r` shapes): tree = perl on p1 p2 p4, p3 differs in the ONE #4785 line; main wrong on all four (p4: dies on the read-only loop).
* MERGED-MAIN CHECK `~/pcl-agent-scratch/s540/merge/a-check.log` END rc=0: 11 guard files / 293 rows PASS; p1-p4 merged = tree.
* COMPLETED by me: #4782 #4281 #4762 #4703 #4802 #4664 #4264 #1493 #4325 (its sub-call half is #4804, the class operand #4785 -- open).  CLAUDE.md + STATUS numbers (incl. the s539 round board).  Open, filed by it: #4801 #4803-#4807.  Its note: `op/tie_fetch_count.t` reads 140/1 on base and tree against a stale 139/2 snapshot -- for the round companion.
* s537b + s538a TOLD (12:0x): rebase over `53dd73da`, keep their generations, re-take what the move can change; legs against the old base are void.
* **s538b LAUNCHED 12:1x** in its worktree (opus / xhigh) with the pre-review's rulings (wording accepted; #4864 unscheduled; one sentence owed in ir-spec on file lexicals destroyed after END).
* NEXT freed slot: **s540a** (#4787).
