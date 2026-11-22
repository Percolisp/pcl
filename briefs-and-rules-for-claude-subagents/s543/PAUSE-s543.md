# s543 -- running notes of the reviewing session (2026-11-16)

Read bottom-up for the newest state.  The record is `docs/session-log.md` `## Session 543` and `docs/DECIDED.md` `## s543`.

## Start
* main `cbf9c350` clean (MAIN-READY-98, code `c5f618f2`, v2-15000), CI green, nothing running, `HEAVY.holder` empty.
* The session began as questions from the USER: the state of the PPI move (64 of 66 registry rows fixed on the vendored branch, one upstream, §11 open; the report 453 ok / 6 not ok), the state against CPAN and perl's own suite, and what it takes to make PCL usable for non-XS production code.

## The decisions (all the USER's; DECIDED `## s543`)
1. **The supported-subset FREEZE**, until it is seen to work: `docs/supported-subset.md` (DRAFT; open: scope-exit DESTROY for the service claim, Moo, the module lists).
2. **The UTF-8 flag is done LAST**; the content rule is rejected as wrong (perl flags a decoded pure-ASCII string).
3. **Line tracking ON by default** -> batch s543a.
4. **Subtasks may be started again** (the s542 stop is lifted for this work); TWO at a time.

## Launched
| label | what | generation | task IDs | brief |
|---|---|---|---|---|
| s543a | line tracking on by default: the flip, the emission bar (tree default = base + `PCL_OPT=line-track`), the rows it fixes edited out, the cost table | v2-15100 | 5100-5119 | `s543/s543a-prompt.md` |
| s543b | seven filed silent wrongs: #5082 (regression) #5021 #4792 #4795 #5005 #4794 #4796 | v2-15200 | 5120-5139 | `s543/s543b-prompt.md` |

The reviewing session's own task IDs: 5084-5099.  Both batches: Opus 5.5 / xhigh, a new isolated worktree each, base = main at the launch commit.  Merge order: whoever is MERGE-READY first; the other rebases.

## Owed by the reviewing session
* The TRIAGE of the open tasks against `docs/supported-subset.md` (917 open on 2026-11-16): each `in` / `out` / `tooling` / `decision` with a reason; the silent-wrong tasks first, because exit criterion 1 counts them and the next brief is drawn from them.
* The next brief's candidates after s543b: #4887 #5081 #5061 #4793 (the rest of the s542 review findings), #5083 + #4904 + #4643 (heap exhaustion: exit criterion 4), the soak test #2945 (criterion 3), the Rosetta census #2104 (criterion 2).
* The ROUND for MAIN-READY-96..-98 + this session's merges (the board + the companion on merged main).
* For the USER: the PPI fork's default branch is `master`, so its front page does not show the 133 commits of branch `pcl`; changing the default branch is a repository setting.  Whether upstream pull request #459 should be split into small per-group requests is the USER's call; nothing was sent.

## 21:4x -- both batches LAUNCHED; MODEL.txt checked
* s543a worktree `.claude/worktrees/agent-a9d6af55959b1f0c0`, s543b `.claude/worktrees/agent-aca67256b5619cf0d`; both `claude-opus-5-5`, both rebased onto `05e9867f` (the s543 docs commit, pushed; gate on it 379 files / 12,303 rows PASS).

## The triage, first pass: the 105 open tasks marked SILENT WRONG (subjects read; 28 probed with ONE shape each, perl vs main)
**A one-shape probe that agrees with perl does NOT close a task**: the task's own reproducer is run first (CLOSE-WITH-ROWS).  It does say the count of 105 is not a count of live bugs.
* **LIVE on my probe, ordinary code (the next brief's first candidates):** #1039 (`"count: " . @a` prints `ARRAY(0x1)`), #1955 (`for (mk())` with `mk` returning an array ref iterates the referent), #540 + #1190 (`local $h{a}{b} = 6`: one family), #691 (`s/A/${$r}/` writes the empty string), #2500 (a `qr//` interpolated into a `/x` pattern takes the outer `/x`), #2560 (`Math::BigRat->new("49/4")` is 1), #4843 (`%_` is per package; Text::CSV_PP uses it), #4846 (`<>` after `@ARGV` is exhausted does not fall back to STDIN), #1040 (`"12" & "10"`), #1494 (`\N{NAME}`).
* **Did NOT reproduce on my one shape (18 of 28; each needs its own reproducer run, then a close with its rows or a narrower subject):** #683 #971 #1059 #1349 #1419 #1420 #1425 #1609 #1956 #2003 #2091 #2540 #2760 #2768 #2780 #3610 #3827 #4302.  (#4302 was fixed by s539c's `keep_line_endings`; #2540 by PPI group AZ; #1059 is still listed as a sharp edge in `docs/STATUS.md` -- check before editing either.)
* **In s543b:** #4792 #4795 #4796 #5005 #5021 (+ #5082, #4794, not marked silent wrong).
* **Proposed OUT (the construct belongs to an OUT area; each should be LOUD or listed in `docs/supported-subset.md` section 4 -- the USER's to confirm with the module lists):** #332 (refaliasing array spellings), #486 (an illegal named prototype), #487 (`$![1]`), #822 (regex code block), #1242 #1484 #3491 #4204 (glob / stash surgery), #1480 (arylen magic), #1487 (`$$qr = V`), #1491 (a self-redefining sort comparator), #1960 #1971 (exotic interpolation), #2110 (unbounded integers: ruled by design), #2638 #2876 (`@_` aliasing through an expression).
* **`decision`:** #2092 (lc / uc on bytes 128-255 in a source without `use utf8`), #1050 (perl's false is a dualvar).
* **IN, rarer shapes (the remaining ~54, subjects only, not probed):** the `local` list / element family #508 #509 #510 #690 #1339 #1341 #2112 #2742 #5001; string-eval name resolution #1943 #2630 #4821; autovivification on read #1350 #1352; handles #1423 #2594 #4842 #4844 #5081; the rest one each.
* NEXT for a freed slot: a VERIFICATION batch -- run every open silent-wrong task's own reproducer perl vs main, close with rows what passes, narrow what half-passes; its output is the true count for exit criterion 1.

## 22:34 -- THE BOX WENT DOWN under both batches; 22:39 both RESUMED (`s543/resume-s543.md`)
* Cause as far as the journal shows: the previous boot ends in `amdgpu` DMCUB display errors and a gnome-shell restart; no OOM kill, no thermal event.  Three bounded legs ran (s543a board-both + gate-set-scan, s543b Path-Tiny), 2 GB used of 12.
* Intact: `git fsck` clean, main `05e9867f` unchanged, s543a at `cd04e308` (WIP 5, clean tree), s543b at `f014f5e2` (WIP 6/7) + an uncommitted 17-line member-7 draft, both STOP.md current to 22:31, the tasks both filed (5101 5102 5104 5105, 5120-5125, appends to #398 #1291) valid JSON.
* **DAMAGED: the module cache.**  A crash leaves files with their FINAL name and NO content: a zero-size core (`pcl-5d366543-…core`), a zero-size extension fasl, ~40 zero-size `proto/*.json`.  Removed: every cache file written from 22:28 on, every zero-size one, and the sibling files of each removed entry (239 files, 23 stems).  The write-to-temp-then-rename in `docs/caching.md` protects against a RACE, not against a power loss (no fsync before the rename) -- worth a task if it recurs: a reader that finds a zero-size core / fasl / proto file should treat it as a miss.
* VOID by the END-line rule: s543a `board.log`, `gss.log`, `sweep.log`; s543b `pt-after.log`; s543b's member-6 gate never started.
* `HEAVY.holder*` cleared (stale text; the flocks died with their holders).  New rule for this sitting: at most TWO bounded legs per batch queued or running (s543a held two slots and a third leg while s543b's stood in the queue up to 20 minutes).
* Resumed as two new Opus 5.5 / xhigh agents in the SAME worktrees (no new worktree); MODEL.txt gets a `resumed` line -- to check.

## 22:5x -- THE TRIAGE, first pass DONE (all 927 open tasks, from their subjects)
* `tools/task-triage.pl`: **927 open -- in 634, out 120, tooling 115, decision 36, untriaged 22; IN and marked silent wrong 117** (an upper bound: see the 18 of 28 above).  Fields `triage` + `triage_reason` in each task file; the flat list is `s543/triage-s543.tsv` (id, class, reason).
* **The 22 untriaged are the tasks the running batches own** (5100-5139 so far filed; #233 #398 #1240-closed #1291 #1361 #4790 #4792 #4794 #4795 #4796 #5005 #5021 #5082): their rows ARE in `triage-s543.tsv`; write them at each batch's merge (a write now could be lost under an agent's read-modify-write).  New tasks the batches file after 22:5x have no row yet.
* Definitions used: `out` = outside the SUBSET (section 4, not-supported, XS, parked, board widening) or outside the CLAIM (speed, message text, warnings); `tooling` = runners, baselines, harness, instruments, structure; `decision` = the USER's (21 constructs proposed for section 4 + the standing questions).
* Families worth ONE brief each (from the reasons column): the `local` family (#508 #509 #510 #540 #1190 #690 #1339 #1341 #2112 #2742 #5001 #574 #652 #1192 #1243 #1451 #3414 #3415 #2598); string-eval name resolution (#246 #1455 #1943 #1957 #2557 #2630 #4821); autovivification (#1350 #1352 #1353 #1663 #3720); tidy errors (#1427 #1595 #1970 #2108 #2109 #2556 #2597 #4786 #5002 #1746 #3062 #2956); exit status (#524 #2740 #285 #3522); heap / stack (#5083 #4904 #4643 #4322 #160 #424 #935 #1929 #2113); "verify, probably long fixed" (grep the reasons for `verify`: 40 tasks -- the verification batch takes them with the silent wrongs).
* For the USER: the 36 `decision` tasks (`tools/task-triage.pl --list decision`).

## 23:0x -- the NEXT brief is written, NOT launched: `s543/s543c-prompt.md` (the VERIFICATION batch)
* 139 tasks (`s543/s543c-tasks.tsv`: the 117 IN + marked silent wrong and the 39 whose reason says `verify`, the running batches' tasks excluded); no `Pl/*.pm` / `cl/` / `lib/` change, guard rows + task edits only; task IDs 5140-5159; no generation.
* Launch it when s543a or s543b is MERGE-READY (two subtasks at a time), in a NEW isolated worktree, Opus 5.5 / xhigh, MODEL.txt checked.
* Both resumed agents wrote `resumed ... claude-opus-5-5` (checked 22:49).

## 23:3x -- SESSION ENDED by the USER; both batches STOPPED ON ORDER at clean checkpoints -> `s543/resume-after-stop.md`
* s543a `bd8c819d` (13 commits, clean): every bounded bar met; owed the three exclusive timing legs, the cost numbers, everyday `--record`, MERGE-READY.
* s543b `f99be16c` (14 commits, clean): seven members + two own regressions fixed, sweep / ir-conform / gate-set scan / A-B taken on the final code; owed artifacts re-run, companion before/after, everyday, inverse, the full gate, MERGE-READY.
* Gate on main 379 / 12,303 PASS (22:54); the record commits pushed.  s543c written, not launched.  Nothing runs.

## 2026-11-21 14:4x -- the session continues; s543a + s543b RESUMED a third time (`s543/resume-1121.md`)
* The box was shut down in order after the 16th and booted today: no cache file but the `.lock` files is empty, the newest is from 23:30 on the 16th; `/tmp` empty; `HEAVY.holder` empty.  Main `ef0e30e6` = `origin/main`, records only over `05e9867f`.
* Both resumed as new Opus 5.5 / xhigh agents in the SAME worktrees; each rebases onto `ef0e30e6` first (records only, voids no log), then finishes its STOP.md list.  s543a's three exclusive timing legs go first on the box.
* s543c (the verification batch) waits for the first MERGE-READY (two subtasks at a time).
* Owed by the reviewing session, unchanged: review + merge both, the batch-owned triage rows, the ROUND, the 36 `decision` tasks for the USER.

## 15:1x -- s543a reported MERGE-READY `c69904ac` and its own regression #5107; RULED: fixed inside the batch before the merge
* #5107: since the flip, `sub DESTROY {}` / an empty `AUTOLOAD` is no longer recognised by `p-sub` (the `(p-file ...)` heading the body hides the shape), so the global-destruction gate opens (+28 ns a bless, one empty call per live object at exit; output unchanged).  The batch's own regression, a one-predicate `cl/` fix -> back to the agent with a work list: the helper, a guard row reading `*p-destroy-live*` with its inverse on `c69904ac`, ONE probe each for the two other body-shape checks it named, a grep for other empty-sub recognitions, the `cl/` bars (sweep, ir-conform, gate, the bless bench rows), everyday `--record` last on the final tree.  Main stays at `ad836e3b` (this file and the tasks uncommitted) until the first merge.
* s543a's numbers as reported (logs in its `scratch/s543a/`): cost median +0.8 % / geomean +1.1 % over 77 rows (noise geomean +0.6 %); outside the band feread x3 +16..17 %, emptysub +8.1 %, collatz +7.2 %, methinh +5.2 %; first run +1.9 / +0.3 / -0.1 %; EVERYDAY 115 -> 116 of 122.  Filed #5103 (PERF, the loop rows).
* REVIEW PROBES s543a (perl -> base -> `c69904ac`; `scratchpad/ra`, not kept): caller / caller(1) / croak / a runtime die in a module and in the script / die in a loop / `$SIG{__DIE__}` / a multi-line call / an anonymous sub / a string eval's runtime die (`(eval N) line 3`) -- tree = perl on every line the base gets wrong (base: `(eval 0) line 0` / `-:0`), except the filed residue: a BEGIN body (#5101), a module's path absolutised (#5104), an explicit `die` in a string eval names `-` (#4942).  Warm = cold; `PCL_OPT=none` = default; `PCL_OPT=-line-track` on the tree = the base's default output byte for byte; one module reached by two spellings gives two texts on a warm cache (the key fix holds).
* REVIEW PROBES s543b (perl -> base -> its final code; 38 + 27 + 10 lines): eval re-declared captures (reads, writes, closure, nested, array / hash, loop, two sites, twice = cache), `local TERM, TAIL` in list / scalar / sub-tail context with unary and list operators, tr escapes -- tree = perl on all 38 (base wrong on 22); the dirhandle record (ref, string, reopen, element, closed), `defined NAME`, per-package constants, the undef flush (a tied scalar is not FETCHed) -- tree = perl except PRE-EXISTING residue, filed: #5160 (`undef $io` on an IO::File object does not flush: one more arm), #5161 (`@$fh` does not die, LOW).  `chdir $closed_fh` dies "fchdir unimplemented" where perl returns false: consistent with PCL's `$Config{d_fchdir}`, not filed.

## 15:4x -- s543a MERGED: MAIN-READY-99 = `78202d0a` (fast-forward, 19 commits over `ad836e3b`; generation v2-15100)
* #5107 fixed inside the batch (`%p-empty-sub-body-p`; `Pl/t/destroy-exit-01.t` rows 31-34, inverse fails on `c69904ac`); the two other body-shape matches measured unaffected with a `*macroexpand-hook*` count (base = `c69904ac` = tree); the `cl/` bars re-taken: sweep TOTAL 18,808 GATE clean, ir-conform 335 / 0, ir-host-leak 31, the full gate 379 files / 12,313 rows PASS, EVERYDAY 116 of 122 recorded on the final tree.
* Records at the merge: CLAUDE.md "Test Status" (the old paragraph to HISTORY.md), the Quick Reference `PCL_OPT` lines in CLAUDE.md + REFERENCE.md, the `caller-implementation.md` lines; `docs/STATUS.md` 12,313; tasks #5107 #4790 #1361 CLOSED (the everyday carp-attribution row now names #233 alone); triage written: #233 decision, #5101 #5102 #5104 #5105 in, #5103 out, #5106 decision; #5160 #5161 filed by the review.
* s543b reported MERGE-READY `12727267` (on `ef0e30e6`) at 15:4x: it gets "main is now <sha>" and rebases, regenerates the artifacts, re-takes the gate, corpus-diff / emission-ab and the sweep.  NOTE from its report: the first everyday run after a generation bump can read one low (a cold Math::BigFloat compile times out; #5127) -- re-run warm before reading the number.

## 15:5x -- main = `22ec674b` (the MAIN-READY-99 record commit, pushed); TWO subtasks run (the USER, today: continue with two)
* Gate on merged main: 379 files / 12,313 rows PASS (`~/pcl-agent-scratch/s543/review/gate-78202d0a.log`).  CI run on `22ec674b` started.
* s543b: sent "main is now `22ec674b`" -- rebase (generation stays v2-15200), artifacts, two composition probes (the #4794 flush and the #5005 tail as a sub's FIRST statement under location forms), corpus-diff / emission-ab against a `78202d0a` extraction, the full sweep (expect 18,808), ir-conform, gate-set scan, the full gate (expect 12,336), everyday (warm) + `--record`.
* s543c LAUNCHED (Opus 5.5 / xhigh, new worktree `.claude/worktrees/agent-aa3026e6fbca3df66`): the verification batch, 139 tasks, no compiler / runtime change, IDs 5140-5159.  MODEL.txt to check.  It gets "main is now <sha>" when s543b merges.
* Main is held at `22ec674b` until s543b's merge (this file stays uncommitted until then).

## 16:2x -- s543b MERGED: MAIN-READY-100 = `64353611` (fast-forward, 20 commits over `22ec674b`; generation v2-15200)
* Its re-taken bars on the rebased tree (logs in its `scratch/s543b/`, each with its END line): the full gate 379 files / 12,336 rows PASS (`gate3.log`); sweep TOTAL 18,808 +0, GATE clean, drops 5 (`sweep3.log`); corpus-diff the same 15 of 111, SILENT-DROP 5; emission-ab lib 57 / 1, wide 1,926 / 121, RCDIFF 0, no newcomer (`ab2.log`); gate-set scan base2 = tree; ir-conform 335 / 0 / 10 known / 0 stale; ir-host-leak 31; EVERYDAY 116 -> 116 of 122 (the second, warm run on each side; #5127); artifacts byte-identical bodies.
* The review's probes re-run on the rebased tree: unchanged, = perl on all seven members' shapes (the residue: #5160 #5161).
* Its composition probe found a gap of member 6 that was never covered: `undef $g` inside a sub on a CAPTURED file-level lexical does not flush -> #5128 (filed, wrong on the base too, not a regression).  Accepted as filed.
* Tasks at the merge: #5082 #1428 #5021 #4792 #4795 #5005 #4796 CLOSED (no row keyed on any of them: the Path-Tiny rows are keyed `NS:ns-escaping-filehandle`); #4794 NARROWED to its residue (`delete $h{fh}`, `undef $obj`), kept open; triage written for #398 #1291 #5120-#5128 -- `tools/task-triage.pl` reads 0 untriaged.
* Records: CLAUDE.md "Test Status" (MAIN-READY-99's paragraph to HISTORY.md), the everyday row's #5127 clause in CLAUDE.md + REFERENCE.md, `docs/STATUS.md` 12,336.
* OWED: the ROUND (the board tree half + the companion `--all --quick` on merged main) -- after s543c's gate, the box to itself; s543c gets "main is now <sha>".

## 16:3x -- main = `34648f2e` (records over MAIN-READY-100 `64353611`, pushed); TWO subtasks run: s543c + s543r
* Gate on merged main: 379 files / 12,336 rows PASS (`~/pcl-agent-scratch/s543/review/gate-64353611.log`).
* s543c told "main is now `f5d89567`" (rebase; re-run the LIVE / NARROWED verdicts in s543b's seven areas) and that the round runs beside it.
* **s543r LAUNCHED** (Opus 5.5 / xhigh, MODEL.txt checked `claude-opus-5-5`; worktree `.claude/worktrees/agent-a26a98dbcc0bff1ae`, brief `s543/s543r-prompt.md`): THE ROUND on `64353611` -- the board in six chunks with a refreshed oracle + `--diff`, the companion `--all --quick`, the full companion with stamps; method = s542r's; IDs 5170-5189.
* For the USER (answered today): the PPI changes are on the fork's branch `pcl` (`66e690d2`, 133 commits over `master`) and in upstream pull request #459 (open); the fork's front page shows `master`, the default branch -- a repository setting.  PPI work is NOT finished: registry section 11 still repaired in `Pl/`, ten `_repair_*` / `_fix_ppi_*` passes not audited for deletion, ~20 open tasks naming a mis-lex.  A PPI slot was offered as the next batch; no answer yet.

## 16:5x -- the USER: the PPI tasks are run one batch at a time until they are done
* The brief of PPI batch 10 is WRITTEN, NOT LAUNCHED: `s543/s543d-prompt.md` (#2095, #483, #937 + the audit table of the ten repair passes still in `Pl/`, which plans batches 11 and 12; IDs 5190-5209; generation v2-15300).  It takes the FIRST FREE SLOT (two subtasks run: s543c, s543r -- the reviewing session's reading of "one at a time": one PPI batch at a time inside the two-subtask limit, not a third beside them).
* The PPI list as read today (open, IN, the fix on the branch or in a repair pass): #2095 #483 #937 (batch 10); #458 #2774 + registry section 11, #651 #2384 (batch 11); #3922 #2670 #1680 #1666 #1876, the tooling closes #357 #3805 #4903, the dead repair passes (batch 12).  Not branch items: #1580 (lowering), #4807 (PExpr), #1053 (a refusal), #4902 (a ruling owed), #1649 (a USER decision).
* After each PPI merge: push the fork, refresh pull request #459.

## 17:5x -- s543c MERGED: MAIN-READY-101 = `b4d401bd` (fast-forward, 7 commits; code still `64353611`)
* The verification batch: 138 listed tasks, each with a dated `=== s543c ===` verdict.  On the task store (a task on both lists counted once): 49 FIXED and closed with guard rows (new `Pl/t/transpile-test-11.t` + rows in fifteen files), 19 NARROWED, 69 LIVE, #3450 merged into #3100 (+ its partner #1615 into #1574).  The agent's own table (34 + 27 fixed) counts the 17 tasks that are on both lists twice.
* **Exit criterion 1's number: 86 live IN silent wrongs** (78 on the list + 8 of today's batches).  `tools/task-triage.pl` prints 92 (it reads a task's whole history): #5162 filed (tooling).  Its ranked list of the live ones in ordinary code is the next FIX brief: #1039 #1955 #750 #2555 #3100 #2766 #1030, the nested-element `local` family #540 #1190 #3415 #1339, #691, string-eval names #2091 #2557 #2630 #4821, then #1494 #2440 #2765 #1002 #2030.
* REVIEW: 12 closed tasks picked by hand (#508 #510 #531 #634 #751 #1186 #1609 #2003 #2768 #3082 #3102 #1655) re-run perl vs main from its `scratch/s543c/t<ID>/`: stdout + exit status equal, 12 of 12.  Every closed task's block names a `Pl/t` file.  No baseline row is keyed on a closed task (the grep's hits are row numbers and other keys).  It edited `fail-baseline.tsv` (kvhslice.t: three rows out, one in; bop.t and pos.t re-keyed) without a sweep -> the review ran the FULL sweep on its tree: TOTAL 18,808 +0, GATE clean, drops 5 (`~/pcl-agent-scratch/s543/review/sweep-s543c-b4d401bd.log`).  Its gate: 380 files / 12,364 rows PASS (`scratch/s543c/legs/gate.log`).
* Records: CLAUDE.md "Test Status" + rule 6 (the next new transpile-test file is `-12`), REFERENCE.md the same, STATUS.md, `docs/supported-subset.md` section 7 (the verification paragraph); #502 #1369 re-triaged `out` (their residue is documented OUT); the task store committed (139 files).

## 18:0x -- s543d LAUNCHED (PPI batch 10) in the slot s543c freed; two subtasks run: s543r + s543d
* s543d: Opus 5.5 / xhigh, MODEL.txt checked `claude-opus-5-5`; worktree `.claude/worktrees/agent-af6132c4984467746`; brief `s543/s543d-prompt.md`; IDs 5190-5209; generation v2-15300; a clone branch `pcl-s543d` of the bare PPI mirror.
* s543r told main moved to `7f9434fe` (records, `Pl/t/`, two baseline files; code unchanged), to run the strict verdict-register form once against the live store (61 tasks closed today), and to look at the Pod-Simple htmlbat.t rows keyed on #2006.
* Main = `7f9434fe`, pushed.

## 19:26 -- s543r MERGED: MAIN-READY-102 = `3ab102a4` (the round on `64353611`; baselines + docs only)
* **Board** (six chunks, oracle refreshed, no timeout): 668 PASS / 137 PARTIAL / 24 FAIL / 67 SKIP of 896; 109,191 ok / 1,190 not ok (s542r: 661 / 144 / 24 / 67; 109,714 / 1,225 -- the lower ok total is the IO-Compress `105oneshot` heap flap #4643, 1,004 rows).  `--diff` vs the same commit's baseline: 1 NEW / 4 FIXED / 3 UNVERIFIED / 2,014 LOST (the LOST are the two flaps).  NEW = IO-Compress `112utf8-zip.t` *PLAN*: a CLOCK-dependent face (the zip header's bytes >= 0x80 written as two; `NS:ns-use-bytes`), one-file identical on MR-95..-100 -- both faces blessed.  FIXED out: TT `chomp.t` 21 25 (s539c) -> #4265 CLOSED.  Pod-Simple `htmlbat.t` 1-8 re-keyed #2006 -> `NS:ns-escaping-filehandle` (the `undef $obj` shape: noted in #4794).
* **Companion**: `--all --quick` 128 OK / 31 NOTAP / 103 XDIFF / 1 FIXTURE / 265 UNEXPLAINED; ROW DIFF 0 NEW / 7 FIXED (s543a: cmd/subval.t 27 28, op/runlevel.t 12, op/tie.t 79, op/method.t, run/fresh_perl.t's expected row; s543b: re/subst.t 76 77) / 0 LOST.  The FULL run with stamps: 129 OK / 104 XDIFF / 242 DIFF / 11 TIMEOUT, 0 NEW / 0 FIXED / 0 LOST against the edited baselines; 12 of 14 stamps re-blessed.
* Review: the diff is baselines + docs only (9 files); `verdict-register-01.t` + the four `tools/t` baseline tests PASS on merged main.  #2006 NOT closed (four companion rows keyed on it; the entry's owner) -> re-triaged `out`.  Filed #5163 (266 companion rows keyed on 16 closed tasks: reported, not gated) and #5164 (`cpan-board.sh` cannot pass `--refresh-oracle`; the exclusive-queue check).
* CI green on `7f9434fe` (MAIN-READY-101's record commit).
* **The next FIX brief is written: `s543/s543e-prompt.md`** -- seven live silent wrongs from s543c's ranked list (#1030 #2555 #1039 #3100 #2766 #1955 #750; IDs 5210-5229; generation v2-15400).  It takes the slot the round freed, beside s543d.  After it: #691 with the nested-element `local` family (#540 #1190 #3415 #1339), then string-eval names (#2091 #2557 #2630 #4821).

## 19:27 -- s543e LAUNCHED in the slot the round freed; two subtasks run: s543d (PPI batch 10) + s543e
* s543e: Opus 5.5 / xhigh, MODEL.txt checked `claude-opus-5-5`; worktree `.claude/worktrees/agent-acfb59c7cdbd085a3`; brief `s543/s543e-prompt.md`; IDs 5210-5229; generation v2-15400.
* s543d told main moved in records only (`ac53cfd5`), that the round is done, and who its new neighbour is; it merges first.

## 20:0x -- s543d MERGED: MAIN-READY-103 = `a5a60b62` (PPI batch 10; fast-forward, 7 commits; generation v2-15300)
* Three fix groups on the branch (`pcl` `66e690d2` -> `842e6bbd`, 139 commits): BB §68 `__FILE__` / `__LINE__` / `__PACKAGE__` / `__SUB__` / `__CLASS__` end a term (`__LINE__-1`, `__FILE__.1` dropped the statement), BC §69 `<<NAME` after a term is the left shift (#483), BD §70 `{;}` ends its statement (+ `_normalize_null_statements` keeps the `;` for a Compound: the fix alone made `sub f { {; a => 1 } }` a hash, found by its wide A/B).  Two premises of the brief were wrong and it said so: #2095's float shape was fixed in s527a already; an empty `{}` at a statement's start is an anonymous hash for perl itself.
* THE AUDIT (`docs/ppi-fix-at-source-s527.md` §10): NO repair pass is dead -- each keeps the half only a declaration decides, and every measured fire follows an imported sub.  PPI's to fix: the builtin half of §11 and `%^`.  Two passes are lowerings (the foreach pair); the negative-number pass is half precedence, half prototype table.
* Its bars: gate 380 / 12,372, sweep 18,808 +0 GATE clean, corpus-diff identical, wide A/B 2,052 SAME / 1 DIFF (Devel/Peek.pm) / RCDIFF 0 (its OWN scripts, not `tools/emission-ab.pl` -- batch 11 is told to use the tool), gate-set scan identical, expect checker 208 / 154 -> 205 / 151, ir-conform 335 / 0, EVERYDAY 116.  It gated `871c1270`, three commits before its head.
* REVIEW: a 24-line probe of the three fixes and their controls (here-docs, `{;}` in six positions, the number spellings) perl -> main -> tree: tree = perl on every line, main dropped two statements.  The FULL gate on merged main: 380 files / 12,372 rows PASS (`~/pcl-agent-scratch/s543/review/gate-a5a60b62.log`).
* At the merge: #2095 #483 #937 #2541 CLOSED; #5190 filed by the batch (perl-tests/sprintf2.t carries PCL's own old edit, invalid perl); **the fork pushed** (`pcl` = `842e6bbd`) and **pull request #459 refreshed** (56 groups, 139 commits, 522 cases; `gh api -X PATCH` -- `gh` IS authenticated here, the old memory note was wrong); CLAUDE.md "Test Status", STATUS.md 12,372.
* **PPI batch 11 = `s543/s543f-prompt.md`** (the builtin half of §11 + #2774's builtin row, #1680, #3922, `%^`, the seven closes; IDs 5230-5249; v2-15500) takes the freed slot.  s543e gets "main is now <sha>".

## 20:01 -- s543f LAUNCHED (PPI batch 11) in the slot s543d freed; two subtasks run: s543e + s543f
* s543f: Opus 5.5 / xhigh, MODEL.txt checked `claude-opus-5-5`; worktree `.claude/worktrees/agent-a64e9ad186336c772`; brief `s543/s543f-prompt.md`; IDs 5230-5249; generation v2-15500; a clone branch `pcl-s543f` of the bare PPI mirror (`842e6bbd`).
* s543e told main moved in CODE (`e8dd2cdd`): it rebases before its end-of-batch bars, keeps v2-15400.

## 21:51 -- s543e MERGED: MAIN-READY-104 = `8959afae` (seven live silent wrongs; generation v2-15400); THE USER ENDS THE SESSION after this merge
* The seven: #1030 (+ #5105: one value layer for %ENV), #2555 (`system` / `qx` inherit the descriptors), #1039 (the string operators take an array's count on the operand as received: `%p-str-operand`), #3100 + #2766 (one list-slice lowering, `p-lslice-1`), #1955 (the foreach flattener: a box is one element; the task's suspect was not the cause), #750 (`p-list-=`: one "is it an aggregate" question).  All eight tasks CLOSED; no baseline row was keyed on any.
* Its bars (logs in its `scratch/s543e/legs/`): gate 380 / 12,380; sweep TOTAL 18,807 = 18,808 - 1 (magic.t row 194, a TODO row perl itself fails, passed only on the `local $ENV{K}` bug: out by edit), GATE clean; corpus-diff 13 of 111, SILENT-DROP 5; wide A/B 1,953 SAME / 94 DIFF / RCDIFF 0, all 94 the list-slice rewrite (a script undoes it: byte-identical); gate-set scan base = tree; ir-conform 335 / 0; ir-host-leak 31; EVERYDAY 116 -> 116; the bench: no row outside the noise band after one reorder of the new type test (strcat +1.4 %, ucshort +3.0 %).  Filed #5210 (`local %ENV` does not reach a child) and #5211 (sprintf's "Redundant argument" ignores `no warnings`).
* REVIEW (perl -> main -> tree, my own shapes, `scratchpad/re`): 48 lines over the seven members + 15 lines of string context (sub results by tail array / map / grep / keys / slice / reverse / wantarray, every reference kind, blessed, overloaded, tied, nested) -- tree = perl on 62 of 63, main wrong on about 45.  The one difference is not the batch's: a lexical `my $b` in scope of a sort comparator (perl warns and uses the lexical) -> #5165 filed, LOW.  The hot-path hunks read: the string / box / number test comes first.  Rebased by the reviewing session onto `ffc7eb72` (notes only) for the fast-forward: `7c5eda2b` -> `8959afae`.
* **The FULL gate on merged main: 380 files / 12,380 rows PASS** (`~/pcl-agent-scratch/s543/review/gate-8959afae.log`).
* **s543f (PPI batch 11) STOPPED ON ORDER at `f83ea7da`**, all five members done on the branch clone (`pcl-s543f` = `72d2bd9e`, §71-§74), the bare mirror NOT moved; owed: the rebase onto -104 and every merge bar again.  Filed #5230 (a comma list under a low-precedence `and` / `or`: SILENT WRONG) #5231 #5232 #5233.
* The verified silent-wrong count: 86 - 8 + #5165 #5210 #5230 #5232 = **82** (the tool prints 87: #5162).
* **NEXT ACTION = `s543/resume-after-stop.md`** (rewritten for this stop).  Nothing runs.

## 2026-11-22 14:4x -- s543 CONTINUES (the USER: continue, THREE tasks in parallel, the box is otherwise idle)
* The box was shut down in order and booted at 14:22; the cache is whole (no zero-size file).  `heavy.sh` = the FOUR-SLOT script (`heavy.sh.four-slot-s542`) on the USER's word; a marker line in `HEAVY.queue` separates the stale text.
* **s543f RESUMED** in its worktree (`s543/resume-1122.md`; v2-15500): the rebase onto -104 and every merge bar again; it merges first.
* **s543g LAUNCHED** (`s543/s543g-prompt.md`; IDs 5250-5269; v2-15600): `local` on a nested element (#540 #1190 #3415 #1339), #691 (the raw-slot verdict vs an `s///` replacement), the dropped-handle flush's three holes (#5128 #5160, #4794 shapes 4-5: eight Pod-Simple board rows), #5162 (the counter).
* **s543h LAUNCHED** (`s543/s543h-prompt.md`; IDs 5270-5289; v2-15700): string-eval name resolution (#2630 #2557 #2091 #4821) -- the sweep is its gate.
* The triage check owed for #5230-#5233 #5210 #5211: all six carry `triage` fields (`tools/task-triage.pl`: untriaged 0).  #5210 (`local %ENV` and a child) is NOT in s543g (its `local` group is the nested PLACE); it is a member for the next fix brief.
* PPI batch 12 (the LAST) is written when s543f has merged.  The ROUND is due after s543f's merge (three code merges since the last).
