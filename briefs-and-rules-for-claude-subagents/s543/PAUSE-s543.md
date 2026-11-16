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

## 22:34 -- THE BOX WENT DOWN under both batches; 22:5x both RESUMED (`s543/resume-s543.md`)
* Cause as far as the journal shows: the previous boot ends in `amdgpu` DMCUB display errors and a gnome-shell restart; no OOM kill, no thermal event.  Three bounded legs ran (s543a board-both + gate-set-scan, s543b Path-Tiny), 2 GB used of 12.
* Intact: `git fsck` clean, main `05e9867f` unchanged, s543a at `cd04e308` (WIP 5, clean tree), s543b at `f014f5e2` (WIP 6/7) + an uncommitted 17-line member-7 draft, both STOP.md current to 22:31, the tasks both filed (5101 5102 5104 5105, 5120-5125, appends to #398 #1291) valid JSON.
* **DAMAGED: the module cache.**  A crash leaves files with their FINAL name and NO content: a zero-size core (`pcl-5d366543-…core`), a zero-size extension fasl, ~40 zero-size `proto/*.json`.  Removed: every cache file written from 22:28 on, every zero-size one, and the sibling files of each removed entry (239 files, 23 stems).  The write-to-temp-then-rename in `docs/caching.md` protects against a RACE, not against a power loss (no fsync before the rename) -- worth a task if it recurs: a reader that finds a zero-size core / fasl / proto file should treat it as a miss.
* VOID by the END-line rule: s543a `board.log`, `gss.log`, `sweep.log`; s543b `pt-after.log`; s543b's member-6 gate never started.
* `HEAVY.holder*` cleared (stale text; the flocks died with their holders).  New rule for this sitting: at most TWO bounded legs per batch queued or running (s543a held two slots and a third leg while s543b's stood in the queue up to 20 minutes).
* Resumed as two new Opus 5.5 / xhigh agents in the SAME worktrees (no new worktree); MODEL.txt gets a `resumed` line -- to check.
