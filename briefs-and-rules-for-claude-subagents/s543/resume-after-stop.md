# s543 -- HAND-OVER at the session's end (2026-11-25 00:2x; the USER ended the session) -- NEXT ACTION for the next reviewing session

Nothing runs.  `HEAVY.holder` is empty, no SBCL, no lane or watch loop.  Main = `origin/main` = MAIN-READY-110 `26d0dac4`
(CODE, generation v2-16000) + record commits.  Numbers = CLAUDE.md "Test Status".  Read `s543/PAUSE-s543.md` bottom-up for the
evening (the sections from "2026-11-24 20:1x" on).  (The hand-over of 2026-11-23 that stood in this file is in git:
`git show 8ee15299:briefs-and-rules-for-claude-subagents/s543/resume-after-stop.md`.)

**THE USER's standing words at the end:** (1) **start NO MORE subtasks after the three that ran -- ask before launching
anything, also before resuming the three stopped batches**; (2) **the Rosetta corpus is the next session's MAIN work, to be
started seriously** (`docs/rosetta-plan.md`, accepted, its fresh sample included); (3) exit criterion 1 counts silent wrongs of
ORDINARY code, a probe-found neighbour does not extend a batch unless it is ordinary (`docs/supported-subset.md` 5 and 7,
`s473/COMMON.md` "ORDINARY OR RARE").

## What merged on 2026-11-24
| MAIN-READY | sha | batch | what | gate on merged main |
|---|---|---|---|---|
| 110 | `26d0dac4` | s543k | six live silent wrongs: `evalbytes` is a string eval, a span-promoted lexical's eval reach, the eval text's own `our`, the drop flush inside an eval text, `postderef_qq` in a replacement, `${1}` in code; #5272's compile cost 83.5 -> 45.9 s | 380 / 12,490 |
| (records) | `c5c87f33` | s543s, THE ROUND | board on -108: 670 PASS / 136 PARTIAL / 23 FAIL / 67 SKIP, 111,258 ok / 1,188 not ok; companion `--all --quick` on -108: 130 OK / 103 XDIFF / 237 DIFF; the FULL companion run on -110: 131 OK / 104 XDIFF / 240 DIFF, 0 NEW / 1 FIXED / 0 LOST | 380 / 12,490 at the SECOND run (tap-assert-01.t row 39 flaked under load: #4904) |

Sweep TOTAL 18,808, GATE clean; EVERYDAY 116 of 122; `tools/task-triage.pl` at the stop: **20 ordinary + 67 rare** live IN
silent wrongs (21 + 64 at the first pass; the batches moved #5121 to rare and filed new rare ones).  CI green through the
s543k merge push; **check the runs of the later record pushes (both workflows)**.

## THREE batches are STOPPED ON ORDER at clean checkpoints (worktrees clean, WIP committed, STOP.md current, all Opus 5.5)
All three sit on main's CODE `26d0dac4`; each owes a records rebase onto main.  **MERGE ORDER: s543n, then s543m, then s543l**
-- the two later ones rebase onto each merge before them, keep their generation, regenerate the three artifacts and re-take
the full gate, the sweep and the wide A/B (their other instruments stand when the A/B's DIFF set is unchanged).

| label | worktree (under `.claude/worktrees/`) | HEAD | generation | state |
|---|---|---|---|---|
| **s543n** (the filehandle family) | `agent-a1fb5119fa665c62c` | `572a46dc` (code `0d5a13e9`), 14 over `617d66c8` | v2-16300 | FIXED: #1423 (with #4844 #5081: one name resolver), #4846, #2594, #683, #4842, #5122, #3451 (through the path the handle was opened on; no foreign call).  MEASURED, not fixed: #5121 (0 re-opens through a copy in 30 dists + everyday + perl-tests + Rosetta + 755 core modules: now `rare`), #513 (filed #5420), #5123 (documented not-supported).  TAKEN: full gate 380 / 12,499 PASS on `a7364c75` and on `0d5a13e9`; sweep 18,808 +0 GATE clean; corpus-diff identical, wide A/B 1 DIFF / RCDIFF 0; ir-conform 337 / 0; companion 23 files, four movers (io/argv.t 30/23 -> 32/21, io/pipe.t 21/6 -> 23/4, op/fh.t 6/2 -> 8/0, op/magic.t 178/30 -> 179/29), seven fails rows out by edit; board six files unchanged; EVERYDAY 116 -> 116.  **OWED: the rebase, the timing leg (`legs/bench.sh`), the bench line in the session-log, `--record`, MERGE-READY.**  REVIEWED: 24 probes, the tree = perl on 22 (`~/pcl-agent-scratch/s543/review/probes-1124/rn/`; re-run `leg.sh` on the final tree: n-08 was fixed after the run).  Filed #5420-#5424 (rare).  A flag for the reviewing session: is #3451's remembered-path route "platform-touching" (the install matrix + a macOS leg)? |
| **s543m** (#5310 + the string-eval residue) | `agent-ac39a725d1d338842` | `bfa405a5` (code `d6b84d2f`), 14 over `36e872cc` | v2-16200 | FIXED: #5310 (the file-level face; the Prototype candidate test), #5374, #5350, #5351, #5353 (the raw-number half; a boxed number stays #551), **#5354 + #5405 in full** (a cleared container, a void `pop` / `shift` / `splice`, `ELEM = undef`, a sub-tail `delete` in a void caller), **#5402** (a qr's `u` flag under `unicode_strings`).  MEASURED, not fixed, FLAGGED: #5352 (the cure would move 105 identity cells in 40 files).  TAKEN on `c782fca7`: full gate 380 / 12,523 with `ir-inventory-01.t` rows 2-3 (the inventory regenerated in `bfa405a5`; **a full gate on the final tree is OWED**), sweep 18,808 +0 GATE clean, wide A/B 304 DIFF / RCDIFF 0 (all member 7's flush forms and #5402's flag), corpus-diff 31 of 111, census identical, ir-conform 337 / 0, gate-set scan identical, companion 23 files (re/pat.t 219/86 -> 279/14, spliced).  **OWED: the rebase onto s543n's merge + artifacts, the gate / sweep / A/B again, everyday, the timing leg (member 7's and #5402's cost numbers: FLAG them), the session-log's BARS_LINE, `--record`, MERGE-READY.**  NOT REVIEWED YET.  Review points: the #551 canary of `Pl/t/prefix-incr-deref-01.t` was REWRITTEN (read the diff: strengthened or weakened?); the sub-tail flush "only in a void caller" (`:tail`); 304 A/B sites for member 7 -- sample them.  Filed #5400-#5405. |
| **s543l** (the `local` residue + the signal member) | `agent-a7d0dcd17f38738f0` | `e3591625`, 12 over `36e872cc` | v2-16100 | FIXED: #5372 #5371 #5331 #5334 #5332 #5333 #5330 (its ternary / short-circuit arm stays a loud refusal, designed in the task) and **#5380 + #2260** (a `die` from a signal handler is no longer swallowed in `kill`, `readline`, `read`, `sysread`, `getc`, `eof`, `accept`, `recv`: 18 of 18 blocking primitives = perl, base 8).  THE RULING APPLIED (told to the USER, no objection): `Pl/t/fileio-02.t` row 12 rewritten to the control flow and `$@`, canary row 12b pins the read target's old value (#5383, rare).  Its own regression caught by the companion and fixed (`local %INC = %INC` and a failed load's undef entry).  TAKEN on the final code: full gate 380 / 12,507 PASS, sweep 18,808 +0 GATE clean, ir-conform 337 / 0, wide A/B 16 DIFF / RCDIFF 0, gate-set scan and census identical; companion BEFORE.  **OWED: the rebase onto the merges before it (expect meetings in `%p-read-record` and the read helpers with s543n) + artifacts, the gate / sweep / A/B again, companion AFTER (VOID), everyday, the timing leg, the session-log section (draft in its scratch), `--record`, MERGE-READY.**  NOT REVIEWED YET.  Review points: the eight re-signalling arms (rule 12 read in its STOP.md); row 12 and 12b; `local %SIG` under a real signal; `local %INC` + a second `require`.  Filed #5380-#5383. |

* **RESUME** = a new Opus 5.5 / xhigh agent IN THE SAME WORKTREE per batch (no new worktree), started from the main checkout,
  as `s543/resume-1124.md` did: "read `scratch/<label>/STOP.md`, the brief `s543/<label>-prompt.md`, the WIP commits; finish
  the OWED list"; MODEL.txt gets a `resumed` line -- check it.  Each STOP.md carries the reviewing session's messages that
  changed its brief.  At each merge: probes perl -> main -> tree, fast-forward, the FULL gate on merged main through
  `heavy.sh` (a flaked row is re-run alone AND the gate re-run: it must end PASS), records, CLOSE-WITH-ROWS, ONE message to
  each other batch.
* **Closes at the merges** (the ordinary count goes 20 -> about 12): s543n #1423 #4844 #5081 #4846 #2594 #683 #4842 #5122
  #3451; s543m #5310 #5374 #5350 #5351 #5354 #5405 #5402 (#5353 narrowed, #5352 stays); s543l #5372 #5371 #5331 #5334 #5332
  #5333 #5380 #2260 (#5330 narrowed).

## THE NEXT SESSION, in this order
1. **Ask the USER before any launch** (the three resumes and the census alike).
2. Resume and merge s543n, s543m, s543l (above).
3. **ROSETTA -- `docs/rosetta-plan.md`**, step 0: re-run the corpus on the merged main
   (`~/pcl-agent-scratch/s543/rosetta/run.sh`; 547 of 652 on -110: 43 silent, 87 of the 105 differing inside the subset),
   then write the two census briefs (A the silent programs, B the rest; one task per CAUSE, `frequency: ordinary` with the
   program as evidence, the cause table `baselines/rosetta-causes.tsv`) and the small instrument brief; then the fix batches
   ranked by programs broken.  The fresh sample (Perl Weekly Challenge) is fetched only at the plan's step 5.
4. The ordinary silent wrongs no batch holds (first pass): #971 #1040 #1494 #1956 #2440 #2500 #2560 #3101 #3616 #3695 #4843
   #4941 -- one slot beside the census, a brief not written.  The first-pass split was made from SUBJECTS: a task moves from
   `rare` to `ordinary` on the evidence of one real program.

## Owed by the reviewing session beside that
* Tooling debts: #5163 (265 rows keyed on 16 closed tasks: about 300 baseline rows whose cause is unverified), #5164, #5190,
  #5291, #5335; #4904 (the gate flake: make `run_cl` in `Pl/t/tap-assert-01.t` keep pl2cl's stderr and exit status).
* For the USER, open: #1876 (`extra_paired_delimiters`: s543i proposes OUT); the PPI branch items #1686 #650; the 39
  `decision` tasks -- three of them hold 955 failing rows (#3670 a stash alias, 388 board rows in one file; #1664 the typeglob
  model, 342 rows; #332 refaliasing, 193); 6c / 6d / 6e of `docs/supported-subset.md`; the cost flags (s543g's two, s543j's
  `local` slice tail; s543m's member 7 is unmeasured until its timing leg).
* After a crash: remove every `~/.pcl-cache` file written in the last minutes and every zero-size one (not the `.lock`
  files) BEFORE any leg (`s543/resume-s543.md`).
