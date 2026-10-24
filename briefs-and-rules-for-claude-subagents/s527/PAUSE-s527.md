# PAUSE-s527 -- running notes (Fable, 2026-10-24 evening).  Resume recipe = the LAST block.

## 18:4x -- SESSION START; USER: "So start subjobs for the runtime and do a review of PPI, if we can just pull that and fix it. :-)"
- Read as: resume the two parked board agents (s526w1 `d76a8d04`, s526w2 `65a1a740`; both NOT merge-ready) as fresh opus agents, no isolation, in their worktrees; Fable does the PPI question (`s526/PAUSE-s526.md` 16:5x) with numbers and puts it to the USER before s526a is launched.
- State verified at start: main `27e30d07` clean, pushed; `HEAVY.holder` empty; no stray sbcl; `git log 706cfde8..main -- Pl cl lib tools baselines` EMPTY (both agents' rebase is notes-only).  Briefs: `s527/SHARED-BOX.md`, `s527/resume-s526w1.md`, `s527/resume-s526w2.md`.

## 18:5x -- both board agents RESUMED (opus, no isolation, in their worktrees); Fable's PPI review begun
- s526w1 + s526w2 launched from `s527/resume-s526w[12].md`; the standing default of two subtasks is full -- s526a / s527a wait for a freed slot.
- The PPI review's materials: upstream clone + all 244 PR heads at `scratchpad/ppi/upstream` (session-local, `/tmp/claude-1000/.../scratchpad/ppi/`), the prototype worktree `ppi-proto` (branch `proto`), `report-rows.pl` (runs `docs/ppi-bug-report.t` with `-I <lib>`), `emit-ab.sh` (PCL emission A/B installed-vs-proto PPI).

## 19:xx -- THE PPI QUESTION ANSWERED: fix at the source.  USER RULED (three messages): patched PPI YES; "Don't make specific patches for a certain version of PPI"; "make certain not to break the existing tests for PPI + add our test file in docs/ppi-bug-report.t, when we put on Github"
- Findings (all in `docs/ppi-fix-at-source-s527.md`): PCL carries 1,313 repair lines for the 28 token rows; `_ppi_parse` 0.95 s vs 0.25 s plain parse (Calc.pm); upstream = 101 unmerged toddr-bot PRs (April 2026), 22 merged since 2024, the USER's own issues #449 / #451 (June) unanswered, releases in bursts; 8 pending PRs merged -> report 25 ok but a plan breaks, 2 conflict.  PROTOTYPE: 12 sections, 54/8 lines, 7 files, PPI suite PASS (+ its TODO "(1)-2" passes), report 14 -> 33, canary flags exactly the 12.  The prototype patch is saved at `s527/ppi-proto.patch`.
- Written: the review doc; DECIDED ## s527; session-log ## Session 527; CLAUDE.md pointer; task #3697 (umbrella) + #3694 note; **brief `s527/s527a-prompt.md`** (IDs 3920-3939) = the next launch in the parser slot -- s526a is SUPERSEDED for token rows (not launched; its structure-row half deferred).
- The end-to-end A/B (PCL emission, installed PPI vs proto, perl-tests + everyday; the `@INC` lines excepted) -- result filled into the review doc §3 when it ended.  RESULT: 233 of 233 files byte-identical, 0 real diffs (review doc §3).  Committed `12e17e21`+amend, pushed.

## 19:4x -- THE PLAN ACCEPTED (USER: "Seems like a good plan. Accepted.")
- Order: board merges (w1, w2) -> s527a ALONE in the parser slot (+ a filler in the other: the §39-§54 registry rows) -> s527b (tokenizer rows) PARALLEL with s527c (Lexer rows) -> s527d (the 16 new) -> the parser program resumes (1c, 1b, C1).  ~two weeks of PPI in the development slots.  Recorded: DECIDED ## s527 (the plan line), review doc §8, session-log, memory STATE.  Fable writes the s527b / s527c / s527d briefs when s527a is MERGE-READY (their row lists = review doc §6 + §8).

## RESUME RECIPE for s528 (draft; the last block supersedes)
1. `cat ~/pcl-agent-scratch/s513/HEAVY.holder`; clear dead pids.
2. The two board agents: read `scratch/<label>/STOP.md` in `.claude/worktrees/agent-a8804ba6e7379e0f6` (s526w1) and `agent-a3ad79b740a76bd47` (s526w2); MERGE-READY -> merge (w1 FIRST; then tell w2, which drops its copy of the `local $/` line and regenerates the artifacts); checkpointed -> resume as fresh opus agents from an `s528/resume-<label>.md` written like `s527/resume-s526w1.md`.
3. First freed slot: **launch s527a** (`s527/s527a-prompt.md`, worktree isolation from main, opus).  Before it, the USER creates the fork `Percolisp/PPI` (or the agent leaves the branch + format-patch series under `s527/upstream/`).
4. The other slot: the PPI-registry filler (#3740-#3755 rows §39-§54) is now written WITH the fix-on-the-branch option in mind -- hold it until s527a has landed M3 (the vendored lib) so its rows can carry `fixed_in => 'pcl'`; else #3650, C4-C7, a correctness batch.
5. Round-level duties unchanged (round-5 board on the merged main; the full companion with `--bless-stamps`).

## 20:2x -- MAIN-READY-47 = s526w1 MERGED `584c1ca6` (+ Fable's records commit on top)
- Merge gate on the rebased tree: gate PASS 336 / 11,462 (= the agent's); corpus-diff vs `d0f53144` = 1 of 111, `range.t` (the #1025 shape; probed perl -> base `1E0` -> tree `a:b:c:d:e`), drops 5 unchanged, shapes identical; emission-ab `lib/**/*.pm` 15 SAME; the agent's sweeps read clean twice (TOTAL 18,761 +0); everyday 116 = 116; four probes perl -> base -> tree (#3832 slurp, #1025 s///e list context, #3825 context bind, the Clone shim): base wrong or dying, tree = perl.
- `perl-tests/range.t`: the three s214 `ok(1, 'SKIP: ...')` rows restored to their original assertions (`is ("@a", $a)`, `is ("@b", $b)` were stale on main too -- string-eval range context has worked for a long time; `is ($x, 'a:b:c:d:e')` passes since #1025) -- one-file sweep 158 passing / 0 failing / 4 skipped, fully passing.  Four-conjunct rule: perl-probed text (`prange2.pl`), the diff is exactly the three rows, the edit STRENGTHENS, the rows themselves are the guard.
- Filed #3698 (PRE-EXISTING, found by the probe): an escaped delimiter `\/` inside an s///e REPLACEMENT is not unescaped before the code compiles (`split(\/\/, $1)` dies); #2704 is the string-side sibling.
- DONE 20:3x: s526w2 told (message queued); **s527a LAUNCHED** (opus, worktree isolation from `b89b6add`) -- the plan's step 1 is under way; the other slot stays with s526w2 until its MERGE-READY.
