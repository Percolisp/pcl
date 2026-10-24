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

## RESUME RECIPE for s528 (draft; the last block supersedes)
1. `cat ~/pcl-agent-scratch/s513/HEAVY.holder`; clear dead pids.
2. The two board agents: read `scratch/<label>/STOP.md` in `.claude/worktrees/agent-a8804ba6e7379e0f6` (s526w1) and `agent-a3ad79b740a76bd47` (s526w2); MERGE-READY -> merge (w1 FIRST; then tell w2, which drops its copy of the `local $/` line and regenerates the artifacts); checkpointed -> resume as fresh opus agents from an `s528/resume-<label>.md` written like `s527/resume-s526w1.md`.
3. First freed slot: **launch s527a** (`s527/s527a-prompt.md`, worktree isolation from main, opus).  Before it, the USER creates the fork `Percolisp/PPI` (or the agent leaves the branch + format-patch series under `s527/upstream/`).
4. The other slot: the PPI-registry filler (#3740-#3755 rows §39-§54) is now written WITH the fix-on-the-branch option in mind -- hold it until s527a has landed M3 (the vendored lib) so its rows can carry `fixed_in => 'pcl'`; else #3650, C4-C7, a correctness batch.
5. Round-level duties unchanged (round-5 board on the merged main; the full companion with `--bless-stamps`).
