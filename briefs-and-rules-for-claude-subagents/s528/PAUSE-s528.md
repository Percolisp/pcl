# PAUSE-s528 -- running notes (Fable, 2026-10-25).  Resume recipe = the LAST block.

## 19:1x -- SESSION START; USER: "Please continue."  The s527 resume recipe applies as written.
- State verified: main `99254f40` clean, pushed, CI green on every -47 commit (six runs, all success); `HEAVY.holder` EMPTY; no stray sbcl/prove; **the box rebooted** (uptime 2 min at 19:17) so nothing of s527 survived in /tmp; `git log b89b6add..main -- Pl cl lib tools baselines vendor` EMPTY (both agents' rebase is notes-only).
- Both agents are CHECKPOINTED, not merge-ready: s526w2 `d34b0f91` (10 commits; OWED = bless Pod-Simple, census Log-Log4perl + Template-Toolkit, M2, records); s527a `fdf7f729` (12 commits; OWED = the gate-cost-01 leak + gate, re-run legs, records).  Six merged agent worktrees + six review worktrees have nothing not on main (checked per branch) -- to remove (step 5).
- The gate-cost-01 leak DIAGNOSED from the test + the loader and RULED in `s528/resume-s527a.md`: the "other" tree symlinks `Pl/` not `vendor/`, so `abs_path(__FILE__)` resolves the loader to THIS tree; the fix is `program_inc` dropping `$Pl::VendorPPI::LIB` (the loader = ONE source of the compiler's lib), never a `vendor` symlink in the test.
- Written: `s528/SHARED-BOX.md`, `s528/resume-s526w2.md`, `s528/resume-s527a.md`, this file.  Next: launch both (opus, no isolation), start the round-5 FULL companion on the quiet box through heavy.sh (exclusive), read #3860 for its ruling, remove the stale worktrees.

## RESUME RECIPE for s529 (draft; the last block supersedes)
1. `cat ~/pcl-agent-scratch/s513/HEAVY.holder`; clear dead pids; CI check.
2. The two agents: `scratch/<label>/STOP.md` in `agent-a3ad79b740a76bd47` (s526w2) and `agent-a4a4e19ab7b7a3ad0` (s527a); MERGE-READY -> merge (either order; the other rebases); checkpointed -> resume from an `s529/resume-<label>.md` written like these.
3. Merge gate per batch = `s528/SHARED-BOX.md` MERGE paragraph.
4. After s527a merges: write s527b + s527c from review doc §6 + §8; then s527d.  After s526w2: #3650 / C4-C7 / a correctness batch (#3824 first; #3860 after Fable's ruling).
5. Round-level: the full companion result (this session's run, `~/pcl-agent-scratch/s528/companion-full.log`) -- its NOT-RUN STAMPS, ROW DIFF and SHORTFALL blocks read; stamps blessed s528.
