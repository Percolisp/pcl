# PAUSE-s528 -- running notes (Fable, 2026-10-25).  Resume recipe = the LAST block.

## 19:1x -- SESSION START; USER: "Please continue."  The s527 resume recipe applies as written.
- State verified: main `99254f40` clean, pushed, CI green on every -47 commit (six runs, all success); `HEAVY.holder` EMPTY; no stray sbcl/prove; **the box rebooted** (uptime 2 min at 19:17) so nothing of s527 survived in /tmp; `git log b89b6add..main -- Pl cl lib tools baselines vendor` EMPTY (both agents' rebase is notes-only).
- Both agents are CHECKPOINTED, not merge-ready: s526w2 `d34b0f91` (10 commits; OWED = bless Pod-Simple, census Log-Log4perl + Template-Toolkit, M2, records); s527a `fdf7f729` (12 commits; OWED = the gate-cost-01 leak + gate, re-run legs, records).  Six merged agent worktrees + six review worktrees have nothing not on main (checked per branch) -- to remove (step 5).
- The gate-cost-01 leak DIAGNOSED from the test + the loader and RULED in `s528/resume-s527a.md`: the "other" tree symlinks `Pl/` not `vendor/`, so `abs_path(__FILE__)` resolves the loader to THIS tree; the fix is `program_inc` dropping `$Pl::VendorPPI::LIB` (the loader = ONE source of the compiler's lib), never a `vendor` symlink in the test.
- Written: `s528/SHARED-BOX.md`, `s528/resume-s526w2.md`, `s528/resume-s527a.md`, this file.  Next: launch both (opus, no isolation), start the round-5 FULL companion on the quiet box through heavy.sh (exclusive), read #3860 for its ruling, remove the stale worktrees.

## 19:3x -- both agents RUNNING (s527a waits on a leg queued behind the companion); #3860 RULED; s527b + s527c DRAFTED
- #3860 (`use base` pushes @ISA at run time): the filing's own discriminating probe TAKEN -- `BEGIN { push our @ISA, ... }` survives a cached run (run 1 = run 2), because the script fasl is built under `*pcl-fasl-build*` where p-BEGIN = load+execute (fires at the fasl's load in source order).  RULED in the task: the push goes inside `p-BEGIN` at its source position, hoists unchanged; the full sweep is its gate; a correctness batch AFTER s526w2 merges.  Layer note recorded for C2 (base.pm is plain Perl).
- `s528/s527b-prompt.md` + `s528/s527c-prompt.md` DRAFTED (DRAFT comment at the top names what to fill at s527a's merge).  New in them: the shared `pcl` branch gets a HOME, the bare repo `~/pcl-agent-scratch/ppi/PPI.git` (Fable creates it from s527a's clone at the merge); each agent works on `pcl-<label>`, rebases onto `origin/pcl`, pushes fast-forward, re-vendors; b = Tokenizer/Token/Include files, c = Lexer/Document/Statement/Structure; a cross-file need is a HAND-OVER, never a side taken.  s527b's M1 = the term-end rule (method name after `->`, postfix `++`/`--`, a subscript's `}`/`]`), then the eight deletions.  IDs: b 3940-3959, c 3960-3979, d 3980-3999 (3880 is taken).
- Twelve stale worktrees removed (six review, six merged agents; `agent-a2e094e8f372e5f03` = #3650 checkpoint kept).

## RESUME RECIPE for s529 (draft; the last block supersedes)
1. `cat ~/pcl-agent-scratch/s513/HEAVY.holder`; clear dead pids; CI check.
2. The two agents: `scratch/<label>/STOP.md` in `agent-a3ad79b740a76bd47` (s526w2) and `agent-a4a4e19ab7b7a3ad0` (s527a); MERGE-READY -> merge (either order; the other rebases); checkpointed -> resume from an `s529/resume-<label>.md` written like these.
3. Merge gate per batch = `s528/SHARED-BOX.md` MERGE paragraph.
4. After s527a merges: write s527b + s527c from review doc §6 + §8; then s527d.  After s526w2: #3650 / C4-C7 / a correctness batch (#3824 first; #3860 after Fable's ruling).
5. Round-level: the full companion result (this session's run, `~/pcl-agent-scratch/s528/companion-full.log`) -- its NOT-RUN STAMPS, ROW DIFF and SHORTFALL blocks read; stamps blessed s528.
