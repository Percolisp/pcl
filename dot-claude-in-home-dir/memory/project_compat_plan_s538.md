---
name: project-compat-plan-s538
description: The USER-approved plan for the compatibility half of the coming weeks (s538, 2026-11-11) -- six ordered points; point 4 is three USER decisions to raise when reached
metadata:
  type: project
---

The USER asked (s538, 2026-11-11) how to raise perl compatibility in the coming weeks, with about
half the time on it and half on functionality (XS etc.), approved the plan below, and asked to be
TOLD when point 4 is reached.

1. **Whole-file losses on the CPAN board first** -- files that stop early (`*PLAN*` / `*FILE*` rows;
   ~56,000 assertions never reached behind 104 files at the 2026-11-01 accounting), one cause each;
   then the board's remaining "ours" rows.
2. **Silent wrongs before loud ones** in triage: wrong answer at exit 0 > dropped statement > unsupported.
3. **One slot per round on the parser program + the PPI branch** (#3694: port 4 the renames, the next
   statement kinds off the v1 text seam, then the keyword table; PPI batches from the expect checker).
4. **Three USER decisions that block application-shaped code** -- RAISE THEM WITH THE USER when the
   work reaches them, do not decide: scope-exit DESTROY (#2370), `caller` fidelity (#233, with #1361),
   the malformed-UTF-8 string representation (#1389).
5. **Widen the board with app-shaped dists**, picked by pure-Perl reverse dependencies, a row census first.
6. **Round instruments on schedule** (board + companion `--all --quick`).

Where it meets the XS half: write a shim only for Encode; leave Storable / Digest::MD5 / File::Glob
to the XS bridge.  Not under this budget: chasing perl's own suite rows, the method-lookup speed batch.

**Why:** the queue is a frontier; whole-program blockers and silent wrongs are what stand between
"my own scripts" and "pure-Perl CPAN apps" ([[project_cpan_board_state]], docs/production-readiness-review-s513.md).

**How to apply:** each new brief names which point it serves; when a batch or a board hunt lands on
#2370, #233 / #1361 or #1389, stop designing and tell the USER it is point 4 ([[feedback_paraphrase_user_in_records]]).
