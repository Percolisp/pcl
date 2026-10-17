---
name: project_round_level_cadence_s519
description: USER ruling s519 (2026-10-17) -- the CPAN board and the companion --quick run once per ROUND by Fable on the merged main, not per batch; evaluation due ~s522 (task #3361)
metadata:
  type: project
---

**ROUND-LEVEL CADENCE (USER, 2026-10-17 23:3x; DECIDED ## s519):** the two SLOW instruments -- the CPAN board tree half (~20 min) and the companion `--all --quick` (15-25 min, exclusive) -- are no longer per-batch agent legs.  Fable runs both ONCE PER ROUND on the merged main (a review worktree at main's sha; logs `~/pcl-agent-scratch/s513/review/round-<N>-board.log` / `round-<N>-companion.log`) after every 3-4 merges, or at session end if any merge happened since the last round run.  A mover is attributed by `git bisect` over the merge range with a one-file run per step; Fable edits the row with its cause or hands it to the next brief.  Per batch stays: gate, sweep (per the WHAT-TO-RUN-WHEN table), corpus-diff + emission-ab + ir-host-leak + ir-conform, everyday + `--record`, and ONE-FILE acceptance runs of the exact board / companion files a member claims (a FIXED blessed row leaves by edit on that run's strength).  Exceptions keeping a per-batch companion: a HARNESS change; a bounded leg on files the brief NAMES.

**Why:** legs + attribution cost about one agent per batch (s518d: three agents for four members, two legs-only; s518e: 450k tokens before any leg); the per-batch instruments that stay take ~10 min and attribute trivially.  A regression in the two slow instruments may sit on main for one round -- accepted, they are baselines, not the gate.

**How to apply:** write NO board / companion leg into a brief (except the two exceptions); Fable's round run is a STANDING duty with no skip (round 1 = after s518d + s518e merge); **EVALUATE at ~s522 = task #3361** (questions + the s518 baseline are in the task).  See [[feedback_sweep_cadence]] for the table it amends.
