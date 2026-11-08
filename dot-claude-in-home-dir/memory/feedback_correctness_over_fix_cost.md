---
name: feedback-correctness-over-fix-cost
description: When a correctness fix costs speed on some rows, keep the fix; real-program benchmarking is a later, separate program (USER s536)
metadata:
  type: feedback
---

A correctness fix that slows some benchmark rows is KEPT; do not hold or redesign it for the cost before merging.  The speed question goes into a later, separate effort: serious benchmarking of what is slow in REAL programs (task #4621), not micro rows.

**Why:** s536 (2026-11-08) -- asked whether #4061 (a first bless recorded on the referent; arrobj-new 1.9x, `ref []` after any array bless 2.3x slower, from SBCL's synchronized weak tables) should stay, the USER chose correctness and said the benchmarking of real programs comes later.

**How to apply:** merge the fix, FLAG the cost in the report, and append the measured cost to #4621 as an input -- never propose reverting a correct fix over an INFO-row slowdown.  This does not cancel the speed target ([[project_product_targets_speed_and_ir]], [[feedback_speed_absolute_not_ratio]]): it orders it after correctness and asks for measurement on real programs first.
