---
name: feedback_speed_absolute_not_ratio
description: USER (2026-09-05) re-based the speed metric — PCL's ABSOLUTE time on real programs, not per-row pcl/perl ratios; stop chasing rows that already beat perl, profile macro programs and take the ranked table from the top
metadata:
  type: feedback
---

USER, 2026-09-05 (s470), twice: "there are enough features faster than Perl that we don't
need to stare at those larger than 1×" and, sharper, "forget beating Perl in individual
items, we do enough for that. Just try to get PCL as fast as possible."

**Why:** the bench board's pcl/perl ratios were steering the perf agents toward whichever
micro-row looked worst against perl; a real program's time is a mix plus constant terms
(extension load ~3 s for `pack`, module load, the string-eval compiler spawn) that no ratio
row shows, and a hot path already "faster than perl" can still be where the cycles go.

**Corrections the same day (USER): `pack`/`unpack` are PARKED until the XS decision (pclxs may carry them — not a compiler target); a lever's rank carries AT LEAST HALF its weight on how low-hanging it is (ease of implementation), the rest on seconds removed; and the IR is targeted for EASE of implementing a backend AND for a FAST implementation together (plan Part B §B.3: facts printed on the general form, not PCL's rewrites).  Standing authorization (2026-09-05, "start new subjobs when the present ones are done"): launch the next round's agents as slots free, max 3, without asking; sharpened later the same day: "when a subjob finishes, just create new ones" — launch the successor IMMEDIATELY on a finish report, before its review/merge; review and merge run in parallel with the new agent (the successor rebases onto whatever main becomes).**

**How to apply:** rank levers by (profile share on macro programs × generality) / cost.  The
plan is `docs/plan-speed-and-ir-s470.md` Part A: three macro rows (`json-rt`, `moo-objs`,
`textproc`) + five constants measured at N and 2N + an `sb-sprof` profile per row → a RANKED
table; rounds take it from the top.  The ten winning board rows are CONTROL rows only.  Never
brief a perf agent with "row X is N× slower than perl" as the goal; brief it with seconds
removed.  Supersedes the "speed must BEAT perl" phrasing in [[project_product_targets_speed_and_ir]]
(the target is now "as fast as possible", measured absolutely).
