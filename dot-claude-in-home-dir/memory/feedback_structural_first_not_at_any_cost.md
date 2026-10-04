---
name: feedback_structural_first_not_at_any_cost
description: USER 2026-08-18 — structural progress first (the one-compiler plan, the optimization flag, the duplicate-code extraction), "but not at any cost"; found bugs are FILED, not fixed at the head of the queue
metadata:
  type: feedback
---

**USER (2026-08-18, start of s411):** "All work seems to be finding bugs and
fixing them … for weeks.  The target is to have a flag for turning
optimizations on/off, so it can be done also after the compiler is done.
When will the duplicated code be extracted?  When will the work move
forward?"  Then: "Please go ahead, I'd like to see structural progress.
But not at any cost."

**Why:** four weeks (568 commits) were ~300 bug/feature vs ~50 structural;
the review→execute loop refilled the correctness queue faster than it
drained (each Fable review filed 2–4 new silent-wrongs at the head of the
next Opus queue), and Fable's own sessions were the reviews — so E5 sat in a
queue nobody was at the top of, and the 88.2 % v1-expression number had not
moved since s316t.

**How to apply:**
- The queue is `docs/plan-one-compiler-s411.md` §6: Phase R (registry) →
  A → B → C, with the extraction worklist (#387) and the InterpScan port
  (#388) interleaved; the plan-post-s408 queue resumes after C.
- A newly found silent-wrong is FILED with its reproducer; it jumps the
  queue only if it regresses a baseline or blocks a phase.
- "Not at any cost" = every step names its bar (corpus-diff IDENTICAL for
  refactors; gen bump + sweep + bench for emission changes; whole-corpus
  compile time before/after, 68.4 s at s411) — the harness is unchanged.
- Fable sessions rule the asks and do structure; no cold re-verification
  of a gate Opus already ran.  Related: [[feedback_signoff_rule_simpler_faster]],
  [[feedback_hard_parts_first_e2_for_opus]].
