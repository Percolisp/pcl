---
name: project_startup_parked_aim_functionality_docs
description: USER 2026-10-06 -- the start-up perf items stay parked ("they feel risky"); the present aim is sleek functionality and good documentation
metadata:
  type: project
---

On 2026-10-06 (end of s509), asked whether the start-up items should leave the
parked list (the speed review `docs/speed-review-s509.md` recommended yes), the
USER answered: "Keep the start-up items parked for a bit longer. They feel
risky, we are aiming for sleek functionality and good documentation right now."

Parked: #2422 (the `pcl` launcher), #1862 (`pcl -e` / `-M` runs are not
cached), #2421 (compile policy around string-eval'd code), #2420 (a first run
loads module fasls), #2423 (pl2cl's per-line cost).  First parked 2026-09-27.

**Why:** the USER's words -- they feel risky, and the present aim is sleek
functionality and good documentation.  (My reading, not the USER's words:
changes to the launcher, the core key and the caches can break every run at
once, which is the opposite of polish.)

**How to apply:** do not schedule or re-propose these items until the USER
raises them.  When choosing or proposing work, prefer what makes existing
behaviour correct and tidy and what improves the documentation over new
performance machinery.  The run-time levers of the same review (#2770-#2773)
were NOT part of this answer -- they are ordinary perf-round candidates, still
subject to "no new batch unasked" and the measured-gain rule.
