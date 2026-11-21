---
name: project-ppi-batches-until-done-s543
description: "USER ruling 2026-11-21 (s543) -- the remaining PPI tasks are run as PPI batches, ONE at a time, back to back, until none is left"
metadata:
  node_type: memory
  type: project
  originSessionId: 039d94b0-4446-4259-9602-4e3c9b34ba84
  modified: 2026-11-21T14:55:46.977Z
---

The USER ruled on 2026-11-21 (session s543), after asking whether the PPI work was finished and hearing that about a dozen PPI tasks were still open: run the PPI tasks one batch at a time until they are done.

**Why:** few are left, and PCL's patched PPI (branch `pcl`, upstream pull request #459) is the part of the project other people can see; the USER wants the list emptied rather than one PPI slot per round (the s538 plan's point 3 pace).

**How to apply:**
- One PPI batch is always running or next in line until the list is empty; never two PPI batches at once (they share the vendored branch and the fix-group letters).  It takes one of the two subtask slots ([[feedback_two_agents_at_a_time]]) -- my reading; the USER did not say a PPI batch may run as a third.
- The plan: batch 10 = `briefs-and-rules-for-claude-subagents/s543/s543d-prompt.md` (#2095 float then `-DIGIT`, #483 `1<<index(` heredoc, #937 empty bare block, + the AUDIT TABLE of the ten `_repair_*` / `_fix_ppi_*` passes still in `Pl/`).  That table (it lands in `docs/ppi-fix-at-source-s527.md`) is the plan of batches 11 and 12: the word-match family (#458 #2774, registry section 11), the glob family (#651 #2384), then #3922 #2670 #1680 #1666 #1876, the tooling closes #357 #3805 #4903, and the deletion of every repair pass the audit finds dead.
- Not PPI-branch items although they name PPI: #1580 (a lowering), #4807 (PExpr), #1053 (a loud refusal), #4902 (awaits a ruling), #1649 / #327 family (proposed OUT, a USER decision).
- "Done" = no open task whose fix belongs on the PPI branch, registry section 11 fixed at the source, and every repair pass either deleted or justified in the audit table.  After each PPI merge the reviewing session pushes the fork and refreshes pull request #459 ([[feedback_push_at_fable_discretion]]).
- The fork's front page shows `master` (= upstream); the work is on branch `pcl`.  Making `pcl` the default branch is a GitHub repository setting only the USER can change; whether #459 is split into per-group requests is the USER's open decision.
