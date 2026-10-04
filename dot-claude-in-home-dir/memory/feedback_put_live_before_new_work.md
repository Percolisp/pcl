---
name: feedback_put_live_before_new_work
description: USER (2026-09-05 evening, session restart) — merge and push verified work ("put things live") BEFORE starting any new work; new briefs wait until the awaiting-merge batches are on main and pushed
metadata:
  type: feedback
---

USER, 2026-09-05 at the s471 restart: "please put things live before doing more work."

**Why:** four finished batches (BR/BQ/BP/BS) sat in worktrees at the s470 shutdown; work that is not on main and pushed is not delivered, and every further batch that lands on top makes the rebases harder.

**How to apply:** when a session starts with batches awaiting merge, the ONLY agents launched until they are live are the verification/merge agents for those batches (sweep, companion, rebase); ff-merge each as soon as its bars are read, push, and only then launch new briefs (BT/BU/#1261…).  Related: [[feedback_commit_per_session]], [[feedback_two_agents_at_a_time]], [[feedback_fable_never_executes_delegate_to_opus]].
