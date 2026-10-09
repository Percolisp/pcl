---
name: feedback_cloud_review_ask_user
description: The USER allows Fable to ASK for a `/code-review ultra` cloud review when a batch warrants it (money on the cloud account, 2026-10-09); it is USER-triggered and billed, Fable cannot launch it
metadata:
  type: feedback
---

**Rule (USER, 2026-10-09 09:2x):** "We have some money on our cloud account. If you need such a review the coming weeks, ask me."

**Why:** `/code-review ultra` (the old alias `/ultrareview`) is a multi-agent cloud review of a branch's diff against main, hunting correctness bugs. It is a second reader of the raw diff: no perl oracle, no knowledge of the project's rulings, cannot run the gates. It is billed to the USER's account and only the USER can start it (Fable must never try via Bash). The first trial: branch `review/s513e` (perf round 43, cl/pcl-runtime.lisp +428) in worktree `~/pcl-agent-scratch/s513/review/ultra-s513e`, started by the USER 2026-10-09 ~09:10; Fable compares its findings with the probes before the merge, and that comparison decides whether it joins the merge recipe for compiler/runtime-touching batches.

**How to apply:** when a batch's diff is large in `Pl/` or `cl/` and a second reader would add to the probes, set up a review branch + worktree at the batch's tip (so the agent's own worktree is undisturbed), then ASK the USER in one line: which branch, why, and what it will cost in time. Never assume the answer; never run it yourself. See [[feedback_fable_never_executes_delegate_to_opus]] for the review-vs-execute split.

**Outcome of the first trial (2026-10-09, `review/s513e` = perf round 43):** THREE nits, NO correctness finding -- an indentation, a generic `replace` where `%pcl-str-blit` exists on a hot path (a real rule-11 catch), a docstring boundary. All non-behavioural, so perl-vs-PCL probes could not have found them by construction. The recipe decision waits for two more runs: a batch where the probes DID find something (s513b, #2877) and the next `Pl/`-touching batch at MERGE-READY.
