---
name: feedback_review_doc_batch_status
description: USER (s523, 2026-10-20) -- keep docs/cpan-board-failure-review-s521.md's batch statuses current at the END of every session while the batches are being worked through; the doc is the queue the USER reads
metadata:
  type: feedback
---

**The USER (s523, 2026-10-20 22:3x):** "It would be nice if this was kept updated at the end of sessions, when we work through the list: `docs/cpan-board-failure-review-s521.md`".

**Why:** the review doc's §4 is the plan the USER steers by (batches 0-6 with their member tasks); when a batch merges, its heading line says `**DONE sNNNx = MAIN-READY-N**` (batches 0-2 had that; batches 3-5 were merged or in flight without the mark until s523).  A stale plan makes the USER re-derive the queue from the session log.

**How to apply:** at every session end (and at each merge, when cheap), edit the batch heading lines in `docs/cpan-board-failure-review-s521.md` §4: `DONE sNNNx = MAIN-READY-N` for a merged batch, `IN FLIGHT sNNNx (worktree ...)` for a running one, and append the round's board numbers to the §0 status block.  The same applies to whatever plan doc replaces it.  Related: [[project_round_level_cadence_s519]], [[feedback_write_session_state]].
