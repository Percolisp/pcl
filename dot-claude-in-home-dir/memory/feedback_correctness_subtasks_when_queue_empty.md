---
name: feedback_correctness_subtasks_when_queue_empty
description: USER 2026-10-24 (s526) -- when the todo queue is empty, Fable ADDS correctness subtasks as usual (supersedes the s494/s498 "no new correctness batch unasked" hold); and after the parser + runtime batches, a CODE-GENERATION review subtask
metadata:
  type: feedback
---

**USER (2026-10-24, s526, mid-turn): "When the updates to the parser and the runtime are done, create a subtask to review the code generation (Fable or Opus after what you think is best). When the todo queue gets empty, please add correctness subtasks as usual."**

**Why:** the s494/s498 hold ("no new correctness batch or test angle unasked") was about Fable inventing work while the USER steered; the USER now wants the pipeline kept full -- an empty queue is the trigger, and correctness is the default filler.  The code-generation review completes the s525 review series (parser `docs/parser-review-s525.md`, runtime `docs/runtime-review-s525.md`): Fable's choice is a FABLE FORK like s525r (the review's value is judgment; the method lives in the session context), read-only, one doc `docs/codegen-review-s526.md`, launched when s525q and s525u are both merged.

**How to apply:** (1) after the two merges, write `s526/s526c-prompt.md` (the codegen review: `Pl/ExprToCL.pm`, `Pl/CLForm.pm`, `Pl/Passes.pm`, `Pl/Parser2.pm`'s lowering half, against `docs/ir-spec.md` and the review series' shape) and run it as a fork.  (2) "Queue empty" = no brief in flight and no leftover owed (#3650, the companion, the PPI-registry filler, the 1a brief): then pick correctness batches from `docs/cpan-board-failure-review-s521.md` §4's remaining batches / the task store's silent-wrongs, two at a time.  No new TEST ANGLE unasked still stands (s493/s498).  Related: [[project_cpan_board_widening_s515]], [[feedback_two_agents_at_a_time]].
