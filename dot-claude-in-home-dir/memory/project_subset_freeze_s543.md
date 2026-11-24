---
name: project-subset-freeze-s543
description: "The USER decided (s543, 2026-11-16) to FREEZE a supported subset and drive in-subset silent wrongs to zero until it is seen to work; UTF-8 flag LAST (the content rule is rejected as wrong); line tracking ON by default; subtasks may start again"
metadata:
  node_type: memory
  type: project
  originSessionId: 60e26d3c-cdb2-4ed6-b32b-25f64011f327
  modified: 2026-11-16T19:36:30.212Z
---

The USER asked (s543, 2026-11-16) how to get PCL usable for non-XS production Perl and how long it
takes, and accepted the recommendation: freeze a SUPPORTED SUBSET, triage the queue against it, and
work only on the IN part until the exit criteria are met ("until we see it works" -- my reading:
a trial, to be re-judged on its results, not a permanent rule).

**Why:** the open silent-wrong count was 105 on 2026-10-10 and 105 on 2026-11-16 while the board went
from 14 to 30 dists; a widening target finds bugs as fast as they close, so no estimate holds
([[project_compat_plan_s538]], docs/production-readiness-review-s513.md).

**The USER's rulings of the same day:**
- **The UTF-8 flag is done LAST.** The content rule ("1 only when the string holds a character above
  0xFF") is REJECTED AS WRONG: perl marks a string as UTF-8 whatever its characters are, so a decoded
  pure-ASCII string is flagged. Do NOT propose the content rule (or content rule + a mark) again;
  when its turn comes it is a real flag with a design of its own. #1389's rows are OUT until then.
- **Line tracking ON by default** (batch s543a): the measured cost is accepted and flagged.
- **Subtasks may be started again** for this work (the s542 "do not start more" is lifted); the
  default of TWO at a time stands.

**How to apply:**
- `docs/supported-subset.md` is the statement (DRAFT: still open are scope-exit DESTROY #2370 --
  only needed for the SERVICE claim --, Moo in or out, the module lists). Do not quote it in
  README / STATUS until confirmed.
- A brief during the freeze names IN tasks only: silent wrongs, then drops, then loud failures.
- PAUSED: widening the board ([[project_cpan_board_widening_s515]]), new steering populations,
  perl-suite rows no IN program reaches. NOT paused: the round instruments, the gate, a PPI fix for
  a mis-lex in an IN program.
- Exit criteria (the doc's section 5): zero IN silent wrongs; a held-out sample (Rosetta #2104, then
  a fresh one) finds none; the soak test #2945; no open heap-exhaustion task (#5083 #4904 #4643).
- Estimates given to the USER: ~3 months after the freeze for batch programs, 5-6 for services
  (two agents + one review session a day); the s513 estimate of the same shape did not hold.
- Still owed: the triage pass over the open tasks (917 on 2026-11-16), two review sessions.
- A lesson of this session: I told the USER line tracking's cost was unmeasured when it was recorded
  in `Pl/Passes.pm` and `docs/caller-implementation.md` -- grep DECIDED for the switch name BEFORE
  saying "not measured".

**Triage, first pass DONE 2026-11-16 (s543):** every open task has `triage` (in / out / tooling / decision) +
`triage_reason`; `tools/task-triage.pl` counts (927 open: 634 in, 120 out, 115 tooling, 36 decision; 117 IN and
marked silent wrong -- an upper bound, classified from subjects). The next brief is the VERIFICATION batch (each
silent-wrong task`s own reproducer, plus the ~40 reasons that say `verify`). The 36 `decision` tasks are the
USER`s to rule (21 constructs proposed for section 4). A new task gets its triage field when it is filed or at
the merge that commits it. Notes: `briefs-and-rules-for-claude-subagents/s543/PAUSE-s543.md`.

**2026-11-24 (s543, the USER asked whether the bug cleanup will ever end; three rulings followed -- paraphrased):**
- **Exit criterion 1 counts silent wrongs of ORDINARY code only.** Each silent-wrong task carries `frequency`
  (`ordinary` | `rare`) + `frequency_reason` (the evidence); no field = ordinary; rare ones are listed known
  divergences. `tools/task-triage.pl` prints both numbers (first pass from the subjects: 21 ordinary + 64 rare).
  **Why:** every fix batch probes neighbouring spellings and files each difference, so the undivided count did
  not converge (five batches closed 39, the count fell 86 -> 75).
- **A neighbour found by a batch's own probes does not extend the batch unless it is ordinary**
  (`s473/COMMON.md` "ORDINARY OR RARE"). **How to apply:** a brief names ordinary tasks; "ordinary" needs a real
  program or a perldoc idiom as evidence; a rare fix is taken only where a batch is already in the function.
- **The Rosetta corpus is the NEXT SESSION's main work** (plan: `docs/rosetta-plan.md`, task #2104): 652
  perl-validated programs by other authors under `~/pcl-agent-scratch/s543/rosetta/` (GFDL: never checked in);
  547 of 652 identical on MAIN-READY-110 (73 % in September), 43 silent, 87 of the 105 differing are in the
  subset. Census (two agents) -> causes -> fix batches ranked by programs broken -> ONE run of a fresh sample
  (proposed: Perl Weekly Challenge; the USER has not confirmed the source).
- My own lesson: when the USER asks "will this end", answer with the convergence of the counted number and
  with what real programs show, not with the queue's size.
