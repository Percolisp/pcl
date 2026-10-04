---
name: feedback_fable_never_executes_delegate_to_opus
description: "USER (s466, 2026-09-03) challenged Fable doing execution work (\"what is it that you need to do, that an Opus subjob can't do?\") — Fable's own hands are for review, design rulings and merge decisions ONLY; every implementation with a written design goes to a pinned Opus agent, even when a memory line says \"(Fable)\""
metadata: 
  node_type: memory
  type: feedback
  originSessionId: acc99a60-0f6a-4929-b976-1964c4e3dcd9
  modified: 2026-09-03T20:51:06.850Z
---

**USER, s466 (2026-09-03):** "Weren't you running a couple of subjobs in Opus? :-) Or are they
finished and you are doing work that needs to be done by Fable?" and then, sharper: "So what is
it that you need to do, that an Opus subjob can't do?"  Context: Fable had done the AZ merge
review (correct), then relaunched BC/BD late, then spent the wait implementing #1035 steps 0+1
itself (sixteen-site refactor, 68 test-row rewrites, artifacts, legs) — execution from a design
that was already written in the task.

**Why:** Fable time is the scarce budget (the weekly cap; s454 burned 19 % in 30 min).  Execution
from a written design is exactly what Opus agents do well in this project; Fable doing it delays
the reviews only Fable can do and spends the cap on typing.  A memory line saying "(Fable)" next
to an item means Fable DESIGNS it, not that Fable types it.

**How to apply:**
- Fable's own hands: reading diffs for review, rulings on measurements (which movers are real,
  what an instrument should do), design decisions not yet in a task, merge/ff decisions, records.
- Everything else — including "Fable-designated" items once their design is written into the
  task — is launched as a pinned `model: "opus"` agent ([[feedback_subagents_must_pin_model]]),
  up to the 3-agent cap; a quiet box is NOT a reason to hold agents back (the runner re-runs
  load movers serially).
- When the user asks this question, answer honestly and stop the execution, do not defend it.
- Related: [[feedback_hard_parts_first_e2_for_opus]], [[feedback_structural_first_not_at_any_cost]].

**s494 addition — REVIEW COMMITTED MEMBERS WHILE THE AGENT RUNS.**  An agent commits a WIP checkpoint per member, so the
review does not have to wait for MERGE-READY: write the probes before reading anything of the agent's, run them
perl → base → a THROWAWAY worktree at the agent's committed sha (`git worktree add --detach ~/pcl-agent-scratch/worktrees/rv-<label> <sha>`;
never run inside the agent's own worktree — it is mid-edit), and SendMessage a residue to the running agent so it is
fixed IN-BATCH.  s494: s492b's #2004 went 21-wrong → 2-wrong on 40 probes, and both residues (die/warn LIST not
flattened; `sprintf(@arr)` format slot) reached the agent hours before its final bar.  Remove the throwaway worktree afterwards.
