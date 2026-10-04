---
name: feedback_signoff_rule_simpler_faster
description: USER standing rule (s379) — design changes need NO user sign-off when simpler + clearer + faster generated code + compile time <50% worse
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 44c8b825-5f57-4845-9128-73128c57c628
  modified: 2026-08-09T16:47:28.375Z
---

USER (2026-08-09, s379, verbatim intent): "Is it simpler, clearer and
generates faster code? (And the total compile time is less than 50% worse?)
Then you really don't need to ask me."

**Why:** Sign-off round-trips were being spent on design decisions whose
verdict is fully determined by measurements the session can take itself
(e.g. the [[project_v2_session_state]] var-handling review's defglobal and
selective-hoisting directions were parked as "user decisions").

**How to apply:** Before starting a design/refactor change, take the
measurements; if ALL FOUR conjuncts hold — (1) simpler (less code /
fewer mechanisms), (2) clearer, (3) generated code runs faster (or
unchanged — a change that SLOWS generated code still gets flagged, per the
standing flag-slowdowns rule), (4) total compile/transpile time < 50 %
worse (USER s379c: the budget is PER CHANGE against the tree it lands on,
with cumulative whole-corpus transpile time tracked and flagged if it
creeps past ~50 % over the s379 ~65 s baseline) — then proceed without
asking. This waives the DESIGN sign-off only:
correctness gates (Pl/t gate, corpus-diff, sweep TOTAL/LOST, probes vs
perl) still apply in full, and behavior-CHANGING semantics (not
refactors) still follow the normal probe-and-rule process. Queue ORDER
remains the user's.
