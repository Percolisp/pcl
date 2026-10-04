---
name: project_v2_nested_package_d1lite
description: "E1.5 nested `package` shipped as D1-lite (s281) — v1-shape transplant, census 84; the exposed-bug pattern for de-gating"
metadata: 
  node_type: memory
  type: project
  originSessionId: 933125d5-e552-4dc7-b751-e3cfa1e44b67
---

**E1.5 / D1 is DONE (session 281, 2026-07-11), implemented as "D1-lite"** — the
user asked whether D1 could be simpler if slower; answer: v1's own mechanism IS
the simple version (shared-Environment push + inline `p-set-current-package`;
qualified emission falls out of Environment-driven emitters).  Took 1 session,
not the budgeted 2–3.  Write-up in `docs/v2-endgame-plan.md` §3 D1.

**Why:** design docs over-specced "qualified emission for the scope remainder";
inspecting how v1 actually passes the same tests revealed the minimal shape.

**How to apply:** before building new v2 mechanism for a gated construct, first
transpile a minimal case with `PCL_V1=1` and read v1's emission — "copy v1
shapes" often collapses a multi-session design into a day.  Also: **de-gating a
file exposes pre-existing v2 bugs in constructs that never ran natively** —
s281 found three (delete-local-in-my lost restore; `$^S` unbound both pipelines
— missing from `%SPECIAL_VARS` so the :invert readtable read it as `$^s`, plus
missing runtime defvar; CLForm raw_wrap closers swallowed when the last body
line ends in a `;;` comment).  A top-level `local` makes the whole segment
remainder ONE raw_wrap form, so a single abort there kills every later test —
suspect this shape when a de-gated file goes PARTIAL at a spot v1 survives.

Verify method gotchas (s281): worktree byte-diff — normalize the wt path inside
outputs AND re-normalize after any regeneration; filter the pipeline-marker
with perl, NOT grep (NUL bytes silence grep → empty .lisp files look like
"no diff").  Related: [[project_parser2_prototype]].
