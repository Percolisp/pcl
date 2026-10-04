---
name: project_s296_state_family_wip_branch
description: "CLOSED s299: state-family branch merged to main (1774981); refusal-tightening postmortem lessons"
metadata: 
  node_type: memory
  type: project
  originSessionId: b7405173-b07e-4aae-bed4-110d8bee2ddd
  modified: 2026-07-19T15:46:22.902Z
---

**CLOSED (s299, 2026-07-19): task #56 done — `wip/s296-state-family` merged
to main as ff (HEAD 1774981) and the branch deleted.**  state.t v2-native
157+0+5skip/166; census 107/4; Pl/t gate 115 files / 4228 tests all pass;
cache gen v2-38.

Postmortem lessons worth keeping (full narrative: session-log §299):
- The s296 flatten refusal `_pkgblock_shadows_file_lexical` was a
  **de-gating aid, not a correctness guard**: without it the span engine
  either renames correctly (eval.t: `$x` → `x__file__7`, segment-top
  re-decl handled by the M-B multi-instance machinery) or DIES to v1
  (state.t: eval-scan hit on `$t`).  Fix = also require
  `_lex_referenced_after` (post-block file-level reference,
  declarator/shadow-discounted, interp-aware, deliberately NO string-eval
  conservatism).  Under-firing such refusals is always safe — it reverts to
  the pre-refusal path.
- `state ($t) //= 3` now emits `p-//=` on the persistent cell — the
  defined-or IS the once-guard (no `__init` flag); guards updated
  (state-01.t #3, parser2-02.t #39 now asserts file-level state is native).
- Writing tests exposed a PRE-EXISTING aliasing bug → task #72: a sub whose
  tail is `++$x` (captured or state cell) returns the LIVE box; holding two
  results before unboxing (`print $f->(), $f->()`) prints the final value
  twice.  perl `12`, PCL `22`, present on main, both closure-`my` and state.
- signatures.t nested-sub de-gate = still a future task (PCL_NO_NESTGATE=1
  measured worse: 765+213 vs gated 796+182, t146–t161 closure family).
[[project_parser2_prototype]]
