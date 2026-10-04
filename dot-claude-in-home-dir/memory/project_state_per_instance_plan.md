---
name: project_state_per_instance_plan
description: "DECIDED (2026-07-18): implement per-instance state cells (let-over-lambda) for anon subs — brief + semantics audit live in task #56; sequence with task #65"
metadata: 
  node_type: memory
  type: project
  originSessionId: 7bb122c3-6d56-4cbf-9063-3068a395b1e0
  modified: 2026-07-18T07:16:54.324Z
---

**User decision (2026-07-18, s295c-2): IMPLEMENT per-instance `state` (never
bless the shared-cell divergence — silent-wrong-answer class on the
factory-closure idiom).**  **Task #56's description = the full implementation
brief** (design, risk, estimate, acceptance); sequence with [[project_parser2_prototype]]
task #65 (anon-sub seam carve-out — same wrap site).

Core design: wrap the seam's anon-sub `(lambda …)` emission in
`(let (($x__state__N (make-p-box nil)) ($x__state__N__init nil)) …)` — cells
mint at closure construction.  The existing guarded-init emission works
unchanged on let-bound cells; map/grep/sort blocks need NO new mechanism
(shared cell via the existing defvar path, ownership = nearest enclosing sub
else file).  Expression-position `++state $x` is a small extension.

**Why:** perl state vars are lexicals with NO symbol-table entry — not
name-reachable cross-package BY SPEC, so let-bound cells are *more* faithful
than the named-sub defvar cells.  Refs/closure calls work (cell = heap p-box);
eval-in-same-sub stays gated (same `_shadow_rename_blocker` as named-sub
state); `\state` address identity stays not-supported (SV-identity class).

Estimate: one strong session with #65 folded in; +hours if state.t's
file-level-state rows are load-bearing (separate die, same shared-defvar fix).

**signatures.t is the SAME family (s295c-3 finding — its "bless residue" note
was stale like lfs.t's):** real gate = state in SIGNATURE DEFAULTS (t126
expression-position, t127 do-block).  No bless decision exists.  Route A
(cheap, ~hour): _rename_state_vars skips state inside signatured subs —
their definitions ALREADY lower via v1 (_fallback_stmt W4) and v1's own
state machinery handles them; acceptance = parity 796+182/979.  Route B =
the expression-position guarded-init item of the state brief (needed only
when signatured subs go v2-native).  **#56 now contains NO open user
decisions** — all parts resolved/reclassified; task #56 = the briefs.
