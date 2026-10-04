---
name: project-return-family-transfer
description: Approved future optimization — transfer sub return-value families through sub_info so call-site writes become proven (no freeze); scheduled after E2–E4
metadata: 
  node_type: memory
  type: project
  originSessionId: 2d5442c8-98d8-4960-bdc4-eba843eeefed
  modified: 2026-07-20T13:49:55.986Z
---

**Return-family transfer (task #77, docs/faster-codegen-suggestions.md item
T1)** — user-approved 2026-07-20 (s303), explicitly deferred until E2–E4 are
done. Idea: in the existing Parser2 sub_info pre-pass, classify each named
sub's `return`/tail expressions with `_tw_shape_ok`'s family oracle; record
`returns => 'num'|'str'` for subs where EVERY return is operator-coerced or
literal. A call-site write `my $x = f()` with a recorded family becomes a
PROVEN family write — plain raw slot, no `%pcl-to-*-strict` wrapper (better
than the B-verdict). No new soundness assumptions: same closed-world rules
as direct calls (no method dispatch/coderefs/AUTOLOAD; bail on glob
redefinition). Phase 2 (larger, two-pass ordering): caller→callee param
use-class transfer so `f($q)` need not classify `$q` opaque. Related:
[[project_product_targets_speed_and_ir]].
