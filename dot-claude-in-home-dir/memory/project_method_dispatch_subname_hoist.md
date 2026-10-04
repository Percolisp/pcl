---
name: project_method_dispatch_subname_hoist
description: "TODO — hoist the loop-invariant %pcl-cl-sub-name computation out of p-method-call's ISA/MRO walks (do NOT memoize)"
metadata: 
  node_type: memory
  type: project
  originSessionId: 305f49fa-fe76-4b8b-8c45-9541b7fe2080
---

**Optimization to do next: hoist `%pcl-cl-sub-name` out of `p-method-call`'s
walk loops.** (Analysis done 2026-06-22; not yet implemented.)

`%pcl-cl-sub-name` (`cl/pcl-runtime.lisp` ~line 341) builds the CL symbol-NAME
`"pl-<name>"` for a method, applying the `:invert` case transform
(`%pcl-invert-case`, a per-char scan + a string-(up|down)case alloc). It is
called from `p-method-call` in THREE walk branches, each recomputing it with the
SAME invariant `method-name` once per class visited:
- CLOS MRO path: inside `(dolist (cls mro) …)` (~10515)
- UNIVERSAL fallback `find-in-u` recursion (~10530)
- `@ISA` dynamic walk `find-in-class` recursion (~10570)

So a dispatch on an object with an N-deep ISA/MRO chain rebuilds the identical
string up to N times. Method dispatch is hot, so this matters.

**Fix = loop-invariant hoist, NOT memoization.** Compute
`(%pcl-cl-sub-name method-name)` ONCE near the top of `p-method-call`, bind it
(e.g. `cl-meth`), and reuse it across all three branches (pass it into / close
over `find-in-class` and `find-in-u`). Turns O(ISA-depth) string-builds into
O(1) per dispatch — no cache, no unbounded growth, obviously correct.

**Why not a global memo:** an `equal` hash memo still scans the key to hash it
(same work as the invert scan), adds a probe + a never-invalidated growing
cache, and the result still feeds a per-package `find-symbol` (which dominates).
Net memo win = avoiding 2 small allocs across *different* dispatches only — the
hoist already kills the bigger O(depth) intra-dispatch redundancy. Revisit a
memo only if a profile shows the single per-dispatch build still matters
(unlikely; `find-symbol` + `apply` dominate).

Note: the SUPER::/explicit-package branch uses a different var `meth-part`
(~10460/10480) — hoist there too only if it sits in a loop. Related:
[[project_case_sensitivity_general_fix]] (where `%pcl-cl-sub-name` came from).
