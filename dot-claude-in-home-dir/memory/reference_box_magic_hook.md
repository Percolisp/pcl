---
name: reference-box-magic-hook
description: "PCL's p-box has a runtime get/set interception hook (the tie proxy at the unbox/box-set chokepoints) — use it before calling any magic-lvalue feature not-supported"
metadata: 
  node_type: memory
  type: reference
  originSessionId: f7152fe6-aa5a-4f5d-a7fb-896373dac1c6
---

**Before judging any Perl magic / magical-lvalue feature "hard" or "not-supported",
check the box chokepoints first.** PCL scalars are NOT bare CL variables — they are
`p-box` structs, and *every* read/write through a box funnels through exactly two
functions in `cl/pcl-runtime.lisp`:

- `unbox` (read) — already dispatches on a magic marker in the box `value` slot:
  `(p-tie-proxy-p v)` → `(p-method-call … "FETCH")`.
- `box-set` (write) — same: `(p-tie-proxy-p current)` → `(p-method-call … "STORE")`.

So PCL *already has* "a variable that is an object whose read calls one method and
write calls another" — it's the `tie` protocol. Any other magic lvalue (arylen
`\$#array`, `\substr`, `\pos`, `\vec`, lvalue `substr`) is a **sibling marker**: a
small struct (e.g. `p-arylen-magic`, or a general `p-magic-cell` holding getter/setter
closures) placed in the box `value` slot, plus ONE `cond` arm in `unbox`, ONE in
`box-set`, ONE in `p-ref`/`p-reftype`, and ONE codegen rule that wraps it via
`p-backslash`. No call sites change; deref read/write flow through the chokepoints
automatically (`$$ref` → `p-cast-$` → `unbox`; `$$ref = v` → `p-setf`/`box-set`).

**The mistake to avoid:** reasoning about CL-the-language ("CL has no runtime hook to
intercept a variable read; only compile-time `define-symbol-macro` or setf-places") and
forgetting PCL already bolted its own SV-level hook onto the box. Session 218 wrongly
called arylen magic "needs a representation change CL has no equivalent for" — the
equivalent was the tie proxy, two arms away.

**Genuinely hard residue:** only the parts needing Perl's SV refcount/lifecycle — e.g.
freed-array-but-magic-alive (array.t 83–88) needs a weak pointer → GC nondeterminism
(same reason DESTROY-via-GC is not-supported). The *live* write-through cases are easy.

See `docs/sweep-bug-catalog.md` array.t entry and [[project-array-aassign-review-gate]].
