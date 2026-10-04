---
name: project-r1-runtime-fast-paths-done
description: "R1 runtime fast paths SHIPPED 2026-07-02 (183540d) — FPU traps once, inline op fast paths, lean p-sub; fib now BEATS perl; key SBCL gotchas recorded"
metadata: 
  node_type: memory
  type: project
  originSessionId: 1a509f1b-73fe-40a8-88a3-63f0e922b002
  modified: 2026-07-22T22:24:44.566Z
---

**GOTCHAS (were in MEMORY.md index, moved here s309):** inline-sandwich `else`
costs 4.9 s load time; SBCL 2.6.0 ICEs on inline + narrow-ftype combination.

**R1 is DONE (2026-07-02, commits `183540d` runtime + `303dcde`+follow-up v2).**
Measured (whole-program − null baseline): intmath **7.5×→1.5× of perl**,
fib(29) **5×→0.56× — BEATS perl** (0.078 s vs 0.138 s). Review checkpoint met.

What shipped in `cl/pcl-runtime.lisp`:
- `(sb-int:set-floating-point-modes :traps '(:divide-by-zero))` once at load
  (+ `sb-ext:*init-hooks*` re-apply for saved cores). **Keep :divide-by-zero
  trapping — perl dies on `1/0.0` too** (the review doc's "1/0.0→Inf" was wrong).
  `%pcl-ieee-arith` deleted.
- Inline numberp/stringp fast-path wrappers over `%p-…-slow` out-of-line
  overload paths for `+ - * / % == != < > <= >= <=> .` + string cmps + `cmp`
  and `unbox to-number to-string p-true-p p-bool %pcl-nan-p`.
- **Lean p-sub**: the ~150 ns/call cost was `%p-sub-perl-name`/`pcl-pkg-perl-name`
  recomputed per call — now hoisted to definition time. `p-sub` lifts leading
  `(declare …)` forms to its lambda head; v2 emits
  `(declare (ignore %_args) (dynamic-extent %_args))` + plain block instead of
  `p-args-body` when the body never reads `@_` (`$body_uses_args` gate includes
  `\bgoto\b` — **goto &sub forwards the live @_**, so goto-subs keep p-args-body).

GOTCHAS (cost hours, do not rediscover):
- **Inline sandwich**: `(declaim (inline f))` BEFORE defun (stores expansion),
  `(declaim (notinline f))` right AFTER, re-proclaim ALL inline + the global
  `(optimize (speed 2) (safety 1) (debug 0))` at END of pcl-runtime.lisp.
  Naive global inline+speed-2 made every SBCL spawn's source-load 1.15→4.9 s
  (gate 4-file spot check 553 s). The sandwich keeps load at ~1.15 s while
  generated user code (compiled after load) open-codes the fast paths.
- **SBCL 2.6.0 ICE**: `declaim inline` + a NARROWED return ftype
  (e.g. `(function (t) string)`) ICEs sb-c during type derivation. Keep the
  wrapper ftypes `(function (t) t)`.
- p-sub call-cost bench harness: [[project-parser2-prototype]] scratchpad
  pattern — plain defun+binds+catch=0.042 s vs p-sub=0.29 s exposed the
  per-call constants; ALWAYS measure components before leaning further.

Remaining per-call costs (diminishing, measure first): catch/throw ~18 ns
(lexical return needs "no closure re-throws" proof), 5 dynamic binds ~15 ns
(elision = whole-program "nobody calls caller()" bit, NOT per-sub — caller()
reads the chain). See `docs/parser2-prototype.md` "Lean p-sub".
