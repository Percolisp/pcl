---
name: project_wantarray_followup
description: Next-session TODO — sweep wantarray/list-vs-scalar context handling across more constructs
metadata: 
  node_type: memory
  type: project
  originSessionId: db8f9009-70c1-40d8-bda8-9c7c8025a98a
---

Wantarray/context work is now AUTHORIZED (user lifted the long-standing prohibition 2026-05-29, session 215). See [[MEMORY]] and `docs/wantarray-context.md`.

Session 215 fixed several list-vs-scalar context bugs. **Next time, look at wantarray/context handling in MORE places** — the same class of bug almost certainly lurks elsewhere. Concrete leads:

- **do.t still 10/73 failing**: tests 35/36 (`return (do{}, (do{}) x N)` list context), 63-68 (`do subname(arg)` vs `do subname("arg")` syntax distinction), 70, 73 (EISDIR on `do dir`).
- **Generalize the slice-in-scalar fix**: `_slice_in_context` (ExprToCL) only covers `slice_a_acc`/`slice_h_acc`. Check kv-slices, `sort`/`reverse` in scalar, and any other list-producing construct whose scalar-context value is wrong.
- **`=~` context wrapper** (session 215) only wraps bare matches with a *definite* annotated context. Audit other context-sensitive builtins (`reverse`, `keys`, `values`, `localtime`, `gmtime`, `caller`, `unpack`) for the same "ambient *wantarray* leaks in from an enclosing list construct" bug — esp. as args to `join`/`print`/list funcs.
- **The deep fix** (`docs/two-phase-compiler.md` + `docs/ast-annotation-plan.md`): proper AST-level context annotation so codegen never relies on the `// SCALAR_CTX` default. The session-215 fixes lean on `get_node_context_raw` (undef when unannotated) to avoid wrongly reducing slices — a band-aid until annotation is comprehensive.
- **Pattern to remember**: PCL conflates "array" and "list" as one adjustable vector. Scalar context wants COUNT for array-var/map/grep/keys/values, but LAST element for slices/list-literals. The consumer (box-set, p-return-value) can't tell them apart → the reduction must happen at the PRODUCING site (the slice/array codegen) using the node's context. `p-list-scalar` (last) vs count are the two helpers.

Verification cadence that caught regressions this session: run `prove -j8 Pl/t/` (gate, ~4min) AND the full `perl sweep-perl-tests.pl --jobs 8` after broad context changes — a `||`-LHS-context mistake cost -1 net and only showed in the sweep.
