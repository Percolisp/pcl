---
name: reference_cl_ppi_gotchas
description: "Dense reference — method dispatch, CL/SBCL runtime, PPI tokenisation and output-bucket gotchas for PCL (moved out of MEMORY.md to keep the index small)"
metadata: 
  node_type: memory
  type: reference
  originSessionId: 44df9e67-ab19-4025-a528-7b137d498079
  modified: 2026-08-07T19:24:40.512Z
---

Kept as one reference because each item is a single line that only matters
while editing the runtime or the parser. Related: [[reference_box_magic_hook]],
[[project_preprocess_source_strings]], [[project_symbolic_ref_slice_decision]].

## Method dispatch
Method names are STRINGS; dispatch via `%pcl-cl-sub-name` (`:invert`); AUTOLOAD
walks @ISA, skips DESTROY; every pkg gets an empty `@ISA`. Qualified/SUPER/
dynamic calls are special-cased; `""`→"main"; `%pcl-find-package` tries upcased
then literal; a tied invocant FETCHes first.

## CL / SBCL runtime
- **NaN/Inf**: SBCL signals on compare/sqrt with NaN → guard `(%pcl-nan-p)`;
  bitwise goes through `%pcl-to-integer` (Inf→0); a float literal that
  overflows becomes inf at transpile. `*read-default-float-format*` = double.
- `defvar` BEFORE `defun`. `p-*` = runtime, `pl-*` = user. Loops are
  `tagbody`/`go`; unlabeled `p-last` = `(block nil)`, labeled = catch.
- `p-return-value` is context-sensitive; `box-set` FETCHes a tie proxy;
  **GC moves objects — never cache an address-based NV**.
- `$.` = p-box; flip-flop keyed by compile-time ID; `(vectorp string)` is T →
  always pair with `(not (stringp v))`; `p-map` aliasing → `%p-map-copy-scalar`.
- `p-defined` → `1`/`""` vs `%pcl-definedp` → t/nil (use the latter in
  conditionals); array holes are `nil`, `p-exists-array` checks `(p-box-p slot)`.

## PPI
- `@{[expr]}` → `(p-join |$"| (p-cast-@ …))`; `%h{keys}` = Symbol+Block;
  `%{$ref}{…}` = Cast+Block+Block; `@{$ref}{…}` = Cast+Block+Subscript.
- `-bareword` is a single Word. **PPI `find` returns `0`, not undef → `|| []`**.
- Hex floats fixed in `_preprocess_source` (binary NOT).

## Output shape
Buckets: preamble / declarations / definitions / runtime; `p-sub` and
`p-defpackage` macros; `CODEGEN_DESIGN.md`. Sweep-skipped: heredoc.t, list.t.
Intra-sub `goto LABEL` partial → [[project_intra_sub_goto]]; t/op gaps →
`docs/perl-test-suite-coverage.md`.
