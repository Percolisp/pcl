---
name: project_cpan_module_log
description: Running log of CPAN/core modules tested through PCL and bugs each surfaced; Data::Dumper now works
metadata: 
  node_type: memory
  type: project
  originSessionId: 305f49fa-fe76-4b8b-8c45-9541b7fe2080
---

Keep a running log of CPAN/core modules tried through PCL in
`docs/cpan-module-log.md` (user asked 2026-06-22). Each entry: module, status,
what worked/broke, and crucially whether the bug was module-specific or a
**general** PCL bug (the valuable kind — tests the "do problems converge to a
finite bucket?" hypothesis, see [[project_cpan_convergence_survey]]).

**Data::Dumper — now ✅ byte-identical to perl 5.40.** Getting it to run
surfaced 3 general bugs, all fixed (session after s264, uncommitted at time of
writing then committed):
1. `XSLoader::load` silently succeeded → dual-life modules never fell back to
   pure Perl (`eval{require XSLoader; XSLoader::load(M);1} or $Useperl=1`). Fix:
   `XSLoader::pl-load` now `p-die`s (cl/pcl-runtime.lisp).
2. `local($ref->{key}) = EXPR` (paren list-form on a subscripted lvalue)
   clobbered the base scalar. Fix: generalized the pre-unwrap in
   `_process_local_declaration` to unwrap a single comma-free subscripted
   lvalue (Pl/Parser.pm).
3. BEGIN inside an expression-level `do{}` in an elsif CONDITION (inside a
   packaged named sub) hoisted the BEGIN mid-`p-if` → "too many elements".
   Root cause: post-s253b sub bodies emit into the `definitions` bucket, and
   `parse_block_as_function` appended the hoisted BEGIN to that same bucket
   mid-emission. Fix: defer into `_pending_hoisted_defs`, flushed by
   `_process_children` at the top-level statement boundary (Pl/Parser.pm).

Regression test: `Pl/t/local-paren-begin-do-01.t` (5 tests).
