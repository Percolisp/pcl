---
name: feedback_dup_census_compiler_only
description: "USER (s413, 2026-08-18) — duplicated-code extraction matters ONLY for the compiler (Pl/** + the shipped runtime cl/pcl-runtime.lisp); tools/** and the runner scripts may be replaced and are out of scope; test files are never \"optimized\""
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 13cf3fad-8643-4245-b90c-d8a27c366d80
  modified: 2026-08-18T19:21:59.252Z
---

**USER, s413 (2026-08-18):** "Only the compiler matters for duplicated code, the
tools might be replaced."  Said when the census population (54 files incl.
tools/**, the runners) was reported and the user feared test files were being
touched.

**Why:** the compiler (`Pl/**`) and the runtime the generated code runs against
(`cl/pcl-runtime.lisp`) are the PRODUCT; `tools/**`, `sweep-perl-tests.pl`,
`tools/run-perl-suite.pl` are scaffolding that may be replaced — effort spent
de-duplicating them is wasted.  Test files (`Pl/t/`, `perl-tests/`) are never
an optimization target at all (only ADD guard rows).

**How to apply:** `tools/dup-census.pl` families in tools/** or the runners are
LEAVE / out of scope (the worklist `docs/dup-census-worklist-s411.md` §1 says
so since s413); do not file or work extraction items there.  `s413a`
(tools/lib/PCLProc.pm, family 6) landed before the ruling and stays as is.
Related: [[feedback_structural_first_not_at_any_cost]], [[feedback_no_simplify_tests]].
