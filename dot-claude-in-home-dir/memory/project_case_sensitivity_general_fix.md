---
name: project_case_sensitivity_general_fix
description: "SHIPPED & COMMITTED (d43640c, 2026-06-21): Perl case-sensitive identifiers work via (readtable-case :invert) — vars/subs/packages/classes/methods/labels case-distinct. s252 __pcl_ci_ AND older __case__N rename both retired (969fb86). Remaining: saved-core/FASL scoped-readtable audit."
metadata:
  node_type: memory
  type: project
  originSessionId: 46ee55e1-bc6e-4c11-a431-e6b88a55aa13
---

# Case-sensitivity: SHIPPED via `:invert` readtable (2026-06-21)

**Status: the general fix is DONE and COMMITTED (d43640c, on `main`).**
Full Pl/t gate green (101 files / 3551 tests). Full sweep at parity (~18017 pass
vs ~18088 baseline). Supersedes the old "deferred / s252-targeted-only" status.

## What shipped
`cl/pcl-runtime.lisp` reads PCL's own runtime AND all generated code under
`(setf (readtable-case *readtable*) :invert)` (set once, right after the
`(require …)` lines so cl-ppcre/asdf/sb-posix load under standard `:upcase`
first). Under `:invert`: an all-lowercase token upcases (standard CL still
works), an all-UPPERCASE Perl name downcases, **mixed-case is preserved**. The
`p-`/`pl-`/`plc-` prefix turns every uppercase Perl name into a *mixed* CL token,
so subs/classes are fixed for free — the gap s252 left.

Result: `$base_len`/`$BASE_LEN`, `sub foo`/`sub FOO`, `package Aa`/`package AA`,
mixed/upper method names, and loop labels are all case-distinct. The Getopt::Long
s264 symref collision (`${"opt_$name"}`) is fixed for free too.

## The one rule + the helpers
Everywhere the RUNTIME builds a CL symbol from a *string* (or reconstructs a
Perl name from a CL symbol), it must apply the SAME transform the reader did.
Helpers in `cl/pcl-runtime.lisp` (near `perl-pkg-to-cl-pkg-name`):
- `%pcl-invert-case` — mirrors the reader's `:invert` (its own inverse).
- `%pcl-cl-sub-name name` → `invert("pl-"+name)` (the `pl-` prefix is part of the
  case-uniformity test, so `PL-`+upcase is WRONG for DESTROY/AUTOLOAD/Foo).
- `%pcl-loop-tag prefix label` — shared by codegen catch + runtime throw so
  `LAST-`/`NEXT-`/`REDO-` tags agree for uniform AND mixed labels.
- `%pcl-uname-to-sub uname` — CODE-slot sub name for a typeglob.
~40 `string-upcase` sites → `%pcl-invert-case`; `@ISA`/`@EXPORT`/`@EXPORT_OK`/
`%EXPORT_TAGS`/`$AUTOLOAD` literals → lowercase; `PL-` prefix CHECKS →
`string-equal`; `caller()[3]` + stash reverse-maps invert-then-strip;
`clos-class-to-pkg` callers use `(symbol-package (class-name cls))`.

## Bugs found during code-review (all fixed + regression-tested)
- **AUTOLOAD fully broken** (literal `PL-AUTOLOAD` ≠ `pl-AUTOLOAD`) — common CPAN
  content; now via `%pcl-cl-sub-name "AUTOLOAD"`.
- **Stash keys lost case** (`keys %Pkg::` gave `bar` for `sub Bar`) — was
  `string-downcase`, now invert-then-strip.
- **Loop labels**: uniform-case (SKIP) broke because codegen baked `pcl::LAST-SKIP`
  as a token (folds) vs runtime string `"LAST-"`; fixed via `%pcl-loop-tag`.
- **Bareword FH** name reconstruction now inverts (lowercase FH round-trips).
Regression tests: `Pl/t/case-invert-01.t` (13 differential-vs-perl) + the 4
behavioral collision tests kept in `Pl/t/misc-fixes-02.t`.

## CPAN validated under :invert
Carp (caller-heavy), Scalar::Util, List::Util, `use parent`, `use overload`,
Exporter constant import (Fcntl SEEK_SET), tie (uppercase TIESCALAR/FETCH/STORE)
— all pass. Data::Dumper `Dumpxs` crashes but that's a PRE-EXISTING XS-fallback
bug (fails on HEAD too), not case-related.

## Retired
s252 `__pcl_ci_N` machinery REMOVED: `_compute_and_apply_case_renames` +
`_bare_ident_of_token` (Parser.pm), `_case_renamed` + 2 call sites (ExprToCL.pm),
`case_renames` attr (Environment.pm). Zero `__pcl_ci_` artifacts now.

## Remaining follow-ups (not blockers, fail-loud or harmless)
1. **Saved-core / FASL scoped-readtable audit.** The readtable is set GLOBALLY
   (process-wide). Fine for `runpcl`/`runt`/sweep/gate (runtime loads first), but
   the `pcl`-runner saved-core and FASL caches need the binding SCOPED + cache
   invalidation (a stale `:upcase` FASL mismatches → the first-sweep crypt/infnan
   "crashes" were exactly this; cleared via `rm -rf ~/.pcl-cache`). Fails loud.

## DONE — dead-code cleanup (2026-06-21, commit 969fb86)
- **`__case__N` lexical rename REMOVED** from Parser.pm `_with_declarations`
  (both the scoped path and the legacy path). Redundant under `:invert` (`$T`/`$t`
  already distinct CL symbols). Gate green 101/3551; case-invert-01.t + closure-01.t pass.
- **`clos-class-to-pkg` REMOVED** (defun + export + forward-declaration) — was
  exported but never called; string-upcase logic was wrong under `:invert` anyway.

See `docs/case-sensitivity-plan.md` for the full analysis/rationale.
Related: [[project_math_bigint_shim]] (the original collision that motivated this).
