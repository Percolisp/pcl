# Session State - Session 69

## What Was Done

### 1. Documentation: `docs/todo-features.md` and `docs/not-supported.md`
(Carried over from previous session — already committed in 6275e0e)
- Created `docs/todo-features.md`: 25 items in 3 tiers with test counts and fix areas
- Expanded `docs/not-supported.md`: 11 new sections
- Moved "Lvalue subs" from TODO to not-supported
- Fixed `caller()` entry: it IS implementable (only the "at FILE line N" suffix is not)

### 2. CL Package Namespace Fix (the Tie::Array/Hash hang root cause)

**Root cause:** User-defined Perl methods (e.g. `sub PUSH`, `sub SHIFT`) in packages
like `Tie::Array` map to `PL-PUSH`/`PL-SHIFT` in CL. These packages `(:use :pcl)`,
so `defun PL-PUSH` in `|Tie::Array|` would redefine the globally-shared `pcl:PL-PUSH`
symbol, causing infinite recursion.

**Fix (three parts):**

#### A. `pl-sub` macro — use symbol's own package, not `*package*`
`pl-sub` previously called `(shadow sym-name *package*)`. For package-qualified
names like `(pl-sub P1::pl-tmc ...)`, `*package*` was `main` — creating
`MAIN::PL-TMC` instead of `P1::PL-TMC`. Fixed with:
```lisp
(let* ((target-pkg (or (symbol-package ',name) *package*))
       (sym-name   (symbol-name ',name)))
  (shadow sym-name target-pkg)
  (setf (symbol-function (intern sym-name target-pkg)) (lambda ...)))
```

#### B. `pl-method-call` — only dispatch to locally-defined methods
Added `(eq (symbol-package fn) pkg)` check in both the MRO path and fallback
path, so inherited `:pcl` built-ins (e.g. `pcl:pl-push`) are skipped when looking
up user-defined methods.

#### C. `pl-load-module-cached` — muffle warnings during module loading
Wrapped all `compile-file` and `load` calls with `(handler-bind ((warning #'muffle-warning)) ...)`.

### 3. `pl-defpackage` macro — clean package declaration

**Problem:** SBCL emits "package at variance" warnings when `defpackage` re-evaluates
and finds extra shadow symbols (added by `pl-sub`'s `eval-when :compile-toplevel`
shadow call during `compile-file`). Simple `handler-bind` around `defpackage`
prevented compile-time package creation, breaking `in-package`.

**Fix:** New `pl-defpackage` macro in `cl/pcl-runtime.lisp`:
```lisp
(defmacro pl-defpackage (name &rest options)
  "Create/update a Perl package. Defaults to (:use :cl :pcl) when no options given."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (handler-bind ((warning #'muffle-warning))
       (defpackage ,name ,@(or options '((:use :cl :pcl)))))))
```

- `eval-when :compile-toplevel` ensures the package is created during `compile-file`
  processing, so subsequent `in-package` forms can find it
- `handler-bind` suppresses the "package at variance" warning
- Defaults to `(:use :cl :pcl)` so callsites can just write `(pl-defpackage :Foo)`

**Used in:** `Parser.pm` `_emit_package_preamble` (both block-depth and normal paths),
`pl2cl` main package setup.

**Note:** Pre-declared package preambles (before `(in-package :pcl)`) still use plain
`defpackage` — they're in `cl-user` context where `pcl:pl-defpackage` isn't accessible,
and they don't need warning suppression (no prior shadow calls at that point).

### 4. Result
- All 2493 PCL tests pass
- Tie::Array/Hash loading no longer hangs (binding stack exhaustion → now a
  different error: infinite recursion in sort.t's Tie::StdArray usage — separate issue)
- "package at variance" warnings eliminated from generated code

---

## Where We Stopped

`sort.t` still has binding stack exhaustion. The module loads now (no more
"compile-file failed" error) but something in Tie::StdArray causes infinite recursion.
Tie::StdArray's `PUSH`/`SHIFT`/`UNSHIFT` methods call `$self->SPLICE(...)` which
calls more Tie methods — there may be a circular dispatch or missing base implementation.

---

## PCL Suite Status
- **53 files, 2493 tests, all passing**
- Perl sweep (session 66): 5624 passing — fixes this session should improve this

