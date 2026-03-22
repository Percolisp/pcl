# PCL: Eliminating SBCL Compilation Warnings

This document tracks the effort to eliminate all SBCL STYLE-WARNING / WARNING
messages emitted when loading `cl/pcl-runtime.lisp` (e.g. via `--load`).

Original count: **24 compilation warnings** (session 86).
After session 86 fixes: **8 remaining**.
After session 87 fixes: **0 remaining** (all fixed).

---

## Already Done (session 86)

### 1. `declaim` block for deeply-forward-referenced functions
Added after the existing `declaim` at line ~238:

```lisp
(declaim (ftype (function (t) t)
                object-address looks-like-number
                p-typeglob-p p-typeglob-name p-typeglob-package
                p-regex-match-p p-regex-match-pattern p-regex-match-modifiers
                clos-class-to-pkg perl-pkg-to-clos-class))
(declaim (ftype (function (t t) t) p-can p-isa))
(declaim (ftype (function (&rest t) t)
                p-method-call p-glob-undef-name p-glob-copy parse-number
                build-ppcre-options))
```

These suppress warnings for functions defined hundreds of lines after their
first caller (e.g. `p-method-call` defined at ~5900, first used at ~370).

### 2. Reorder `expand-autoviv` `eval-when` before `p-autoviv-set`
The `defmacro p-autoviv-set` calls `expand-autoviv` at macro-expansion
time (compile time).  Previously the `eval-when` defining `expand-autoviv`
was the block AFTER the `defmacro`.  Moved the `eval-when` block first.

Also added an inner `declaim` inside the `eval-when` for `expand-autoviv-for-array`
(mutually recursive with `expand-autoviv`):

```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (ftype (function (t) t) expand-autoviv-for-array))  ; ← new
  (defun expand-autoviv (form) ...)
  (defun expand-autoviv-for-array (form) ...))
```

### 3. Move `object-address` before `box-nv`
`box-nv` (line ~546) called `object-address` which was defined just after
it (~572). Moved `object-address` to immediately before `box-nv`.

### 4. Move `looks-like-number` before `p--`
`p--` (line ~748) calls `looks-like-number` which was defined at ~851.
Moved `looks-like-number` to just before `p-+` / `p--`.

---

## Remaining Warnings (8 total, 3 root causes)

### A. `%SIG` undefined variable in `p-warn` (1 WARNING — not just style-warning)

**Location:** `defun p-warn`, line ~3830.
**Code:** `(gethash "__WARN__" %SIG)`
**Root cause:** `(defvar %SIG ...)` is at line ~4820, about 1000 lines after `p-warn`.

**Fix:**
Move the `(defvar %SIG (make-hash-table :test 'equal) ...)` line from the
"Module System" section (~4820) up to the special-variables section (~line 400),
where `%ENV` and other global hashes live.  The line is:

```lisp
;; %SIG: signal/exception handler hash
(defvar %SIG (make-hash-table :test 'equal) "Perl %SIG - signal handlers")
```

### B. `|Carp|::TO-STRING` undefined (4 STYLE-WARNINGs, one per Carp stub)

**Location:** `pl-croak`, `pl-confess`, `pl-carp`, `pl-cluck` (lines ~6580+),
compiled inside `(in-package :|Carp|)`.
**Code:** `(to-string (car args))`
**Root cause:** `to-string` is NOT exported from `:pcl`.  The `(:use :pcl)`
in the `|Carp|` package only inherits exported symbols, so `to-string` in
that package context creates a fresh `|Carp|::to-string` with no definition.

**Two equivalent fixes (pick one):**

Option A — Export `to-string` and `to-number` from `:pcl`:
```lisp
;; In defpackage :pcl (:export ...) — add:
#:to-string #:to-number
```

Option B — Use the fully-qualified name in the 4 Carp stubs:
```lisp
;; Replace (to-string (car args)) with:
(pcl:to-string (car args))
```

Option A is cleaner since `to-string` / `to-number` are already used everywhere
else in the `:pcl` package context; exporting them makes no behavioral difference.

### C. Unused variables (3 STYLE-WARNINGs in 3 functions)

Add `(declare (ignore ...))` in each function:

| Variable(s) | Function | Fix |
|---|---|---|
| `has-dot`, `has-exp` | `parse-perl-number` | `(declare (ignore has-dot has-exp))` |
| `found` | `%p-readline-impl` | `(declare (ignore found))` |
| `bracket-start` | `expand-glob-char-ranges` | `(declare (ignore bracket-start))` |

For `parse-perl-number`, the two variables are bound in a `let*` block but
their values are never read (they were apparently left over from an earlier
implementation draft).  The `declare` goes inside the enclosing `let*`.

---

## Implementation Order for Next Session

1. **%SIG** (1 line move) — eliminates the one real WARNING (not style-warning).
2. **to-string export** (1 line in defpackage) — eliminates 4 Carp warnings cleanly.
3. **Unused variables** (3 `declare` additions) — eliminates 3 style-warnings.

After all three: zero warnings.
