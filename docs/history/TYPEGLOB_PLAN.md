# Typeglob Implementation Plan

## What Typeglobs Are

A Perl typeglob `*foo` (or `*Pkg::foo`) is a reference to the symbol table entry
for the name `foo` in package `Pkg`. The symbol table entry ("stash slot") has
six independent slots:

| Slot     | Perl syntax | What it holds           |
|----------|-------------|-------------------------|
| SCALAR   | `$foo`      | A scalar value          |
| ARRAY    | `@foo`      | An array                |
| HASH     | `%foo`      | A hash                  |
| CODE     | `&foo`      | A subroutine            |
| IO       | `foo`       | A filehandle/dirhandle  |
| FORMAT   | `format foo`| A write-format (rare)   |

Every named variable and sub in Perl is implicitly a slot in some typeglob.

---

## Complete Operation Catalog

### Typeglob as rvalue (reading)

```perl
my $g = *foo;          # store the glob object as a scalar
\*foo                  # reference to the glob  — ref() returns "GLOB"
"$g"                   # stringify → "*main::foo"
$g + 0                 # numify → 0  (all globs are 0 numerically)
++$g                   # auto-increment → 1 (numifies first, then adds 1)
```

### Typeglob as lvalue (writing)

Assignment to a typeglob installs a new value in the appropriate slot,
determined by the **type** of the RHS at runtime:

```perl
*foo = \$scalar   # install $scalar as the SCALAR slot → $foo is now an alias
*foo = \@array    # install @array as the ARRAY slot   → @foo is now an alias
*foo = \%hash     # install %hash  as the HASH slot    → %foo is now an alias
*foo = \&sub      # install &sub   as the CODE slot    → &foo is now an alias
*foo = sub { }    # same — anonymous sub goes to CODE slot
*foo = *bar       # full copy: all bar's slots become foo's slots (aliasing, not cloning)
*foo = *bar{CODE} # copy only bar's CODE slot into foo
*foo = "string"   # install a string as the glob name (rare; IO slot import)
```

The RHS-type dispatch is the central operation. The current code special-cases
`*foo = \&bar` (already works) but handles nothing else.

### Slot access

```perl
*foo{SCALAR}    # reference to $foo (same as \$foo)
*foo{ARRAY}     # reference to @foo (same as \@foo)
*foo{HASH}      # reference to %foo (same as \%hash)
*foo{CODE}      # reference to &foo (same as \&foo)
*foo{IO}        # IO object (filehandle)
*foo{NAME}      # string "foo"
*foo{PACKAGE}   # string "main"
*foo{GLOB}      # the typeglob itself
```

### Special statements

```perl
local *foo               # save ALL slots of *foo, restore on scope exit
local *foo = sub { }     # save CODE slot, install new, restore on exit
local *Pkg::foo = \&bar  # most common: temporary function override
undef *foo               # clear all slots (removes sub, clears vars)
```

---

## What PCL Currently Does (Audit)

| Operation             | Current output          | Status |
|-----------------------|-------------------------|--------|
| `*foo = \&bar`        | `(setf (symbol-function 'pl-foo) #'pl-bar)` | ✓ works |
| `*foo = sub { }`      | `(setf (symbol-function 'pl-foo) #'--anon-N--)` | ✓ works |
| `*foo` as expression  | `*foo` (CL special var) | ✗ UNBOUND-VARIABLE crash |
| `*Pkg::foo` as expr   | `\|Pkg\|::*foo` (CL sym)| ✗ UNBOUND crash |
| `*foo = \$x`          | `(pl-setf *foo ...)` | ✗ wrong |
| `*foo = \@arr`        | `(pl-setf *foo ...)` | ✗ wrong |
| `*foo = \%h`          | `(pl-setf *foo ...)` | ✗ wrong |
| `*foo = *bar`         | `(pl-setf *foo *bar)` | ✗ wrong |
| `local *foo`          | `(let ((*foo (make-pl-box nil))))` | ✗ wrong: creates CL dynamic binding |
| `local *foo = sub{}`  | `(let ((*foo (make-pl-box #'...))))` | ✗ wrong |
| `undef *foo`          | `(pl-undef *foo)` | ✗ UNBOUND crash |
| `\*foo`               | `(pl-backslash *foo)` | ✗ UNBOUND crash |
| `ref \*foo`           | — | ✗ falls through |
| `*foo{CODE}`          | PARSE ERROR | ✗ |

### Why `local *foo` is wrong

`local` for plain `$foo` works in PCL because `(defvar $foo ...)` makes `$foo` a
CL *special* (dynamically scoped) variable, and CL `let` saves/restores it. But
`*foo` has no corresponding CL special variable — it's not a symbol at all, it's
a named table entry. Using `(let ((*foo ...)))` creates a fresh CL binding for the
symbol `*FOO` which is completely unrelated to the actual glob.

---

## CL Representation

### The `pl-glob` struct

A typeglob object is just a **label** — package + name. The slot values live in
the symbol table, not in the struct. This keeps the struct cheap and means that
reading a glob slot always reflects the current state of the symbol table.

```lisp
(defstruct (pl-glob (:constructor make-pl-glob (package name)))
  package   ; CL package object (from find-package)
  name)     ; upcased name string, e.g. "FOO"
```

### Symbol table mapping

For a glob named `foo` in package `main`, the CL symbol table has:

| Glob slot | CL symbol       | Accessor |
|-----------|-----------------|----------|
| SCALAR    | `MAIN::$FOO`    | `symbol-value` |
| ARRAY     | `MAIN::@FOO`    | `symbol-value` |
| HASH      | `MAIN::%FOO`    | `symbol-value` |
| CODE      | `MAIN::PL-FOO`  | `fdefinition` / `symbol-function` |
| IO        | property list on `MAIN::*FOO-IO*` or similar | TBD |

CL upcases symbol names, so `$foo` in generated code is interned as `$FOO`.
`(intern "$FOO" (find-package "MAIN"))` finds the right symbol.

### Aliasing via shared containers

This is the key insight. PCL already uses indirection everywhere:
- `$x` is a `pl-box` — modifying `$x` modifies the box's contents
- `@arr` is an adjustable vector — all holders of the same vector object see changes
- `%h` is a hash-table — same

When we do `*foo = \$x`, we want `$foo` to *be* `$x`. In PCL terms, this means
making `$foo` (the CL symbol `MAIN::$FOO`) hold the **same box** as `$x`. Since
boxes are shared by reference, writing to `$foo` then writes to `$x` and vice
versa — Perl aliasing behaviour achieved for free.

```lisp
;; *foo = \$x
;; \$x returns (make-pl-box $x), i.e. outer-box wrapping $x (the inner box)
;; We want MAIN::$FOO to hold the inner box = $x
(setf (symbol-value '$foo-in-pkg) (unbox the-backslash-box))

;; *foo = \@arr
;; \@arr returns @arr directly (it's already a vector, not boxed)
;; We want MAIN::@FOO to hold the same vector object
(setf (symbol-value '@foo-in-pkg) @arr)
```

---

## Implementation Plan

### Step 1 — Runtime: `pl-glob` struct + primitives

**File:** `cl/pcl-runtime.lisp`

Add near the ref/backslash section:

```lisp
;;; Typeglob support

(defstruct (pl-glob (:constructor make-pl-glob (package name)))
  package   ; CL package object
  name)     ; string, upcased Perl name, e.g. "FOO"

(defun pl-make-typeglob (pkg-str name-str)
  "Create a typeglob object for *Pkg::Name."
  (make-pl-glob (or (find-package (string-upcase pkg-str))
                    (error "No package: ~A" pkg-str))
                (string-upcase name-str)))

;; Stringification — add to stringify-value cond
;; ((pl-glob-p v) (format nil "*~A::~A" (package-name (pl-glob-package v))
;;                                       (pl-glob-name v)))

;; Numification — add to pl-to-number/to-number cond
;; ((pl-glob-p v) 0)

;; ref() — add "GLOB" to pl-ref cond
;; ((pl-glob-p inner) "GLOB")
```

### Step 2 — Runtime: `pl-glob-assign`

The single dispatch function for `*foo = RHS`:

```lisp
(defun pl-glob-assign (pkg-str name-str rhs)
  "Assign RHS to the appropriate slot of typeglob *pkg::name.
   Dispatch is by type of the unwrapped RHS value."
  (let* ((pkg  (find-package (string-upcase pkg-str)))
         (uname (string-upcase name-str))
         ;; Unwrap one level of boxing to see what was referenced
         (inner (if (pl-box-p rhs) (unbox rhs) rhs)))
    (cond
      ;; *foo = *bar — full glob copy (each slot independently aliased)
      ((pl-glob-p rhs)
       (pl-glob-copy pkg uname rhs))
      ((pl-glob-p inner)
       (pl-glob-copy pkg uname inner))

      ;; *foo = \&sub or *foo = sub{} — CODE slot
      ((functionp inner)
       (setf (fdefinition (intern (concatenate 'string "PL-" uname) pkg))
             inner))

      ;; *foo = \$scalar — SCALAR slot (inner is the pl-box = the variable itself)
      ((pl-box-p inner)
       (setf (symbol-value (intern (concatenate 'string "$" uname) pkg))
             inner))

      ;; *foo = \@array — ARRAY slot (inner is the adjustable vector)
      ((and (vectorp inner) (adjustable-array-p inner))
       (setf (symbol-value (intern (concatenate 'string "@" uname) pkg))
             inner))

      ;; *foo = \%hash — HASH slot (inner is the hash-table)
      ((hash-table-p inner)
       (setf (symbol-value (intern (concatenate 'string "%" uname) pkg))
             inner))

      ;; *foo = undef — no-op (or clear all slots, see undef below)
      ((or (null inner) (eq inner *pl-undef*)) nil)

      ;; Fallback: try treating as CODE (some callers pass function directly)
      ((functionp rhs)
       (setf (fdefinition (intern (concatenate 'string "PL-" uname) pkg))
             rhs)))))

(defun pl-glob-copy (dst-pkg dst-uname src-glob)
  "Copy all slots from src-glob into dst (pkg, uname)."
  (let ((sp (pl-glob-package src-glob))
        (sn (pl-glob-name src-glob)))
    ;; CODE
    (let ((src-sym (intern (concatenate 'string "PL-" sn) sp)))
      (when (fboundp src-sym)
        (setf (fdefinition (intern (concatenate 'string "PL-" dst-uname) dst-pkg))
              (fdefinition src-sym))))
    ;; SCALAR
    (let ((src-sym (intern (concatenate 'string "$" sn) sp)))
      (when (boundp src-sym)
        (setf (symbol-value (intern (concatenate 'string "$" dst-uname) dst-pkg))
              (symbol-value src-sym))))
    ;; ARRAY
    (let ((src-sym (intern (concatenate 'string "@" sn) sp)))
      (when (boundp src-sym)
        (setf (symbol-value (intern (concatenate 'string "@" dst-uname) dst-pkg))
              (symbol-value src-sym))))
    ;; HASH
    (let ((src-sym (intern (concatenate 'string "%" sn) sp)))
      (when (boundp src-sym)
        (setf (symbol-value (intern (concatenate 'string "%" dst-uname) dst-pkg))
              (symbol-value src-sym))))))
```

### Step 3 — Runtime: `pl-glob-undef` and `pl-glob-slot`

```lisp
(defun pl-glob-undef-name (pkg-str name-str)
  "undef *foo — clear all slots."
  (let* ((pkg (find-package (string-upcase pkg-str)))
         (uname (string-upcase name-str)))
    (let ((sym (intern (concatenate 'string "PL-" uname) pkg)))
      (when (fboundp sym) (fmakunbound sym)))
    (dolist (prefix '("$" "@" "%"))
      (let ((sym (intern (concatenate 'string prefix uname) pkg)))
        (when (boundp sym)
          (set sym (if (string= prefix "$") (make-pl-box *pl-undef*)
                       (if (string= prefix "@")
                           (make-array 0 :adjustable t :fill-pointer 0)
                           (make-hash-table :test 'equal)))))))))

(defun pl-glob-slot (glob slot)
  "Read *foo{SLOT}."
  (let* ((pkg (pl-glob-package glob))
         (uname (pl-glob-name glob))
         (slot-str (string-upcase (stringify-value slot))))
    (cond
      ((string= slot-str "CODE")
       (let ((sym (intern (concatenate 'string "PL-" uname) pkg)))
         (when (fboundp sym) (make-pl-box (fdefinition sym)))))
      ((string= slot-str "SCALAR")
       (let ((sym (intern (concatenate 'string "$" uname) pkg)))
         (when (boundp sym) (make-pl-box (symbol-value sym)))))
      ((string= slot-str "ARRAY")
       (let ((sym (intern (concatenate 'string "@" uname) pkg)))
         (when (boundp sym) (symbol-value sym))))
      ((string= slot-str "HASH")
       (let ((sym (intern (concatenate 'string "%" uname) pkg)))
         (when (boundp sym) (symbol-value sym))))
      ((string= slot-str "NAME")    (make-pl-box (pl-glob-name glob)))
      ((string= slot-str "PACKAGE") (make-pl-box (package-name (pl-glob-package glob))))
      ((string= slot-str "GLOB")    glob)
      (t *pl-undef*))))
```

### Step 4 — Runtime: `pl-local-glob` macro

For `local *foo = sub { ... }`. The common case is temporary function override.
Full-slot save/restore requires `unwind-protect`.

```lisp
(defmacro pl-local-glob (pkg-str name-str &body body)
  "Save all slots of *pkg::name, execute body, restore on exit."
  (let ((pkg-var  (gensym "PKG"))
        (uname-var (gensym "UNAME"))
        (saved-code  (gensym "SAVED-CODE"))
        (saved-had-code (gensym "HAD-CODE"))
        (saved-scalar (gensym "SAVED-SCALAR"))
        (saved-array  (gensym "SAVED-ARRAY"))
        (saved-hash   (gensym "SAVED-HASH")))
    `(let* ((,pkg-var  (find-package (string-upcase ,pkg-str)))
            (,uname-var (string-upcase ,name-str))
            (code-sym  (intern (concatenate 'string "PL-" ,uname-var) ,pkg-var))
            (,saved-had-code (fboundp code-sym))
            (,saved-code     (when ,saved-had-code (fdefinition code-sym)))
            (scalar-sym (intern (concatenate 'string "$" ,uname-var) ,pkg-var))
            (,saved-scalar   (when (boundp scalar-sym) (symbol-value scalar-sym)))
            (array-sym  (intern (concatenate 'string "@" ,uname-var) ,pkg-var))
            (,saved-array    (when (boundp array-sym) (symbol-value array-sym)))
            (hash-sym   (intern (concatenate 'string "%" ,uname-var) ,pkg-var))
            (,saved-hash     (when (boundp hash-sym) (symbol-value hash-sym))))
       (unwind-protect (progn ,@body)
         (if ,saved-had-code
             (setf (fdefinition code-sym) ,saved-code)
             (when (fboundp code-sym) (fmakunbound code-sym)))
         (when ,saved-scalar (setf (symbol-value scalar-sym) ,saved-scalar))
         (when ,saved-array  (setf (symbol-value array-sym)  ,saved-array))
         (when ,saved-hash   (setf (symbol-value hash-sym)   ,saved-hash))))))
```

### Step 5 — Transpiler: `*foo` expression (ExprToCL.pm)

In `gen_leaf`, replace the two glob-expression cases:

```perl
# Typeglob as value: *foo -> (pl-make-typeglob "main" "foo")
if ($content =~ /^\*(\w+)$/) {
  my $name = $1;
  my $pkg  = $self->environment ? $self->environment->current_package : 'main';
  return "(pl-make-typeglob \"$pkg\" \"$name\")";
}

# Package-qualified: *Pkg::foo -> (pl-make-typeglob "Pkg" "foo")
if ($content =~ /^\*(.*)::([^:]+)$/) {
  my ($pkg, $name) = ($1, $2);
  $pkg = 'main' if $pkg eq '';
  $self->environment->add_referenced_package($pkg) if $self->environment;
  return "(pl-make-typeglob \"$pkg\" \"$name\")";
}
```

### Step 6 — Transpiler: `*foo = RHS` assignment (ExprToCL.pm)

Extend the existing special case in `gen_binary_op` to use `pl-glob-assign` for
non-function RHS, while keeping the fast direct path for `#'...`:

```perl
# Typeglob assignment
if ($op eq '=' && $left =~ /^\(pl-make-typeglob "(.+)" "(.+)"\)$/) {
  my ($pkg, $name) = ($1, $2);
  # Fast path: direct function reference (most common case, keep static)
  if ($right =~ /^#'/) {
    my $cl_func = $self->cl_name($name);
    return "(setf (symbol-function '$cl_func) $right)";
  }
  # General dispatch at runtime
  return "(pl-glob-assign \"$pkg\" \"$name\" $right)";
}
```

### Step 7 — Transpiler: `undef *foo` (ExprToCL.pm)

In `gen_named_unary` or wherever `undef` is handled, check if the argument is a
typeglob expression:

```perl
# undef *foo → (pl-glob-undef-name "pkg" "name")
if ($func eq 'undef' && $arg =~ /^\(pl-make-typeglob "(.+)" "(.+)"\)$/) {
  return "(pl-glob-undef-name \"$1\" \"$2\")";
}
```

### Step 8 — Transpiler: `local *foo` (Parser.pm)

The `local` handler in `_process_local_statement` needs a new branch. When the
localized value is a typeglob, emit `pl-local-glob`:

```perl
# local *foo  →  (pl-local-glob "main" "foo" ...rest-of-block...)
# local *foo = sub {...}  →  also pl-local-glob, with RHS installed before body

if ($target =~ /^\(pl-make-typeglob "(.+)" "(.+)"\)$/) {
  my ($pkg, $name) = ($1, $2);
  # With initializer: install the RHS before the body
  my $install = $has_initializer
      ? "(pl-glob-assign \"$pkg\" \"$name\" $rhs)"
      : "";
  return "(pl-local-glob \"$pkg\" \"$name\"\n  $install\n  ...body...)";
}
```

The tricky part here is that `local` in Parser.pm generates the wrapping scope
via `let` bindings that then enclose the rest of the block. The typeglob case
needs to use `pl-local-glob` instead of `let`, wrapping the same remaining
statements. The implementation in Parser.pm should reuse the existing
"wrap remaining statements" mechanism already used for `local $scalar`.

### Step 9 — Transpiler: `*foo{SLOT}` slot access (PExpr.pm + ExprToCL.pm)

PPI parses `*foo{CODE}` as a `PPI::Token::Symbol` (`*foo`) followed by a
`PPI::Structure::Subscript` (`{CODE}`). The subscript case for symbols starting
with `*` falls through because there's no handler for it.

In `PExpr.pm`, when building the AST for a subscript on a `*foo` token, treat it
as a call to `pl-glob-slot`:

```
*foo{CODE}  →  (pl-glob-slot (pl-make-typeglob "main" "foo") "CODE")
```

This is lower priority than steps 1–8 because it's less commonly used in the
test suite.

---

## Export List

All new runtime symbols must be added to `(:export ...)` in `defpackage :pcl`:

```lisp
#:pl-glob   #:pl-glob-p  #:make-pl-glob
#:pl-glob-package  #:pl-glob-name
#:pl-make-typeglob
#:pl-glob-assign
#:pl-glob-copy
#:pl-glob-slot
#:pl-glob-undef-name
#:pl-local-glob
```

---

## What This Unlocks

### Direct (function/method aliasing works already, except with local)

| Test     | Failure mode          | What's needed             |
|----------|-----------------------|---------------------------|
| auto.t   | UNBOUND `*FOO`        | Step 5 (`*foo` as value)  |
| anonsub.t| UNBOUND `*DATA`       | Step 5 (+ `\*DATA` → glob ref) |
| method.t | package read error    | Separate issue; glob ops needed for `*A::x = *A::d`, `local *BB::d`, `undef *door::dohtem` |
| local.t  | wrong `local *f1`     | Step 8 (`local *foo = sub{}`) |
| hash.t   | `*guard = sub (&){}`  | Step 6 (already works via fast path) |
| aassign.t| `*xalias = \$x`       | Step 6 (scalar aliasing)  |
| concat.t | `*Bar = (...)`        | Step 6 (runtime dispatch) |

### Out of Scope (too complex or too rare)

- **IO slot / filehandle typeglobs**: `open *FH, "file"` — requires integrating
  the IO slot with PCL's filehandle system. Deferred.
- **Stash manipulation**: `*Pkg:: = *Other::` — aliasing entire packages.
  Very rare, deferred.
- **FORMAT slot**: Not used in test suite.
- **`tied` interface through typeglobs**: Requires `tie` support first.

---

## Implementation Order

1. Steps 1–3 (runtime structs + assign/undef/slot) — self-contained, no parser changes
2. Steps 5–6 (expression + assignment codegen) — uses the runtime from step 1
3. Step 7 (`undef *foo`) — small addition once step 5 is done
4. Step 8 (`local *foo`) — needs careful integration with Parser.pm's `local` handler
5. Step 9 (`*foo{SLOT}`) — lowest priority, needs PExpr changes

After steps 1–7, run the sweep and see what unblocks. The estimate of ~30 tests
is conservative; scalar aliasing alone (step 6 scalar case) could unblock more
in array.t and aassign.t.
