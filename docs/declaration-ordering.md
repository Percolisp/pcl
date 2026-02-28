# Declaration Ordering: Perl vs Common Lisp

## The Problem

Perl has two distinct phases: **compile** and **run**. CL source loading is **sequential** (each top-level form is compiled+executed in order). This difference causes ordering problems when transpiling.

## Perl Semantics

### Subs are compiled at BEGIN time
In Perl, `sub foo { ... }` is compiled during the compilation phase, BEFORE any runtime code executes. This means:
```perl
foo();          # Works! Sub is already compiled
sub foo { 42 }  # Compiled before foo() runs
```

### Variables declared with `our`/`my` at file scope
`our $x` creates a package variable. The DECLARATION happens at compile time, but the ASSIGNMENT runs at runtime:
```perl
our $x = 10;  # Declaration at compile time, assignment at runtime
```

### `local` creates dynamic bindings
`local $x = 20` temporarily overrides `$x` for the duration of the scope AND all functions called from that scope (dynamic scoping).

### BEGIN blocks
`BEGIN { ... }` runs immediately at compile time, in source order. Variables and subs declared before a BEGIN block are available inside it.

## Common Lisp Semantics

### `defvar` proclaims "special" (dynamic)
`(defvar $x ...)` does two things:
1. Proclaims `$x` as a **special** (dynamically scoped) variable — globally and permanently
2. Sets initial value (only if not already bound)

### `let` behavior depends on special proclamation
- If variable IS proclaimed special: `let` creates a **dynamic** binding (visible in called functions)
- If variable is NOT proclaimed special: `let` creates a **lexical** binding (only visible in current scope)

**Critical**: The proclamation must happen BEFORE the `defun` containing the `let` is compiled. Otherwise SBCL compiles a lexical binding:

```lisp
;; WORKS: defvar before defun
(defvar $x 10)
(defun test () (let (($x 20)) (get-x)))  ;; dynamic binding

;; BROKEN: defvar after defun
(defun test () (let (($x 20)) (get-x)))  ;; lexical binding!
(defvar $x 10)
```

### `eval-when` does NOT reorder
`(eval-when (:compile-toplevel :load-toplevel :execute) ...)` controls WHICH phases a form runs in, but NOT the order. Forms still execute sequentially in source order.

### `compile-file` vs `load`
`:compile-toplevel` only fires during `compile-file`, not during `load` of source files. For our use case (loading .lisp source files), only `:load-toplevel` and `:execute` matter.

### Symbol interning and `shadowing-import`
When the CL reader encounters `(pl-basename ...)`, it interns a new symbol in the current package if one doesn't exist. If a `(pl-use ...)` later imports that function via `shadowing-import`, the package's symbol mapping is updated — but any already-compiled code still holds a reference to the **old** uninterned symbol. This means:

```lisp
;; BROKEN: defun before import
(defun my-func () (pl-basename "/a/b"))  ;; interns main::pl-basename
(pl-use "File::Basename" :imports '("basename"))  ;; replaces mapping
;; my-func still calls the dead old symbol!

;; WORKS: import before defun
(pl-use "File::Basename" :imports '("basename"))  ;; imports pl-basename
(defun my-func () (pl-basename "/a/b"))  ;; uses imported symbol
```

This is the key motivation for the two-phase approach: `use` statements must be processed before sub definitions that reference imported functions.

## Our Solution: Two-Phase Declaration Reordering

In `Parser.pm::_insert_sub_forward_declarations()`:

### Phase 1: Hoist defvars + emit stubs

1. **Extract** all `(eval-when ... (defvar ...))` blocks from their original positions
2. **Scan** the full output for package references (e.g., `TestMod::pl-foo`)
3. **Re-insert** at the top of each package section, right after `(in-package :pkg)`:
   - `defpackage` for any referenced packages
   - `defvar` declarations (so variables are proclaimed special before any defun)
   - `pl-declare-sub` stubs for subs NOT found at top level (nested subs only)
4. **Leave** sub bodies and value assignments (`setf`, `box-set`) at their original positions

### Phase 2: Reorder compile-time vs runtime forms

Within each package section, **partition** output into compile-time and runtime forms, preserving source order within each group:

**Compile-time forms** (recognized by first line):
- `(pl-sub ...)` — sub definitions, including `use constant` (expand to eval-when)
- `(eval-when ... :load-toplevel ...)` — `use`/`require` statements, `defvar` wrappers
- `(defpackage ...)` — package declarations
- `(defclass ...)` — CLOS classes for MRO
- `(defconstant ...)` — constant definitions
- `(pl-declare-sub ...)` — forward stubs
- `(unless (fboundp ...)` — inline forward stubs
- `(push (lambda ...)` — END block registrations
- `(let ...)` containing `(pl-sub ...)` — state variable closures

**NOT compile-time** (stays in source order):
- `(eval-when (:compile-toplevel :execute) ...)` — BEGIN blocks (no `:load-toplevel`).
  In interpreted mode (`--load`), BEGINs run sequentially when encountered.
  Reordering them before runtime code would break the expected sequential semantics.

**Runtime forms**: everything else (`setf`, `box-set`, `pl-print`, function calls, control flow, etc.)

Output order per package section:
```
[defvars + defpackages + stubs]         ← inserted by Phase 1
[compile-time forms in source order]    ← reordered by Phase 2
[runtime forms in source order]         ← reordered by Phase 2
```

### Why two phases?

Phase 1 must run first because defvar extraction changes line indices. Phase 2 then operates on the stable output to reorder chunks.

Phase 2 runs unconditionally (even without declared subs) because `use constant` generates `(pl-sub ...)` forms without registering as declared subs. Without Phase 2, files containing only constants and runtime code would not be reordered.

The old approach (single phase) extracted sub bodies and moved them to the absolute top of each section. This caused the symbol conflict described above: a hoisted sub referencing an imported function would intern a dead symbol before the `use` import ran.

The new approach keeps sub bodies in source order relative to other compile-time forms (especially `use` statements), while still ensuring they appear before any runtime top-level calls.

## Edge Cases and Corner Cases

All of these are covered by tests in `Pl/t/decl-ordering-01.t`.

### BEGIN blocks stay in source order
BEGIN blocks generate `(eval-when (:compile-toplevel :execute) ...)` — note: NO `:load-toplevel`.
Phase 2 does NOT classify these as compile-time, so they stay in source order among runtime forms.
This is correct for interpreted mode (`--load`), where BEGINs run sequentially when encountered.

Key implications:
- `defvar` (with nil default) runs before everything (Phase 1 hoists it), so variables exist
- Value assignments (`setf`) stay at original positions, so BEGIN sees undef (same as Perl)
- BEGINs see subs defined **before** them (subs are reordered to compile-time group) but
  see other BEGINs and runtime code in source order

### `use` before subs
The motivating case. With the two-phase approach:
```perl
use File::Basename qw(basename);
sub f { basename("/a/b.t") }
print f();
```
Generates (in order):
1. `(eval-when ... (pl-use "File::Basename" ...))` — compile-time (has `:load-toplevel`), imports pl-basename
2. `(pl-sub pl-f ...)` — compile-time, references the correctly imported pl-basename
3. `(pl-print (pl-f))` — runtime

### Forward calls
```perl
foo();
sub foo { print "ok\n" }
```
`(pl-sub pl-foo ...)` is compile-time, `(pl-foo)` is runtime. Phase 2 reorders the sub before the call.

### Interleaved subs and runtime
```perl
sub a { return 1; }
print "x";
sub b { return a() + 1; }
print "y";
```
Phase 2 moves both subs before both prints, preserving source order within each group:
`pl-sub a` → `pl-sub b` → `print "x"` → `print "y"`.

### Mutual recursion
```perl
sub is_even { ... is_odd($n - 1) ... }
sub is_odd  { ... is_even($n - 1) ... }
```
Both are compile-time forms. Source order is preserved. CL allows forward function references
(the call is resolved at runtime when the function exists), so this works.

### Multi-package files
Each package section is reordered independently. The `in-package` directive determines section boundaries.
If the same package appears twice, each occurrence is a separate section with independent reordering.

### `use constant` (generates `pl-sub`)
`use constant FOO => 42` generates `(pl-sub pl-FOO () 42)`. This is classified as compile-time
and reordered before runtime code. However, `use constant` does NOT call `add_declared_sub`,
which is why Phase 2 must run unconditionally (not gated on `@$subs`).

```perl
use constant A => 1;
$x = A;
use constant B => 2;  # must be reordered before $x = A
print $x + B;
```

### `local` in called functions (defvar ordering)
This is the main reason defvar ordering matters. Without it:
```perl
our $x = 10;
sub get_x { return $x }
sub test { local $x = 20; say get_x() }  # Should print 20, not 10
```
The `local $x = 20` compiles to `(let (($x 20)) ...)`. For this to create a dynamic binding visible
in `get_x`, `$x` must be proclaimed special (via `defvar`) BEFORE `test` is compiled.

Works correctly with multiple variables and triple-deep nesting:
```perl
our $x = "global";
sub show { return $x }
sub wrap { local $x = "wrapped"; return deeper() }
sub deeper { local $x = "deep"; return show() }
wrap();  # returns "deep" — innermost dynamic binding visible
show();  # returns "global" — restored after scope exit
```

### defvar value assignment stays in runtime
`our $x = 10` splits into:
- `(eval-when ... (defvar $x (make-pl-box nil)))` — Phase 1 hoists to top
- `(setf (pl-box-value $x) 10)` — stays in runtime at original position

This ensures the declaration (special proclamation) happens before any `defun`, while the
value assignment runs at its original runtime position.

### Nested subs get `pl-declare-sub` stubs
```perl
sub outer {
    sub inner { return 42; }
    return inner();
}
```
`inner` is NOT at top level, so Phase 1 emits `(pl-declare-sub pl-inner)` before `pl-sub pl-outer`.
Top-level subs do NOT get stubs (they're reordered by Phase 2 instead).

### Qualified subs trigger `defpackage`
```perl
sub A::DESTROY { ... }
```
Phase 1 scans for package references and emits `(defpackage :A (:use :cl :pcl))` before
any code that references `A::` symbols. Pipe-quoted packages (`|Foo::Bar|::`) are also detected.

### END blocks
`END { ... }` generates `(push (lambda ...) *end-blocks*)`, classified as compile-time.
Multiple END blocks interleaved with subs preserve source order within the compile-time group:
`sub a` → `END1` → `sub b` → `END2` → runtime.

### Auto-vivified globals (`_insert_variable_forward_declarations`)
Runs AFTER Phase 2. Inserts `(defvar $x ...)` for undeclared package variables.
These defvars go after `(in-package ...)`, before Phase 2's reordered output, so they
precede any sub definitions that reference the variable.

## Known Limitations

### `sub` before `use` (reverse order)
```perl
sub process { return basename($0); }  # basename not yet imported
use File::Basename qw(basename);      # imports at compile-time
print process();
```
In Perl, this works because function lookup is late-bound (resolved at call time).
In CL, `pl-basename` gets interned as a dead local symbol before `shadowing-import` replaces it.
Phase 2 preserves source order within compile-time forms, so the sub stays before the `use`.

**Workaround**: Write `use` before `sub` (the normal Perl convention).

### BEGIN in interpreted mode
BEGINs don't truly run at compile time in `--load` mode. They run sequentially when encountered.
This means `BEGIN { $x = 42 }` followed by `print $x` works, but `print $x` followed by
`BEGIN { $x = 42 }` prints undef (Perl would print 42 because BEGIN runs at compile time).

## Test Coverage

`Pl/t/decl-ordering-01.t` has 50 tests covering all the above cases:
- Parse-level tests verify output structure (compile-time before runtime, defvar hoisting, stubs)
- Runtime tests verify semantic correctness (forward calls, mutual recursion, local/dynamic scoping,
  cross-package calls, nested subs, use constant, BEGIN+require interactions)
