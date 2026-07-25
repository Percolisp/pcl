# `caller()` — package tracking implementation

How PCL makes `caller()` report the **package of the calling frame**, the
problem it solves, where the code lives, and what it costs.

> Scope note: this document is about the **package** field of `caller`. The
> filename/line fields remain best-effort (`"at (unknown) line 0"`) — that is a
> separate, deliberately-deferred item (see `docs/not-supported.md`,
> "`caller()` filename and line number"). The list-context return of the full
> 4-tuple also has a known `wantarray`-propagation gap on `(caller(0))[N]`
> slices (noted at the end).

---

## The problem

`caller` in scalar context returns the package name of the code that called the
current sub. Real CPAN modules rely on this at **import time** to discover the
*target* package they are being used from:

```perl
# Class::Method::Modifiers, Exporter, and many installer modules do this:
sub around { _install_modifier(scalar(caller), 'around', @_) }
sub import { my $target = caller; *{"${target}::thing"} = \&...; }
```

Before this change `p-caller` **hard-coded the package as `"main"`**:

```lisp
(setf frame-info (list "main" ...))   ; Package (simplified - always "main")
```

So `use Class::Method::Modifiers` inside `package Foo` installed the modifiers
into `main` instead of `Foo`, and CMM died with *"method 'greet' is not found in
the inheritance hierarchy for class main"*. Any caller-driven module was
silently misdirected.

### Why we can't just read the CL package

PCL upcases **single-segment** Perl package names into CL packages: `package
Foo;` becomes CL package `FOO` (multi-segment names like `Try::Tiny` are
pipe-quoted and keep their case). At runtime `(package-name *package*)` for code
in `Foo` is `"FOO"` — the original case `"Foo"` is **gone**. `caller` must
report `"Foo"`, so the original case has to be carried out-of-band; it cannot be
recovered from the CL package object.

(The Perl-level case *is* still correct everywhere a literal is emitted —
`__PACKAGE__`, `ref`, `bless "Foo"` — because the parser emits those as literal
strings. Only runtime introspection that goes through the CL package object
loses it.)

---

## The mechanism

Three dynamic variables and one setter in `cl/pcl-runtime.lisp`, plus emission
from `Pl/Parser.pm`.

### Runtime (`cl/pcl-runtime.lisp`)

```lisp
(defvar *pcl-current-package* "main")     ; orig-case Perl name of current code
(defvar *pcl-caller-pkg-stack* nil)        ; caller packages, pushed per sub call
(defvar *pcl-pkg-name-map* (make-hash-table :test 'equal))  ; CL name -> Perl name

(defun p-set-current-package (pkg perl-name) ...)  ; register + set current
(defun pcl-pkg-perl-name (cl-pkg) ...)             ; CL package -> orig Perl name
```

- **`p-set-current-package`** is called by generated code at each `package`
  statement. It records `CL-name → original-case` in `*pcl-pkg-name-map*` and
  sets `*pcl-current-package*` to the original case.
- **`p-sub`** (the macro that wraps every Perl sub body) now binds, per call:

  ```lisp
  (let* ((*pcl-caller-pkg-stack* (cons *pcl-current-package* *pcl-caller-pkg-stack*))
         (*pcl-current-package*  (pcl-pkg-perl-name (symbol-package ',name)))
         (*pcl-sub-call-depth*   (1+ *pcl-sub-call-depth*))
         (*pcl-caller-wantarray* *wantarray*))
    (catch :p-return ,@body))
  ```

  i.e. it **pushes the caller's current package** onto the stack, then rebinds
  current to the sub's *own* package (looked up via the registry, so the case is
  right for deeper `caller` calls too). The bindings are dynamic, so they unwind
  automatically when the sub returns.

- **`p-caller`** reads `(nth level *pcl-caller-pkg-stack*)` for the package
  instead of the hard-coded `"main"`. Out-of-range levels return `nil` (Perl
  `undef`), matching the existing top-level behaviour.

### Module loads must not leak their package

`*pcl-current-package*` is set imperatively by `p-set-current-package` as each
`package` statement executes. When a module is loaded mid-execution (`use`/
`require` → `p-load-module-cached`), the loaded file runs its *own* `package`
statements and would leave `*pcl-current-package*` pointing at that module's
last package after the load returns — corrupting the caller's view (and any
`caller()` / `overload::import` that reads it). So `p-load-module-cached`
**dynamically rebinds `*pcl-current-package*` around the load**:

```lisp
(let ((*pcl-current-package* *pcl-current-package*))   ; restored after load
  ... load the module ...)
```

The orig-case **name map** (`*pcl-pkg-name-map*`) is a separate global hash and
is intentionally *not* rebound — the loaded module's package→case mappings must
persist so `caller()` reports the right case for those packages later.

### Why a stack (not just one "caller package" variable)

`caller(N)` needs the package of the frame *N* levels up. Pushing the caller's
package at each sub entry makes `(nth N stack)` exactly that:

```
top-level (main) → A (pkg X) → B (pkg Y), B calls caller(0)/caller(1):
  on entering A: push "main"          stack = ("main")
  on entering B: push "X"             stack = ("X" "main")
  caller(0) in B = (nth 0) = "X"      (B was called from inside A, pkg X)  ✓
  caller(1) in B = (nth 1) = "main"   (A was called from top level, main)  ✓
```

### Codegen (`Pl/Parser.pm`)

The orig-case name lives only in the parser, so one runtime call per `package`
statement is unavoidable. To keep it elegant it was funnelled through a single
chokepoint and a single helper rather than sprinkled across the four
package-statement branches:

- New `_cl_pkg_designator($pkg_name)` — the **single source of truth** for the
  CL package designator (the `::`-or-CL-symbol-collision pipe-quote rule). It
  replaced three copies of that ternary.
- `_emit_package_preamble` now emits `(p-set-current-package <designator>
  "<orig-name>")` into the section's **runtime** bucket (execution order, *not*
  hoisted with the preamble) at both of its return points. This covers the
  top-level simple form, the top-level block form, and the inline-in-runtime-
  block form.
- The block-form restore and the inside-a-sub `package` switch emit the call
  explicitly (the inside-a-sub setf is restored on sub exit by `p-sub`'s dynamic
  binding).

---

## Files changed

| File | Change |
|------|--------|
| `cl/pcl-runtime.lisp` | Add `*pcl-current-package*`, `*pcl-caller-pkg-stack*`, `*pcl-pkg-name-map*`, `p-set-current-package`, `pcl-pkg-perl-name` (+ exports). `p-sub` macro: 2 extra dynamic bindings. `p-caller`: package from stack instead of `"main"`, with a placeholder-frame fallback when the SBCL backtrace walk can't locate a frame. `p-load-module-cached`: rebind `*pcl-current-package*` around the load so a loaded module's `package` statements don't leak into the caller. |
| `Pl/Parser.pm` | New `_cl_pkg_designator` helper (de-duplicates the triplicated ternary). `_emit_package_preamble` + the package-statement branches emit `p-set-current-package`. |
| `Pl/t/misc-fixes-01.t` | Regression tests (calling-frame package; single-segment case preserved). |

---

## Cost

### Execution time

Per **sub call**, `p-sub` now additionally does:

- one `cons` (push onto `*pcl-caller-pkg-stack*`) — a single heap allocation;
- one `symbol-package` + one `equal` hash lookup (`pcl-pkg-perl-name`);
- two extra dynamic-variable bindings (`*pcl-current-package*`,
  `*pcl-caller-pkg-stack*`) on top of the two already there.

This is O(1), allocation-light, and dwarfed by the work in any real sub body. No
measurable change in the gate (3253 tests) or the full sweep wall-clock
(~295 s, within run-to-run noise).

Per **`package` statement** (compile-time-rare): one extra runtime form
(`p-set-current-package`) — a hash insert + a `setf`. Negligible.

`caller()` itself is unchanged in cost: it already walked the SBCL backtrace for
filename/subname; reading `(nth level stack)` is trivial on top.

### Complexity

- **Runtime:** +~35 lines (three defvars, two small functions), +4 lines in the
  `p-sub` lambda. Conceptually one new idea: "the lexically-current Perl package
  is a dynamic variable, snapshotted per call onto a stack."
- **Parser:** net **simpler** — the new `_cl_pkg_designator` removed three copies
  of the pipe-quote ternary; the emission is one line at the chokepoint.
- **Memory:** the stack grows by one cons per active frame and unwinds on
  return; it never outlives the call chain.

### What it deliberately does **not** do

- It does **not** fix `caller`'s filename/line (still `"-"`/`0`) — that needs a
  source-map and is documented as deferred.
- `caller(0)` in **list context** still returns only the package today, because
  `(caller(0))[N]` slices don't propagate list `wantarray` to the call — a
  pre-existing, orthogonal gap (the package field, the part real modules use via
  `scalar(caller)`, is correct).
- It does **not** touch `p-get-class`/method dispatch — package *case* recovery
  for blessed objects already worked via literal strings.

---

## What pclxs needs from this, and what it will inherit

*Added 2026-07-26, from the pclxs side. This section is here rather than in
that repo because it is host-side knowledge: pclxs never names PCL.*

XS asks `caller` too, and the first measured case is **Params::Validate**,
whose `get_caller` walks perl's context stack for exactly one thing — the
package and sub name to put in an error message:

```c
/* Params-Validate-1.31/lib/Params/Validate/XS.xs */
while (cxix >= 0) {
    ... CXt_SUB / CXt_EVAL ...
    gv_efullname4(sv, CvGV(cx->blk_sub.cv), NULL, TRUE);   /* "Foo::bar" */
}
```

pclxs classifies this **B, not D** (`tools/xs-classify`): the *mechanism* is
perl's `cxstack`, which is interpreter guts, but the *question* is `caller`,
and a host has an answer. So when pclxs implements it, it will be a vtable
question answered by the host — which for PCL means this file's machinery.

**And it inherits this file's limits, deliberately.** The pclxs side has
been told to copy the behaviour rather than invent a better one:

| field | PCL today | what XS sees through pclxs |
|---|---|---|
| package | correct (this document) | correct |
| filename | `"-"` / unknown | `"-"` — error messages will say so |
| line | `0` | `0` |
| sub name (`(caller(N))[3]`) | not returned; the list-context 4-tuple has the `wantarray` gap above | pclxs cannot fill it in either |

The consequence is narrow and worth writing down before someone reports it
as a pclxs bug: **XS-generated diagnostics will carry `(unknown) line 0`
and, where a module prints the calling sub's name, an incomplete one.**
Params::Validate's message degrades from
`Foo::bar(): the 'x' parameter is required` to something without the
`Foo::bar` part. The validation itself is unaffected — no module measured
so far *branches* on caller's filename or sub name, they only print them.

**Where fixing it would pay twice.** A source map (this file's deferred
item) would fix perl-side `caller` and XS-side diagnostics in one move,
since the pclxs entry would just forward. That is the argument for doing
it here rather than working around it in the shim: the shim cannot
synthesise a filename it was never given.
