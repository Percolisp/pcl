---
name: reference_in_package_is_read_time
description: "A nested (in-package :X) inside one top-level CL form does NOT change how the symbols around it were read — that is why a `package X;` inside eval{}/do{} silently resolves globals in the enclosing package"
metadata: 
  node_type: memory
  type: reference
  originSessionId: 95276357-2e6f-490f-a270-0552f702799e
  modified: 2026-08-09T13:14:08.655Z
---

`in-package` takes effect at READ time, per top-level form. Common Lisp's
reader interns every symbol in a top-level form before evaluation begins, so
an `(in-package :Foo)` nested *inside* a form cannot re-home the symbols
written around it — they were already interned in whatever package was
current when the reader reached that form.

**Where this bites PCL (found s376, task #239):** a `package X;` inside an
`eval { … }` / `do { … }` block lowers to ONE top-level form carrying both
`(in-package :Foo)` and the block's statements. Every unqualified global in
that region therefore resolves in the ENCLOSING package — `eval { package
Foo; $z = "Z" }` writes `main::z`, where perl writes `Foo::z`. A FILE-level
`package X;` is correct only because D1-lite splits it into separate
top-level forms.

**The rule that follows:** any package switch that is not a top-level form
boundary must be expressed as QUALIFIED symbol names (`Foo::$z`), never as a
nested `in-package`. That is what #226 does for eval regions, and what
`_requalify_block_our_after_pkg_switch` (Pl/Parser2.pm) already does for
`our`-declared names in exactly this shape. Related: [[project_v2_session_state]].

Corollary when reading emitted CL: an `(in-package …)` at column 0 is real;
an indented one inside a larger form is decoration, and the symbols beside it
belong to the outer package.
