# Module compile-file+load DOUBLE-EXECUTION bug — proper fix is NEXT SESSION

> ► **NEXT SESSION OPENS HERE.** The decision to make first: **what to do about
> saving/loading FASL.** Right now modules load as *source* (correct but slower);
> we want compiled-FASL speed back without re-introducing the double-execution.
> Pick from the options in "NEXT SESSION — proper fix" below (lean C or D), then
> implement and flip `*pcl-cache-fasl*` back to `t`. Everything needed to decide
> is in this doc.

**Status (session 251):** root cause found and documented; a *correctness*
workaround is in place (`*pcl-cache-fasl*` defaults to **nil** → modules load
as source, single-pass). The **proper fix that keeps FASL caching is deferred
to next session** (this doc is the handoff).

## The bug

PCL caches modules by `compile-file` → `.fasl` then `load`, **in the same
process** (`p-load-module-cached`, `cl/pcl-runtime.lisp`). `p-sub` installs each
sub inside `(eval-when (:compile-toplevel :load-toplevel :execute)
(setf (symbol-function …) (lambda …)))`, so `sub NAME {…}` is installed **at
both compile-file time and load time**.

When a module **redefines a sub at BEGIN/compile time** and that redefine is
**guarded by an idempotency check** (`||=`, `%DEFERRED`, `%MAKERS`, `%INC`,
`unless defined &…`):

1. **compile-file pass:** `sub NAME` installs the original body; the BEGIN
   redefine runs and replaces it; the guard flag is set.
2. **load pass:** `sub NAME` re-installs the **original** body (clobber); the
   guarded BEGIN redefine is now **skipped** (guard already set), so the clobber
   is never undone.

Net: the original `sub` body wins; the runtime replacement is lost.

## Minimal repro (no Moo) — `/tmp/GuardBoot.pm` + `/tmp/guard.pl`

```perl
package GuardBoot; our $DONE;
sub greet { return "BOOTSTRAP"; }
BEGIN { $DONE ||= do { no warnings 'redefine';
  *GuardBoot::greet = sub { return "REPLACED"; }; 1; }; }
1;
# main: use GuardBoot; print GuardBoot->greet, "\n";
```

`perl -Ilib /tmp/guard.pl` → `REPLACED`.
PCL with FASL caching on → `BOOTSTRAP` (WRONG).
PCL with `*pcl-cache-fasl* nil` → `REPLACED` (correct).

Both ingredients are required: redefine at **BEGIN/compile** time AND **guarded**
so it skips on the load pass. A non-BEGIN guarded redefine runs at load *after*
the sub-def, so it wins — no bug.

## Why it broke Moo

`Method/Generate/Constructor.pm`: bootstrap `sub new` + top-level
`install_delayed` → `defer_sub "MGC::new"` guarded by
`$MAKERS{$target}{constructor} ||= …`. Load-pass re-install of the bootstrap
clobbers the deferred constructor; the bootstrap's
`delete _getstash(MGC)->{new}` then leaves `MGC::new` unbound, so the first
`MGC->new` for a **subclass** (`Dog extends Animal`, dispatched via
`(ref $con)->new`) falls through `@ISA` to `Moo::Object::new` → `bless {}` →
**empty attrs**. Single-class Moo worked by luck (leaf ctor called once).

This **supersedes** the earlier "unstable coderef identity" theory (s249/s250).
The s249 stable-object-id fix was real (killed an infinite `goto` loop) but was
not the empty-attrs cause; the "2nd bootstrap-body function" was just the
load-pass re-install (a distinct lambda object).

## The workaround in place (session 251)

`*pcl-cache-fasl*` now defaults to **nil**. Modules cache as `.lisp` and load as
**source** → single pass → correct. Cost: no compiled FASL for modules → slower
module loads, most visibly in the full `perl-tests/` sweep (each test is a fresh
process that re-loads modules).

## NEXT SESSION — proper fix (keep FASL, kill the double-exec)

The loading process must execute the module body **exactly once**. Preferred
options (decided with the user that A above is only a stopgap):

- **C (principled):** in the transpiler, narrow the `eval-when` so `compile-file`
  does **not execute** the module's runtime / `BEGIN` body — emit declarations
  only for compile-time visibility, run the body once at load. Keeps FASL.
  Touches the transpiler module-body wrapper (find where the module body is
  wrapped to run at `:compile-toplevel`).
- **D (perf-balanced):** on cache **miss**, `load` the `.lisp` as source (1×)
  and spawn a subprocess to build the `.fasl` for next time; on cache **hit**,
  `load` the prebuilt `.fasl` (1×). Single execution in the loading process
  either way.
- **B:** build the FASL in a subprocess, then `load` the FASL in-process.

When implemented, flip `*pcl-cache-fasl*` back to `t` and re-verify both the
`GuardBoot` repro and `Moo` subclass (`/tmp/moo_probe.pl` → `Dog name=B,
breed=lab`) still pass, then re-run the full sweep for the perf win.

See `memory/project_module_compile_load_double_exec.md`.
