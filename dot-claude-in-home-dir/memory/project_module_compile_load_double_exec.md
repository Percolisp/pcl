---
name: project_module_compile_load_double_exec
description: "Moo subclass empty-attrs ROOT CAUSE FOUND (s251): module compile-file+load DOUBLE-EXECUTION. A `sub NAME` re-installs at BOTH compile-file and load; a guarded BEGIN-time redefine (||= / %DEFERRED) runs only at compile → load-pass sub-def CLOBBERS it. GENERAL bug (Moo/Moose/Sub::Defer/Sub::Quote/Type::Tiny). Confirmed fix: single-pass load. NOT coderef-identity (s249/s250 theory superseded)."
metadata: 
  node_type: memory
  type: project
  originSessionId: a2e18eb2-c4ef-4664-b55c-f871695cc0a3
---

# Module compile-file+load double-execution — Moo subclass root cause (session 251)

## THE BUG (general, not Moo-specific)
PCL caches modules by `compile-file`→`.fasl` then `load` **in the same process**
(`*pcl-cache-fasl*` defaults `t`; `p-load-module-cached`, cl/pcl-runtime.lisp
~8012). `p-sub` wraps every sub in `(eval-when (:compile-toplevel :load-toplevel
:execute) (setf (symbol-function …) (lambda …)))`, so a `sub NAME {…}` is
**installed at BOTH compile-file time AND load time**. A `BEGIN`-block redefine
that is **guarded by an idempotency check** (`||=`, `%DEFERRED`, `%MAKERS`,
`%INC`, `unless defined &…`) runs only on the FIRST (compile) pass; on the load
pass the guard is already set so the redefine is SKIPPED — but the plain
`sub NAME` re-runs and **CLOBBERS the redefinition with the original body**.
Net: original `sub` body wins, the runtime replacement is lost.

## Minimal GENERAL repro (9 lines, no Moo) — /tmp/GuardBoot.pm + /tmp/guard.pl
```perl
package GuardBoot; our $DONE;
sub greet { return "BOOTSTRAP"; }
BEGIN { $DONE ||= do { no warnings 'redefine';
  *GuardBoot::greet = sub { return "REPLACED"; }; 1; }; }
1;
# main:  use GuardBoot; print GuardBoot->greet;
```
perl → `REPLACED`; PCL (fasl default) → **`BOOTSTRAP`** (WRONG).
The two ingredients that BOTH must be present: (1) redefine at **BEGIN/compile**
time, (2) **guarded** so it's skipped at load. Plain (non-BEGIN) guarded redefine
does NOT trip it (it runs at load, after the sub-def, so it wins).

## Moo instantiation of the pattern (Method/Generate/Constructor.pm)
`sub new { delete _getstash(__PACKAGE__)->{new}; bless $class->BUILDARGS(@_) }`
(bootstrap) + top-level `Moo->_constructor_maker_for(MGC)->install_delayed`
which `defer_sub "MGC::new"` (installs a deferred stub, guarded by
`$MAKERS{$target}{constructor} ||= …`). Compile-file: bootstrap installed,
`_constructor_maker_for` runs, stub installed (id2), MAKERS cached. Load:
`(p-sub pl-new)` RE-installs bootstrap (id3) over the stub; `||=` skips the
stub reinstall. → MGC::new = bootstrap again. First `MGC->new` (a SUBCLASS like
`Dog extends Animal`, via `(ref $con)->new`) runs the bootstrap, which
`delete`s MGC::new (fmakunbound) → next dispatch falls through @ISA to
`Moo::Object::new` → bare `bless {}` → **empty attrs**. Single-class Moo works
only because its leaf deferred ctor (Animal::new) is called once and the
MGC::new clobber is invisible.

## How it was found (white-box, the right approach per s249/s250 notes)
Shadowed real Moo MGC + Sub::Defer into lib/ with warns; added env-guarded
traces (`PCL_MGC_DEBUG`) to `p-method-call`, `%p-glob-assign-slots`, `p-delete`,
and the `p-sub` installer printing `(if *compile-file-pathname* "COMPILE"
"LOAD")`. Trace showed: `PSUB phase=COMPILE`, bootstrap+stub (compile),
`PSUB phase=LOAD` (clobber), then Dog→empty. ALL debug + shadows REMOVED after
(runtime back to pristine zero-diff).

## SUPERSEDES prior theory
s249/s250 blamed "unstable coderef identity" / "2nd bootstrap-body function
(0x4)". The s249 stable-object-id fix (`bacf448`) was real+needed (killed an
infinite goto loop) but was NOT the empty-attrs cause. The "2nd bootstrap-body
function" = the **load-pass re-install** (a genuinely different `(lambda …)`
object from the compile-pass one). Not GC/boxing.

## CONFIRMED FIX (validated, not yet implemented)
Load modules SINGLE-PASS. With `*pcl-cache-fasl* nil` (load .lisp as source),
moo_probe.pl gives `Dog name=B, breed=lab` and `MGC_GENERATE_METHOD` fires for
MGC+Animal+Dog. Fix options (ARCHITECTURAL — discuss tradeoffs):
- **A. load modules as source** (no compile-file). Correct, simplest. Cost: no
  compiled fasl → slower module loads, esp. sweep (fresh process per test).
- **B. build fasl in a subprocess, load fasl in-process** (single exec in loader).
  Keeps fasl. More complex.
- **C. transpiler: narrow eval-when** so compile-file does NOT execute the
  module's runtime/BEGIN body — only emit declarations for compile-visibility;
  body runs once at load. Correct + keeps fasl, but transpiler change, riskier.
- **D. on cache miss: load source (1x) + spawn fasl build for next run;
    cache hit: load fasl (1x).** Best perf/correctness balance.

Comment at cl/pcl-runtime.lisp ~8001 already half-knew: "p-sub's eval-when
:compile-toplevel shadow calls run during compile-file, then …re-runs at load
time — harmless but noisy." It is NOT harmless.

See [[project_moo_progress]], [[project_coderef_identity_blocker]].
