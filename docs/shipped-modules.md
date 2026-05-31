# Shipped & Overridden Modules

How PCL provides implementations for the standard/CPAN modules a Perl program
pulls in with `use`/`require` — and how `use Foo` decides *which* implementation
to load.

This is the architecture behind "Test::More is hardly the last case": every core
module PCL needs to supply (especially the XS ones) should slot into **one
convention and one registry**, not a new special case each time.

---

## 1. Two homes, split by implementation language

A module PCL ships lives in exactly one of two places, chosen by *how it has to
be implemented*:

| If the CPAN module is… | Ship it as… | Home | Loaded by |
|---|---|---|---|
| pure Perl, or has a pure-Perl fallback | a `.pm`, transpiled like any module | `lib/` | normal `@INC` transpile path |
| XS, or needs runtime/host integration | hand-written Common Lisp | `cl/modules/` | `p-load-extension` |

This split already exists in practice — it just isn't named:

- **`lib/`** (pure-Perl shims, transpiled on demand): `Config`, `POSIX`,
  `Errno`, `Cwd`, `version`, `File::Spec`, `File::Spec::Functions`,
  `Scalar::Util`, `List::Util`, `Test::Simple`.
- **CL-backed** (today flat in `cl/`, to move under `cl/modules/`):
  `cl/pcl-pack.lisp` (`pack`/`unpack`), `cl/pcl-test.lisp`
  (`Test::More`/`Test::Simple` TAP).

### The "which home?" heuristic

1. **Is it pure Perl on CPAN (or does it have a `PP` fallback)?** → `lib/` as a
   `.pm`. Cheapest: it rides the existing transpiler, no CL to maintain.
   (`List::Util`/`Scalar::Util` are XS on CPAN but have pure-Perl forms, so they
   live in `lib/`.)
2. **Is it XS with no pure-Perl form, or does it need to reach the host** (TAP +
   `$?` + exit for testing, raw memory for `pack`, the SBCL clock for
   `Time::HiRes`)? → `cl/modules/NAME.lisp`, hand-written CL.
3. **Is it XS and out of scope?** → no implementation; a registry `:xs` entry so
   `use` fails cleanly (see §3).

Prefer home #1 whenever a pure-Perl form is feasible — CL-backed modules are more
work to write and maintain.

---

## 2. The engine already exists — `p-load-extension`

CL-backed modules are not a new mechanism; they are **extensions**, already
documented in `docs/extensions.md`:

- `p-load-extension "NAME"` loads `NAME.lisp` relative to
  `*pcl-runtime-directory*` (captured from `*load-truename*` at runtime load),
  once, recording it in `*pcl-loaded-extensions*` so repeats are no-ops.
- Files must start with `(in-package :pcl)` and export their symbols.
- Eager-load lines at the bottom of `pcl-runtime.lisp` pull in built-ins
  (`(p-load-extension "pcl-pack")`); self-loading stubs are the lazy fallback.

The only structural change is to give these files a folder — `cl/modules/` — and
to teach `p-load-extension` (or the registry below) to look there.

---

## 3. The missing piece — one provider registry

Today three things decide what `use Foo` does, and they're scattered:

- `*p-xs-only-modules*` — a skip-list of XS modules to refuse;
- the eager `p-load-extension` list at the bottom of the runtime;
- `cl/pcl-test.lisp`, loaded *only by the sweep*, invisible to user scripts.

Collapse all three into a single table that `p-use` consults first:

```lisp
;; Perl module name → how to satisfy `use`/`require` of it.
;;   :cl   NAME  → (p-load-extension "NAME")   ; cl/modules/NAME.lisp
;;   :perl       → fall through to the lib/ + @INC transpile path
;;   :xs         → no implementation; clean "unavailable" error/skip
(defparameter *pcl-module-providers*
  '(("Test::More"        :cl   "test-more")
    ("Test::Simple"      :cl   "test-more")
    ("Test2::Bundle::More" :cl "test-more")
    ("List::Util"        :perl)          ; lib/List/Util.pm
    ("Scalar::Util"      :perl)          ; lib/Scalar/Util.pm
    ("Storable"          :cl   "storable")
    ("Time::HiRes"       :cl   "time-hires")
    ("Socket"            :xs)))           ; out of scope → clean error
```

### Resolution order in `p-use`

```
use Foo;
  ├─ 1. provider registry says :cl NAME  → (p-load-extension NAME), register
  │                                          preloaded, run Foo->import
  ├─ 2. provider registry says :perl, OR no entry
  │        → normal path: lib/ shim if present, else user @INC (transpile+cache)
  └─ 3. provider registry says :xs (no impl) → clean "Foo is not available
            under PCL (XS module)" error / sweep-skip
```

This is a strict generalisation of what `p-use` does now: step 1 absorbs the
eager extension list and the Test::More special-case; step 3 absorbs
`*p-xs-only-modules*`; step 2 is unchanged.

### Why this answers "Test::More is hardly the last case"

With the registry in place, **a new CL-backed core module is one file plus one
row** — `cl/modules/storable.lisp` + `("Storable" :cl "storable")` — and it is
immediately visible to user scripts *and* to PCL's own suite, through the same
code path. No new special-casing, ever.

---

## 4. Relationship to the FASL cache

CL-backed modules load through `p-load-extension`, so they compile/cache like any
extension. A `:perl` shim in `lib/` is an ordinary transpiled module and so
appears in a script's **`use`-closure** — meaning the `.deps` manifest from
`docs/fasl-caching-design.md` already covers it for `--cache`/`--fasl`
freshness. No additional caching machinery is needed for shipped modules.

CL-backed (`:cl`) modules are *part of PCL*, not user code: their freshness is
tied to the PCL install/core, not the per-script `.deps` manifest. Rebuild the
saved core (`pcl --make-core`) after editing one, exactly as for the runtime.

---

## 5. Migration (mechanical, low risk)

1. `mkdir cl/modules/`; move `cl/pcl-pack.lisp` → `cl/modules/pack.lisp`,
   `cl/pcl-test.lisp` → `cl/modules/test-more.lisp` (keep `(in-package :pcl)` +
   exports). Update the eager-load lines / `*pcl-runtime-directory*` join to look
   in `cl/modules/`.
2. Add `*pcl-module-providers*` and the step-1/step-3 lookups in `p-use`.
3. Fold the current `*p-xs-only-modules*` entries in as `:xs` rows; delete the
   standalone list (or keep it as the data source for the `:xs` rows).
4. Add the `("Test::More" :cl "test-more")` rows — this is the change that lets a
   user script `use Test::More` under plain `pcl` (see the rollout plan, the
   "run your own tests" phase).

Order matters only in that step 2 should land before step 4 relies on it.

---

## 6. Summary

- **Two homes:** pure-Perl shims in `lib/` (transpiled), CL-backed overrides in
  `cl/modules/` (via `p-load-extension`). Prefer Perl when feasible.
- **One registry** (`*pcl-module-providers*`) makes `use Foo` resolution
  deterministic and unifies the three current ad-hoc mechanisms.
- **Extensible by construction:** each future core/XS module = one file + one row.
- **Caching is already handled:** `:perl` shims via the `use`-closure manifest;
  `:cl` modules via the PCL install/core.

## See also

- `docs/extensions.md` — the `p-load-extension` mechanism this builds on.
- `docs/fasl-caching-design.md` — why `:perl` shims are covered by the `.deps`
  manifest and `:cl` modules by the core.
- `docs/pcl-command-plan.md` — the `pcl`/`pclbuild` commands.
- `docs/pcl-rollout-plan.md` — the phased build order (this registry is a phase).
- `cl/pcl-runtime.lisp` — `p-use`, `p-load-extension`, `*pcl-loaded-extensions*`,
  `*p-xs-only-modules*`, `*pcl-runtime-directory*`.
