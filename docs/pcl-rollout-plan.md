# PCL User-Facing Rollout Plan

The phased build order for everything discussed in the `pcl` command thread:
the `pcl` runner, saved core, the caching tiers + `.deps` manifest, the module
provider registry, and running users' own Perl test suites.

**Guiding principle (the user's):** keep the first version simple. Each phase
ships something usable on its own, and earlier phases don't depend on later ones.

Design references: `docs/pcl-command-plan.md` (command interface),
`docs/fasl-caching-design.md` (caching/manifest), `docs/shipped-modules.md`
(module registry).

---

## Where we're starting from

The transpile-and-run mechanics already exist as dev helpers: `runpl` (transpile
+ run a snippet/file through the full pipeline, runtime + test lib loaded) and
`runt`/`clt` (per-test-file). **`pcl` is essentially `runpl` productionised** —
argument handling, `@ARGV` injection, a clean exit code, and the caching tiers.
So Phase 1 is polish around proven plumbing, not new ground.

---

## Phase 1 — the `pcl` runner (Tier 0: transpile-and-run) ✅ DONE

The minimum useful command: run a script or inline code, no FASL, no cache.

**Deliver:**
- `pcl script.pl [args…]` — transpile `script.pl` → temp `.lisp` → load under
  SBCL (runtime or core) → delete temp on exit.
- `pcl -e 'code' [args…]` and `pcl -E 'code'` (E is a plain alias of e).
- `-I dir` (prepend `@INC`), `-M Module` (`use` before the script),
  `-c` (syntax-check only → print "Syntax OK", exit), `-w`.
- `@ARGV` injected via an `--eval` form before `--load` (see command plan §SBCL
  invocation).
- One-shot maintenance actions that need no script: `--clear-cache`,
  `--make-core` (stub the bodies now; fill in Phases 2/3).

**Files:** create `pcl` (Perl, per the sketch in `docs/pcl-command-plan.md`).
No runtime or transpiler changes.

**Verify:**
- `pcl -e 'print "hi\n"'` → `hi`.
- `pcl t/some_non_test_script.pl arg1 arg2` matches `perl` for a handful of
  scripts that don't use Test::More yet.
- Exit code propagates (`pcl -e 'exit 3'; echo $?` → 3).

**Done when:** `pcl` runs arbitrary non-test scripts as a drop-in for `perl` on
the subset PCL already supports.

**Shipped (this session):** `pcl` created at repo root. `-e`/`-E`/`-I`/`-M`/`-c`/
`-w`/`-v`/`-h` + `@ARGV` injection (explicit `setf` of `pcl::@ARGV` to a raw-string
vector, overriding the `*posix-argv*`-derived default). Tier-0 only: the main
script is transpiled to a temp `.lisp` and loaded directly (never compiled to a
main-script FASL); the module cache applies underneath. Output is clean — the
runtime's "PCL Runtime loaded" banner was moved to `*error-output*`
(`cl/pcl-runtime.lisp`), and the SBCL load is wrapped in a warning-muffling,
non-verbose `(load …)` so only the program's own stdout shows. Verified output
matches `perl` on sample programs; `warn`→stderr; missing file → exit 2; `-I`
resolves a custom module dir; `-M` prepends `use`. **`--cache`/`--fasl` (Tier 1/2)
deliberately NOT added yet — Phase 6.** **`$0` not yet wired** (resolves to "sbcl";
follow-up — no clean symbol hook found).

---

## Phase 2 — saved core (startup speed) ✅ DONE

**Deliver:** `pcl --make-core` → `save-lisp-and-die` of the loaded runtime to
`$PCL_CACHE_DIR/pcl.core`; `pcl` auto-uses it when present and newer than
`cl/pcl-runtime.lisp` (`find_core`), else falls back to `--load` the runtime.

**Files:** `pcl` (`make_core`, `find_core`). No runtime changes.

**Verify:** `time pcl -e 'print 1'` drops to ≈0.3 s with a core; deleting/ageing
the core transparently falls back.

**Done when:** repeated `pcl` invocations are fast and the core auto-refreshes
when the runtime changes.

**Shipped (this session):** `pcl --make-core` writes `$PCL_CACHE_DIR/pcl.core`
(~40 MB); `find_core` auto-uses it when present and `>=` the runtime's mtime, else
falls back to a muffled `(load RUNTIME)`. Measured `pcl -e 'print …'` at ~0.12 s
with a core. `--clear-cache` removes cached `.fasl`/`.lisp` but leaves `pcl.core`.

---

## Phase 3 — the module provider registry + `cl/modules/`

This is the keystone for everything module-related (and a prerequisite for
running test suites). See `docs/shipped-modules.md`.

**Deliver:**
- `mkdir cl/modules/`; move `cl/pcl-pack.lisp` → `cl/modules/pack.lisp` and
  `cl/pcl-test.lisp` → `cl/modules/test-more.lisp` (keep `(in-package :pcl)` +
  exports); point `p-load-extension` at `cl/modules/`.
- `*pcl-module-providers*` table + the step-1 (`:cl`) / step-3 (`:xs`) lookups in
  `p-use`; fold `*p-xs-only-modules*` in as `:xs` rows.

**Files:** `cl/pcl-runtime.lisp` (registry + `p-use` lookups + extension path),
file moves. Update `docs/extensions.md` pointer.

**Verify:** full sweep + `prove -j8 Pl/t/` green (pack-using and Test-using files
must be unaffected by the relocation); `use Socket` gives a clean
"not available" error, not a crash.

**Done when:** `use Foo` resolves through the single registry, and adding a
CL-backed module is one file + one row.

---

## Phase 4 — run your own Perl tests

With Phase 3's registry, this is mostly a data change.

**Deliver:**
- Registry rows `("Test::More" :cl "test-more")`, `("Test::Simple" …)`,
  `("Test2::Bundle::More" …)` so `use Test::More` auto-loads
  `cl/modules/test-more.lisp` under plain `pcl` (no sweep needed).
- **Dev-vs-user separation:** the skip-registry (`cl/skip-registry.lisp`),
  `PCL_TEST_LOG_DIR` faillog, and `*current-test-file*` stay **sweep-only**. A
  user `pcl t/foo.t` gets plain TAP with the registry OFF (the default once
  you're not the sweep — confirm `test-more.lisp` is self-contained without the
  registry).
- Clean TAP on stdout (compile noise already goes to stderr; `# PCL Test library
  loaded` is a valid TAP comment) and a correct exit code.

**Files:** `cl/pcl-runtime.lisp` (registry rows). Possibly a tiny guard so the
test lib loads cleanly without the registry.

**Verify:** `pcl t/foo.t` for a small Test::More suite emits correct TAP and exit
status; compare a few against real `perl t/foo.t`.

**Done when:** a user can run a single `.t` file under `pcl` and get TAP.

---

## Phase 5 — `prove` integration + `pcl-prove`

**Deliver:**
- Document `prove --exec pcl t/*.t` — the standard harness, parallelism, and
  reporting come for free.
- A one-line convenience wrapper `pcl-prove` → `exec prove --exec "$ROOT/pcl"
  "$@"`.

**Files:** create `pcl-prove`; doc section.

**Verify:** `pcl-prove t/` on a small suite matches per-file `pcl t/foo.t`
results; `-j` parallelism works.

**Done when:** users run their whole suite with the familiar `prove` workflow.

---

## Phase 6 — caching tiers (`--cache`, `--fasl PATH`)

Now that the common path (Tier 0) is proven, add opt-in persistence.

**Deliver:**
- Tier 1 `--cache`: managed md5 FASL in `$PCL_CACHE_DIR` (key = md5(abs-path +
  mtime)); `PCL_CACHE=1` makes it the default.
- Tier 2 `--fasl PATH` (alias `-o PATH`): compile to an explicit file, reuse when
  newer than source.
- `--clear-cache` body: wipe `*.fasl`/`*.lisp` in `$PCL_CACHE_DIR`, keep
  `pcl.core`.
- Inline `-e`/`-E` never caches.

**Files:** `pcl` (`get_load_file` tiers, `compile_to_fasl`, `clear_cache`).
No runtime changes.

**Verify:** second `--cache` run skips transpile/compile; editing the script
invalidates by mtime; `--clear-cache` empties the cache but leaves the core.

**Done when:** opt-in caching works for the script's *own* source (caveat 2 still
open — Phase 7 closes it).

---

## Phase 7 — the `.deps` manifest (close caveat 2 for cached scripts)

Makes Tier 1/2 sound against compile-time interface drift in the `use`-closure.
See `docs/fasl-caching-design.md` §9.

**Deliver:**
- `pl2cl --emit-deps FILE` — dump the set of files `_extract_module_prototypes`
  opened (the `use`-closure; `require` files are naturally absent).
- On rebuild, `pcl` writes `<fasl>.deps` (`path<TAB>mtime` per closure file).
- On run, before loading a cached FASL, `pcl` stats each manifest entry;
  any missing/newer ⇒ rebuild. **Fail-closed** (rebuild, never run-stale).

**Files:** `Pl/Parser.pm` / `pl2cl` (`--emit-deps`: collect the paths already
walked); `pcl` (write + check the sidecar).

**Verify:** edit a `use`d module's prototype without touching the main script →
the cached main FASL rebuilds; `require`-only deps don't force a rebuild.

**Done when:** `--cache`/`--fasl` are sound against `use`-closure drift, scoped to
the main-script FASL (runtime-`require`'d modules still self-check by own mtime).

---

## Phase 8 (later, optional) — `pclbuild`

Redistributable artifacts, independent of the day-to-day `pcl` workflow.

- `pclbuild --fasl` — transpile + `compile-file` to a shippable FASL (largely
  subsumed by `pcl --fasl` for the common case).
- `pclbuild --bundle` — `x.pl` + its entire `use`-closure compiled into **one**
  FASL (topo-sort + `--module` concat + preload short-circuit). Kills caveat 2 by
  construction; the risk is BEGIN/load-order — **test diamonds and
  `use`-inside-`BEGIN`**. See `docs/fasl-caching-design.md` §8B.
- `pclbuild --exe` — standalone binary via `save-lisp-and-die` (needs
  `pl2cl --build-mode` wrapping exec statements in `pl-__pcl_main__`, and
  `*pcl-preloaded-eval-libs*` for `--eval-lib`). See command plan §pclbuild.

---

## Dependency graph of phases

```
P1 pcl runner ──┬─► P2 saved core
                ├─► P3 module registry ──► P4 run tests ──► P5 prove/pcl-prove
                └─► P6 caching tiers ─────► P7 .deps manifest
                                       P8 pclbuild (independent; reuses P3 preload)
```

P1 is the only hard prerequisite for everything. P3→P4→P5 is the "run real CPAN
test suites" track (highest mission value). P6→P7 is the "fast repeated runs,
soundly" track. P8 is shipping artifacts, deferrable.

## Suggested first commit

Phase 1 alone — the `pcl` runner with Tier-0 run + `-e`/`-E` + `-I`/`-M`/`-c` +
`@ARGV`, with `--clear-cache`/`--make-core` stubbed. It's self-contained, needs
no runtime or transpiler changes, and immediately makes PCL usable as a `perl`
drop-in for supported scripts.

## See also

- `docs/pcl-command-plan.md` — command interface + code sketches.
- `docs/fasl-caching-design.md` — caching model, caveats, the `.deps` manifest.
- `docs/shipped-modules.md` — `lib/` vs `cl/modules/`, the provider registry.
- `docs/extensions.md` — `p-load-extension`, the engine Phase 3 builds on.
