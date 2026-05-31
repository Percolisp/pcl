# FASL Caching in PCL — How It Works, Why It's (Mostly) Safe, and the v1 Plan

This document explains PCL's compiled-code caching: the mechanism, the two real
soundness caveats, why one of them is the same hazard that sank Perl's own
bytecode compiler, and the simple first-version fix we settled on.

It is written to be read top-to-bottom — each section sets up the next. If you
only want the conclusion, jump to [§9 The chosen v1](#9-the-chosen-v1--a-dependency-manifest-checked-up-front).

---

## 1. The three compiled artifacts

PCL can persist compiled Common Lisp in three forms. Keep them distinct — they
have different lifetimes and different freshness rules.

| Artifact | What it is | Keyed/checked on | Who manages it |
|---|---|---|---|
| **Module cache** | A `.fasl` per `use`/`require`d Perl module | abs-path + mtime | the **runtime** (`p-load-module-cached`, `cl/pcl-runtime.lisp`) |
| **Main-script FASL** | A `.fasl` for the script you run (opt-in) | abs-path + mtime | the **`pcl` driver** (`--cache` / `--fasl`) |
| **Saved core** | The compiled PCL runtime, snapshotted | rebuilt on demand | `pcl --make-core` |

The saved core is just a startup optimisation (replace ~0.5 s of runtime
compilation with a ~0.05 s `mmap`) and is orthogonal to the correctness story
below. The rest of this document is about the two FASL caches.

---

## 2. The core mechanism: `use`/`require` become *runtime calls*

This is the single most important fact, and everything else follows from it.

When PCL transpiles `a.pl`, a statement like `use B;` does **not** copy B's code
into `a`'s output. It emits a *call*:

```perl
use B;            →   (p-use "B")            ; runtime call
require "c.pl";   →   (p-require-file "c.pl") ; runtime call
```

Those calls execute **when `a`'s FASL is loaded**, not when it was compiled. At
that point each call reaches `p-load-module-cached` (`cl/pcl-runtime.lisp:7132`),
which checks *that dependency's* freshness independently.

Freshness is decided by `p-cache-valid-p` (`~7043`): a cached `.fasl` is valid
only if

1. it exists, **and**
2. `cache-mtime > source-mtime`, **and**
3. `cache-age < 7 days` (`*pcl-cache-max-age*`).

The cache **key** is `sxhash` of the file's absolute path (`p-compute-cache-path`,
`~7033`) — *path-only, not content*. `%INC` (`*p-inc-table*`) dedups within a
single process so a module isn't loaded twice.

So the cache is **not** a "compile `a.pl` once and freeze the world" model. It is
a **lazy, per-file, recursive freshness check** performed at load time.

---

## 3. Walking the dependency scenarios

Let `a.pl` both `use B` and `require "c.pl"`, and let `b.pl` itself `use D`.

| You change… | What happens | Picked up? |
|---|---|---|
| `b.pl` | its mtime bumps past `b.fasl` → `p-cache-valid-p` nil → B re-transpiled + recompiled | ✅ yes |
| `c.pl` | same, via the runtime `(p-require-file "c.pl")` call | ✅ yes |
| a dependency *of* `b.pl` | loading the new `b.fasl` runs B's own `(p-use …)` calls, each re-checked — the graph is walked transitively | ✅ yes |
| `b.pl` to add `use D` | B's mtime changed → B recompiled → the **new** `b.fasl` contains the new `(p-use "D")` → D loads | ✅ yes |

The takeaway: **ordinary edits are picked up, recursively, including
newly-added `use` statements**, because the changed file's mtime forces *its*
recompilation and its fresh FASL carries its fresh dependency calls.

So where's the problem?

---

## 4. Caveat 1 — it's an mtime+path cache, not content

Validity is `cache-mtime > source-mtime` and the key is the path. That inherits
the classic `make`-style staleness holes:

- a source edited without bumping mtime (restored from backup, `touch -t`,
  two edits within one filesystem-second on a coarse FS) is missed;
- the key never reflects content, only location.

Backstop: the 7-day `*pcl-cache-max-age*` forces a rebuild eventually even if a
file is never touched. This caveat is mild and well-understood; the interesting
one is next.

---

## 5. Caveat 2 — compile-time interface drift (the real one)

A FASL is compiled against its dependencies' **compile-time interface at build
time**. If a dependency's compile-time interface changes but the *dependent's*
own source mtime does not, the dependent's cached FASL can keep a **stale value
baked in** — and nothing in the per-file mtime check notices, because the
dependent's own source didn't change.

### What "compile-time interface" means in Perl

`use B;` is, by definition, `BEGIN { require B; B->import; }` — it runs at the
**compile time** of the using file. So while `a.pl` is being compiled, B is
already loaded and its compile-time effects are visible to the rest of `a`:

- **`use constant`** values can be folded inline;
- **prototypes** change how subsequent calls *parse* (`&` blocks, `(\@)` ref
  capture, etc.);
- import-driven syntax can alter parsing.

`require` is different: it is a **pure runtime** load. When `a.pl` is compiled,
nothing about the `require`d module is known, so nothing of it can be baked into
`a`'s FASL. **`require` dependencies are caveat-2-safe by construction.** This
distinction is the lever the whole fix turns on (see §8).

### Why this is the bytecode-compiler hazard

This is exactly why compiling Perl to a frozen bytecode/artifact was never made
sound (the B::Bytecode line of work): a Perl program's meaning depends on
compile-time effects of the modules it pulls in, so a snapshot taken at one
moment can silently disagree with the sources later. PCL's long-standing
sensitivity to `BEGIN`/load ordering is the same issue wearing a different hat.

### What PCL *actually* bakes across a `use` — and what it doesn't

The blast radius in PCL is **narrow**, and the code tells us exactly how narrow.
When the transpiler processes `use B`, `_process_include_statement`
(`Pl/Parser.pm:5234`) calls `_extract_module_prototypes($module)`
(`~5453`). That helper spins up a sub-parser with `collect_prototypes_only => 1`
and returns **only the module's prototypes**; `_merge_module_prototypes` (`~5502`)
merges only those (the ones that affect codegen — block args, ref params).

Consequently:

- **Prototype drift** across a `use` **is** a real caveat-2 case in PCL: change a
  sub's prototype in B, and A — compiled against the old prototype — keeps
  parsing its calls the old way.
- Cross-module **`use constant` *values* are not inlined** into the dependent;
  they resolve through the symbol table at load time.

So in PCL today, the entire caveat-2 surface is essentially **prototype drift
across a `use` edge** — rare in practice, and exactly what the no-FASL default
(see §10) neutralises.

---

## 6. The governing principle: a freshness cache must fail *closed*

Before discussing fixes, state the invariant they must satisfy. A correctness
cache has exactly three outcomes when asked for an artifact:

1. **Found, proven current** → use it.
2. **Found but not provably current, or not found** → rebuild it.
3. **Not current and cannot rebuild** → **abort.**

The forbidden outcome is "found something, can't prove it's current, run it
anyway" — *fail-open*. That is precisely caveat 2.

PCL's per-file check is already fail-closed **with respect to a file's own
source** (stale own-mtime ⇒ rebuild). Its blind spot is only that "current" is
judged on the file's *own* mtime and ignores its dependencies.

For the normal `pcl` flow the transpiler is always present, so outcome 3 (abort)
essentially never occurs — a mismatch just rebuilds. Abort matters only for a
**standalone `--exe`**, which has no transpiler at runtime: there, freshness
can't be deferred to runtime at all, so the *whole* `use`-closure must be
compiled consistently at **build** time, and the build must refuse to emit a
binary it can't assemble consistently.

---

## 7. Why the naïve "fold dependency mtimes into the key" needs care

The obvious fix — make `a`'s cache key depend on its dependencies' mtimes — works
only if you respect a chicken-and-egg problem:

> To compute `a`'s key you need `a`'s dependency set, but you only *learn* `a`'s
> dependencies by parsing `a` — the work you were trying to skip.

So validation can't be done on `a` in isolation; it must be **bottom-up**:
validate/rebuild dependencies first, then `a` is valid iff (its own source
matches) **and** (the dependency fingerprint recorded in `a`'s artifact still
equals the freshly-computed one). Any rebuilt dependency changes its fingerprint
and so invalidates `a`. With the fingerprint in hand, the fail-closed rule from
§6 applies cleanly.

mtime is itself an unreliable oracle (§4), so if you are recording a fingerprint
anyway, a **content hash of the file plus its resolved dependency set** is the
stronger choice — it stops trusting the clock.

---

## 8. Three designs considered

### (A) Per-file dependency-aware key (incremental)

Record each file's transitive `use`-closure fingerprint; rebuild a file when any
file in its closure changes. **Pro:** incremental — only changed pieces
recompile; module cache shared across scripts. **Con:** needs the
fingerprint/fail-closed plumbing and many small loads.

### (B) Whole-closure bundle

Compile `x.pl` **and its entire recursive `use`-closure together, in one pass,
into one FASL**. **Pro:** kills caveat 2 *by construction* — the unit of caching
equals the unit of consistency; one manifest, one artifact, trivially
fail-closed; fast single load; shippable. **Con:** any closure file changing
rebuilds the whole bundle; you must get **topological order** and **BEGIN/load
timing** right (the historical hazard). This is the non-executable sibling of
`pclbuild --exe`, and it reuses machinery that already exists or is planned:
closure discovery (`_extract_module_prototypes`, recursive + memoized),
`pl2cl --module` mode, single `compile-file` (as in `pclbuild --fasl`), and the
`*pcl-preloaded-eval-libs*` short-circuit so a bundled module's `(p-use …)` skips
re-loading. Deferred — we want v1 simple.

### (C) Dependency manifest, checked up front by the driver — **chosen for v1**

Dump the list of `use`d files in the dependency graph, with their modification
dates, into a sidecar; the `pcl` driver checks them before running. See §9.

### (D) Do nothing clever — rely on the no-FASL default + `--clear-cache`

The baseline. Since the default re-transpiles every run (§10), caveat 2 cannot
occur there at all; the opt-in caches stay vulnerable to the narrow prototype
case, and `--clear-cache` is the manual escape hatch. This is the *floor* we
build on, not really a competing design — (C) is (D) plus automatic detection.

### At a glance

| Option | Soundness vs caveat 2 | Incremental? | New machinery | Risk | Verdict |
|---|---|---|---|---|---|
| **(D)** default + `--clear-cache` | manual only | n/a | none | none | floor / always available |
| **(C)** manifest pre-flight | automatic, main-script scope | yes | `pl2cl --emit-deps` + ~20 line driver check | low | **v1** |
| **(A)** per-file dep-aware key | automatic, every cached file | yes (best) | fingerprint plumbing in runtime + emit deps | medium | later, if `--cache` gets heavy use |
| **(B)** whole-closure bundle | automatic, by construction | no (whole bundle rebuilds) | topo-sort + concat + preload short-circuit | medium (BEGIN/load order) | a `pclbuild --bundle` artifact, later |

The progression is deliberate: **(D)** is free and already true; **(C)** adds
automatic detection for the opt-in caches with almost no machinery; **(A)** and
**(B)** are heavier and only pay off once persistent caching is a common
workflow. Ship (C) on top of (D); revisit (A)/(B) when the need is demonstrated.

---

## 9. The chosen v1 — a dependency manifest checked up front

The simplest *sound* option, and the one we'll ship first. It lives **entirely
in the `pcl` driver as a pre-flight check, with zero CL runtime changes.**

### Mechanism

Two sidecars next to each cached main-script FASL — the `.fasl` and a `.deps`
manifest.

**On rebuild** (cache miss, or any check below fails):
1. transpile `x.pl` → compile → `x.fasl`;
2. write `x.deps`: one line per file in `x`'s `use`-closure, `path<TAB>mtime`.

**On run** (`pcl --cache x.pl`, or `pcl --fasl PATH x.pl`):
1. if `x.fasl` is missing or older than `x.pl` → rebuild;
2. else read `x.deps` and `stat` each listed file — if any is **missing or its
   mtime ≠ the recorded mtime** → rebuild;
3. else load `x.fasl` directly.

That is ~20 lines of Perl in the driver, and it is **fail-closed**: a mismatch
means rebuild (the transpiler is always present here), never "run the FASL
anyway."

### Where the file list comes from — it already exists

`pl2cl` already opens every module in the `use`-closure during transpilation
(`_extract_module_prototypes` → `_find_module_file`, recursive + memoized). The
manifest is just *"the set of files pl2cl read."* The only new transpiler work is
to **record and emit** those paths — e.g. `pl2cl --emit-deps FILE`. No analysis
pass; you are dumping a set you already computed.

And the elegant payoff of the §5 `require`/`use` distinction: **`require`d files
are automatically absent from the manifest**, because the transpiler never opens
them (`require` → a runtime call, no extraction). So the manifest is *exactly*
the compile-time closure, with nothing to filter.

### Honest scope limit

This pre-flight covers the **main-script FASL** (`--cache` / `--fasl`) — where
caveat 2 actually bites. It does **not** reach modules loaded by runtime
`require` from inside the running program; those still self-check by their own
mtime via `p-load-module-cached`, so a runtime-`require`d module that bakes in
*its* dependency's prototype is a deeper layer left for later. For v1 that is
fine: the default is no-FASL (always fresh), the opt-in cache is now sound
against its whole `use`-graph, and `--clear-cache` is the blunt fix for the
residual narrow case.

---

## 10. The resulting tiered model

The user-facing behaviour stays simple — a ladder of persistence, default at the
bottom:

| Invocation | Behaviour | Freshness |
|---|---|---|
| `pcl x.pl` *(default)* | transpile → temp `.lisp` → run → delete | always fresh (re-transpiled every run) |
| `pcl --cache x.pl` | managed md5 FASL + `.deps` manifest | own mtime **and** `use`-closure manifest |
| `pcl --fasl PATH x.pl` | explicit FASL + `.deps` manifest | same, at a path you control |
| `pcl --clear-cache` | wipe cached `.fasl`/`.lisp`; keep `pcl.core` | — |

Because the **default re-transpiles every run**, it always sees a dependency's
current compile-time interface — caveat 2 simply cannot occur there. The manifest
exists to make the *opt-in* caches equally sound without paying that cost on
every run.

---

## 11. Summary

- `use`/`require` compile to **runtime calls**; the cache is a **lazy, per-file,
  recursive mtime check**, not a frozen snapshot. Ordinary edits — including
  newly-added `use`s — are picked up.
- **Caveat 1:** mtime+path key has the usual `make`-style holes (7-day backstop).
- **Caveat 2:** a FASL can bake in a dependency's *compile-time* interface; in
  PCL the surface is **prototype drift across a `use`** (via
  `_extract_module_prototypes`), not cross-module constant values. This is the
  bytecode-compiler hazard and ties into PCL's `BEGIN`/load-order history.
- **`require` is safe by construction** — nothing of it is baked in.
- A freshness cache must **fail closed**: rebuild on any unproven freshness;
  abort only where rebuild is impossible (`--exe`, decided at build time).
- **v1 (chosen):** a `.deps` manifest of the `use`-closure + mtimes, emitted by
  `pl2cl` and checked by the `pcl` driver before running — ~20 lines, no runtime
  changes, fail-closed, scoped to the main-script FASL.

## See also

- `docs/pcl-command-plan.md` — the `pcl`/`pclbuild` command design (tiers,
  `--cache`, `--fasl`, `--clear-cache`, `--make-core`, open-question #6).
- `docs/declaration-ordering.md` — Perl compile/load phases, `eval-when`,
  the `BEGIN`-visibility machinery that the bundle option (§8B) would lean on.
- `cl/pcl-runtime.lisp` — `p-compute-cache-path`, `p-cache-valid-p`,
  `p-load-module-cached`, `p-use`, `p-require-file`.
- `Pl/Parser.pm` — `_process_include_statement`, `_extract_module_prototypes`,
  `_merge_module_prototypes`.
