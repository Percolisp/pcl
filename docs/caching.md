# PCL's caches

For someone running `pcl`, `pl2cl` or `runpcl` on their own Perl programs
and CPAN modules — not a PCL developer (that record is
[`test-infrastructure.md`](test-infrastructure.md)). What PCL keeps
between runs, where, what invalidates each thing, how to see or clear it.
**None of it can change what a program does — only how fast a run
starts.** The one-line answer to "why did PCL not notice my change?" is
`pcl --cache-info`.

PCL keeps four kinds of thing under `~/.pcl-cache/` (or `$PCL_CACHE_DIR`,
§6): a compiled image of PCL's own runtime (§1), each `use`d/`require`d
module's transpile and compiled form (§2), each distinct `eval "STRING"`
text's transpile (§3) — and, for now, nothing for the three checked-in
extension files pack/mro/warnings, which are recompiled every run (§4).

## 1. The saved runtime core

Every `pcl`/`pl2cl`/`runpcl` invocation needs PCL's ~26,000-line runtime
(`cl/pcl-runtime.lisp`) loaded into SBCL. Loading it from source costs
about a second; PCL instead loads a pre-built SBCL image (a "core") with
the runtime already compiled in, dropping that to about 0.1 s.

- **Location and key**: `~/.pcl-cache/core/pcl-<path-hash>-<content-hash>.core`,
  named from the runtime's absolute path, its own source, `sbcl --version`,
  the contents of the `cl/vendor/` tree beside it (§1a), `~/.sbclrc`'s
  size+mtime and a format version (`tools/lib/PCLSbcl.pm`; see
  `test-infrastructure.md` "The saved-core optimisation"). Editing the
  runtime, replacing the vendored library, upgrading SBCL, or using a
  different checkout each produce a *different* name — there is no
  stale-core case, only a miss that rebuilds.
- **Build**: on first use, under an `flock` so concurrent spawns don't race;
  written temp-file-then-rename. A failed build leaves `<core>.failed` for
  one hour and falls back to loading from source, loudly.
- `PCL_NO_CORE=1` always runs from source; `PCL_CORE=path` uses a named
  core; `pcl --make-core` builds the cache entry now and exits.
- **A checkout's cached core and an *installed* core are different
  things.** `tools/install-pcl` compiles the runtime once, at install
  time, into `<prefix>/lib/pcl/pcl.core` — an installed `pcl` never pays a
  core-build wait on first use (`tools/install-pcl --help`, task #1302). A
  plain checkout builds its own core under `~/.pcl-cache/core/` instead
  (§7 has what an install shares vs. keeps per-user).
- `PCL_SHOW_SBCL=1 pcl -e 1` prints the exact `sbcl --core …` command, so
  you can see which core a run actually used.

### 1a. Where cl-ppcre comes from

The runtime's one external Lisp dependency is
[cl-ppcre](https://edicl.github.io/cl-ppcre/), the regex engine `m//`,
`s///` and `split` run on. It is **vendored**: the sources live in
`cl/vendor/cl-ppcre/`, carried verbatim from upstream, and
`cl/pcl-runtime.lisp` pushes that directory onto `asdf:*central-registry*`
before loading the system. A machine therefore needs SBCL and nothing
else — no Quicklisp, no `~/.sbclrc`, no distribution Lisp package.
`cl/vendor/README.md` records the version, the upstream commit and the
rule that the directory is never edited here.

- **Fallback**: if `cl/vendor/cl-ppcre/` is missing (a repackaged tree, or
  a deliberate test against another version), ASDF's ordinary search runs
  instead — a distro package, Quicklisp, whatever a `~/.sbclrc` set up.
- **When neither answers**, the load fails with a message naming which of
  the two was tried, never a silent fall-back.
- Because the vendored sources are compiled *into* the saved core, they are
  part of the core's key (above): replacing them rebuilds the core.
- `sbcl … --load cl/pcl-runtime.lisp --eval '(print (asdf:system-source-directory :cl-ppcre))'`
  says which copy an image actually loaded; the gate row
  `Pl/t/vendored-ppcre-01.t` asserts it is the vendored one under an empty
  `$HOME` with ASDF's inherited configuration ignored.

## 2. The module cache

Every module a program `use`s or `require`s is transpiled once and, when
it qualifies (below), compiled once — not on every run. On disk, under
`~/.pcl-cache/modules/`, three files per cached module (`docs/ir-spec.md`
§9.2b, normative):

| file | contents |
|---|---|
| `<key>.lisp` | the module transpiled to Common Lisp |
| `<key>.deps` | the **dependency manifest**: which other modules this transpile read facts from, and their content hashes |
| `<key>-<runtime-id>.fasl` | that text compiled to native code, for one runtime build and one SBCL |

**Validity is a content manifest — editing a dependency DOES re-transpile
the dependent.** A module's own source changing was always caught by its
mtime; since task #1261, a module's *parse* also depends on facts about
the modules it `use`s (whether a sub has a `(&@)` prototype, what a module
exports, …), so editing **only** the dependency now invalidates every
module whose cached transpile read from it. The check is the `.deps`
sidecar: every dependency line is re-hashed (MD5 of content, never an
mtime — a `git checkout` restoring an old mtime must not look valid), and
a mismatch, or a missing/unparsable manifest, makes the entry invalid
(`docs/ir-spec.md` §9.2b; `docs/DECIDED.md` §s470bw). Confirmed by hand
(the shape `Pl/t/module-fasl-cache-01.t` tests): with a module `A3.pm`
that `use`s `B2.pm` and calls one of B2's subs bareword-style, editing
*only* `B2.pm` to drop that sub's empty prototype — never touching
`A3.pm` — changes `A3`'s next-run answer to match perl's new parse,
without `A3.pm`'s own mtime having moved.

**The compile policy — which entries also get a `.fasl` — is two directory
lists**, read at run time (`PCL_COMPILE_DIRS` / `PCL_NO_COMPILE_DIRS`,
§6). By default it's perl's own installed library directories plus PCL's
`lib/`: a module you're actively editing (under `-I`/`PERL5LIB`/`.`) is
cached as readable text, never silently as an opaque fasl. A module that
doesn't qualify for a fasl still gets the `.lisp` cache and the manifest
check.

**The cache directory** is `$PCL_CACHE_DIR` (default `~/.pcl-cache`),
resolved once per **process**, never baked into a saved core (task #1303
— before this fix, a root-built shared core would have sent every user's
modules into root's home). One resolver on each side: `PCLPaths::cache_root`
(Perl), `%p-default-cache-dir` (runtime).

**Pruning** is by last *use*, not age: a hit re-stamps its entry (at most
once a day), and anything untouched for 30 days is deleted, in `modules/`
and `proto/` alike (`*pcl-cache-max-age*`; `docs/DECIDED.md` §s470by).
There is no age limit on *validity* — an untouched, unedited entry is a
hit indefinitely.

**The directory is created `0700`, and an unsafe one is refused** — a
cached module is compiled code that will be loaded and run. PCL refuses a
cache root owned by another user, or writable by group/other, with a
normal `eval`-trappable die naming the fix:

```
PCL: refusing to use the cache directory DIR: it is group- or
world-writable (mode 0775). A cached module is compiled code, so PCL keeps
its cache private. Fix it with: chmod 700 DIR -- or set PCL_CACHE_DIR to a
directory you own
```

(A directory PCL creates itself is always `0700`; this fires only on one
made by hand with `mkdir`, which inherits the shell's umask.)

## 3. String eval has its own disk cache

`eval "PERL CODE"` used to transpile through a `pl2cl --server` subprocess
**on every run** — cached only for the life of one process. That's costly
because generating accessors with `eval` at load time is a standard
pure-Perl idiom (JSON::PP, Moo/Sub::Quote, Class::Accessor, Moose,
Type::Tiny — one measured case runs 80 such evals), so a program paid the
same 80 transpiles every single time it started.

Since task #1200, each distinct eval text's transpile is kept under
`~/.pcl-cache/evals/`, one `.lisp` + one `.deps` sidecar per entry, beside
`modules/`. The key is exactly what decides the emission and nothing more
— the perl text, the caller's package, the names of any captured
lexicals, and the feature set in force, plus the compiler generation
(`%p-eval-cache-stem`, `cl/pcl-runtime.lisp` "THE STRING-EVAL DISK CACHE").
Validity uses the same `.deps` manifest as §2: an eval that `use`s a
module is re-transpiled when that module's content changes. A **failing**
eval (a syntax error in the string) writes no entry — perl re-tries a
failing eval every time, so PCL does too. This directory has no natural
size ceiling (an eval per loop iteration is ordinary Perl; three of
perl's own test files alone leave over 600 entries), so it's the one the
30-day last-use prune matters most for.

`pcl --no-cache` / `PCL_NO_CACHE=1` disables this together with the module
cache — one switch for both, not two. **As of this writing,
`pcl --cache-info` does not list `evals/` among the directories it
reports** — a gap, filed as task #1335, so don't read its absence there as
"the eval cache isn't working."

## 4. The three checked-in artifacts (pack, mro, warnings)

`cl/pcl-pack.lisp`, `cl/pcl-mro.lisp` and `cl/pcl-warnings.lisp` are
Perl-to-CL transpiles of `pack`, `mro` and `warnings` support, checked into
the tree so PCL doesn't need Perl itself to build them at run time. They
load into a running program via `p-load-extension`, on first use of
`pack`/`unpack`, `mro::...` or `warnings`.

**They are not cached at all today.** `p-load-extension` calls plain
`load` on the `.lisp` source (`cl/pcl-runtime.lisp`, `p-load-extension`),
so SBCL recompiles the file every run — measured, `pack("N",1)` costs
about 8.3 s for exactly this (`docs/DECIDED.md` §s470bp). It's the same
disease §2 and §3 solve, and the same fix would apply, but it's a known,
open, user-parked gap: task **#1202**.

## 5. Measured numbers (dated; each an A/B on one machine, one core)

Numbers below are best-of-several, interleaved runs; treat them as "this
lever is worth roughly this much," not as a portable benchmark.

| what | before | after | session |
|---|---|---|---|
| startup, warm core, no `use` | — | ~0.17 s | s470bn/s470by, 2026-09-05/06 |
| `use Carp` | 0.406 s | 0.216 s | s470bp, 2026-09-05 (module fasl caching, #1188) |
| `use Scalar::Util qw(blessed)` | 0.337 s | 0.170 s | s470bp, 2026-09-05 |
| `use Moo` | 1.127 s | 0.417 s | s470bp, 2026-09-05 |
| `use JSON::PP` | 2.751 s | 1.432 s | s470bp, 2026-09-05 (fasl caching alone) |
| `use JSON::PP` (9 deps, manifest check cost) | 1.118 s | 0.999 s | s470bw, 2026-09-06 (dependency-hash check adds no measurable cost) |
| `use JSON::PP; print 1` | 1.13 s | 0.41 s | s473p, 2026-09-07 (string-eval disk cache, #1200, layered on top of the above) |
| `eval "1"` | 0.304 s | 0.172 s | s473p, 2026-09-07 |
| `use Moo` (eval-cache leg) | 0.343 s | 0.201 s | s473p, 2026-09-07 |
| `use JSON::PP` cold (no fasl caching at all, pre-#1188 baseline) | 13.42 s | — | s470bn, 2026-09-05 |
| `pack("N",1)` | — | 8.3 s | s470bp, 2026-09-05 (extension load, §4, still uncached) |

These stack: pre-#1188 `use JSON::PP` cold was 13.4 s; fasl caching (§2)
alone warmed it to 1.43 s; the string-eval disk cache (§3) on top — since
JSON::PP's own loader `eval`s — warms it further to 0.41 s.

## 6. The knobs

### Environment variables

| variable | meaning | default |
|---|---|---|
| `PCL_CACHE_DIR` | root of every per-user cache: `modules/`, `evals/`, `proto/`, `core/`, `xs/` | `~/.pcl-cache` |
| `PCL_COMPILE_DIRS` | colon-separated directories (`PERL5LIB` syntax); a module under one is compiled to a fasl. `*` = every directory | perl's installed library directories + PCL's own `lib/` |
| `PCL_NO_COMPILE_DIRS` | same syntax; a module under one is **never** compiled to native code (still gets the `.lisp`/manifest cache); wins over `PCL_COMPILE_DIRS` on any match. `*` = compile nothing | empty |
| `PCL_NO_FASL_CACHE=1` | kept alias of `PCL_NO_COMPILE_DIRS='*'` | |
| `PCL_NO_CACHE` / `pcl --no-cache` | this run reads and writes no module cache **and** no eval cache — one switch | off |
| `PCL_NO_CORE=1` | never build or use a saved core; always load the runtime from source | off |
| `PCL_CORE=path` | use this specific saved core | — |
| `PCL_FASL_DEBUG=1` | per-module trace: FASL HIT / fasl-build / TEXT, and why | off |
| `PCL_SHOW_SBCL=1` | print the exact `sbcl` command line a run spawns (which core, which flags) | off |
| `PCL_OPT` | switch named speed optimizations off (`PCL_OPT=none` = fully generic emission); unrelated to caching but sits beside these knobs in `pcl --help` | all on |

### Flags

| flag | what it does |
|---|---|
| `pcl --cache-info` | where the cache is, size/count per kind, which core this run would use and why, and the compile policy in effect. **The** diagnostic for "PCL did not notice my change" |
| `pcl --clear-cache` | removes everything under `PCL_CACHE_DIR` that PCL made: modules (`.lisp`/`.fasl`/`.deps`/`.failed`), evals, prototype facts, saved cores. One flag, no sub-selection — a core rebuilds in ~20 s. **Not** the XS artifacts (`tools/pcl-xs-install --clean` for those) |
| `pcl --no-cache` | this run only: skip both the module and the eval cache entirely |
| `pcl --version` | PCL version, cache generation, SBCL version, PPI version |
| `pcl --make-core` | build the cached core now, then exit |

`pcl --help` has the live, authoritative text; this table mirrors
`docs/plan-cache-and-install-s471.md` §1.1, where the design was ruled.

## 7. Multi-user, worktrees, and CI

- **The cache is per `$HOME`** (or per `$PCL_CACHE_DIR`) — two users, or
  one user with two different `PCL_CACHE_DIR`s, share nothing.
- **A checkout or `git worktree` gets its own runtime core**: the core's
  name embeds the runtime file's absolute path, so two trees never collide
  there, even with byte-identical runtime source.
- **The module and eval caches ARE shared**, though, across every tree
  using the same `$PCL_CACHE_DIR` that produces the same compiler
  generation string — including two checkouts, or a checkout and an
  install. **Open caveat, task #1119 (unfixed as of this writing):** that
  generation string is a hand-maintained version marker, not a hash of the
  compiler's content, so two different PCL builds sharing one generation
  string (e.g. two branches mid-development) can read each other's cached
  entries. Until it closes, don't point two different PCL *versions* at
  one `$HOME`/`$PCL_CACHE_DIR` concurrently; `pcl --clear-cache` recovers.
- **A shared, system-wide install** (root installs to `/opt/pcl`, several
  users run it) works for the module/eval caches: each user's `pcl` writes
  its own cache under their own home, never under the install prefix
  (`tools/install-pcl --help`; tasks #1302/#1304). **One known limitation,
  task #1327 (found, not fixed):** a saved core remembers the ASDF cache
  directory of whoever *built* it, so any `asdf:load-system` work done from
  a shared install's core (recompiling the vendored cl-ppcre, say) can
  target a directory another user can't write to — ordinary Perl
  `use`/`require` is unaffected.
- `tools/install-pcl --uninstall` removes the installed tree and wrappers
  only; it never touches any user's `~/.pcl-cache` — `pcl --clear-cache`
  for that.

See also: [`README.md`](../README.md)'s "Caches" paragraph (the short
version), [`docs/ir-spec.md`](ir-spec.md) §9.2/§9.2b (normative on-disk
format), and [`docs/test-infrastructure.md`](test-infrastructure.md) (the
saved core's development history and how PCL's own tests use it).
