# PCL's caches

For someone running `pcl`, `pl2cl` or `runpcl` on their own Perl programs
and CPAN modules — not a PCL developer (that record is
[`test-infrastructure.md`](test-infrastructure.md)). What PCL keeps
between runs, where, what invalidates each thing, how to see or clear it.
**None of it can change what a program does — only how fast a run
starts.** The one-line answer to "why did PCL not notice my change?" is
`pcl --cache-info`.

PCL keeps five kinds of thing under `~/.pcl-cache/` (or `$PCL_CACHE_DIR`,
§6): a compiled image of PCL's own runtime (§1), each `use`d/`require`d
module's transpile and compiled form (§2), **the main script's own transpile
and compiled form (§2c)**, each distinct `eval "STRING"` text's transpile
(§3) — and, for now, nothing for the three checked-in extension files
pack/mro/warnings, which are recompiled every run (§4).

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
- **One core per runtime that still exists** (task #1863). Editing the runtime
  replaces that runtime's core; a *checkout you delete* used to leave its
  ~49 MB core behind for ever, because the core's name holds a **hash** of the
  runtime path and a hash cannot be turned back into a path. Each core now
  names its runtime in a `pcl-<path-hash>.path` sidecar beside it, and the
  next core *build* removes any core whose sidecar names a path that is gone.
  A core with no sidecar at all (one built before this) is stamped the first
  time it is used, and collected only if nothing has used it for a week. The
  core the current run needs is never removed, and the prune happens at build
  time — never on the path a warm start takes.
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

**What the key holds — and therefore what a change to it re-transpiles.**
The `<key>` above is a hash of the module's absolute path, the cache
*generation* string, and a **fingerprint of the compiler that will write the
entry** (task #1119): the PCL tree's own path plus every `Pl/**.pm`'s and
`pl2cl`'s mtime and size, *and* — since task #1843 — the toolchain those
files run on, namely the `perl` binary first on `$PATH` and every one of
PPI's own `.pm` files. The fasl adds a fourth component, the runtime
identity (`cl/pcl-runtime.lisp`'s content plus the SBCL), because a fasl has
this runtime's macro expansions baked in.

PPI and perl are in there because **a transpile's output is a function of
PPI's token stream**: PCL's tokenizer repairs are keyed on PPI 1.291's
stream, and a PPI point release that fixes one of the bugs in
`ppi-upstream-bugs.md` changes that stream for the shapes it touches. A
perl *major* upgrade moves the library directories, so module paths move and
the keys move with them; the hole was the **in-place** upgrade — a distro PPI
update, `cpanm PPI`, a same-version perl rebuild — where nothing else in the
key moves at all. It is mtime+size and not a version *string* for the same
reason: `$PPI::VERSION` does not move for a patched PPI and `$]` does not move
for a rebuilt perl. Cost, measured: ~95 extra `stat(2)`s, ~0.8 ms, paid once
per process that loads a module and invisible in a `pcl` run's wall time
(`pcl -e 'use JSON::PP; 1'` reads 0.360 s with and without it). The
transpiler's own prototype memo (§`proto/`) keys on the same two inputs.

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

**A dependency that MOVES counts as a change too** (task #1860). Editing a
dependency is only half of it: the same `use`d *name* can start resolving to
a **different file**, and that changes the parse the same way while the
recorded file still exists and still hashes as read. Two spellings — you
create `d1/B.pm` on a `-I` directory that was already on the list, shadowing
the `d2/B.pm` the transpile read; or you change the `-I` list itself, which a
script's key notices (§2c) and a module's key cannot. Both re-transpile now.
The manifest says how each dependency was found — the directories PCL's
transpiler probed before the hit, and whether the hit was in a `use lib`
directory or PCL's shim `lib/` rather than on the child perl's own search path
— and the next run re-checks exactly those facts. Nothing else changed: a
module you have not touched, whose dependencies have not moved, is still a hit
indefinitely, and the check costs one `stat` per probed directory.

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

The prune is never a startup cost: it is reached only on a cache **miss**,
runs once per process, and the scan itself happens at most once a day
across processes (a `.last-prune` stamp in the cache root claims the day;
`%p-claim-prune`) -- one directory walk of `modules/`, `evals/` and `proto/`
comparing mtimes to the cutoff, milliseconds.  A warm start never reaches
it (USER question s486, answered s488).

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

## 2c. The script cache — the program itself is an entry too

`pcl prog.pl` used to transpile the program on **every** run and hand SBCL
the text, which compiled every form again. Measured on a 1,211-line script:
6.57 s, every time. Since task #1841 the program is a cache entry like any
module, and the second run loads its compiled form in **0.037 s**.

- **Where**: `~/.pcl-cache/scripts/`, the same three files per entry as §2
  (`.lisp`, `.deps`, `<key>-<runtime-id>.fasl`) and the same 30-day
  last-use prune. `pcl --cache-info` counts them separately.
- **Validity** is §2's, unchanged: the entry must be newer than the script,
  and every module whose prototypes or exports its transpile read must still
  hash to what was read. Editing the script, **or editing only a module it
  uses**, re-transpiles it on the next run.
- **The key carries more than a module's**, because a program's emission
  does: besides the absolute path, the generation and the compiler
  fingerprint (§2), it carries **the path as you spelled it** (`$0` is that
  string verbatim, so `pcl ./p.pl` and `pcl p.pl` are two entries), **the
  `-I` list, the cwd and `PERL5LIB`**. The last three are not belt and
  braces: a different `-I` can resolve the same `use`d name to a *different*
  file, and which file that was changes the parse — measured, with two
  directories whose `B.pm` differ only in an empty prototype, perl answers
  8 then 107, and a key without the include path would answer 8 twice.
- **The main script is compiled to a fasl by default**, unlike a module
  under `-I`/`PERL5LIB`/`.`. The module rule exists because such a module is
  probably being *edited*; a main script is always under one of those
  directories, so applying it would mean paying the SBCL compile on every
  run and the cache would buy nothing. The **off** switches still reach it:
  `PCL_NO_FASL_CACHE=1` or a `PCL_NO_COMPILE_DIRS` match leaves the entry as
  readable text, and `--no-cache` / `PCL_NO_CACHE` skips the whole thing.
- **Not cached**: `pcl -e CODE` and a file run with `-M` prefixes. `pcl`
  writes both to a temp file with a fresh random name, so a path-keyed entry
  would leak one dead entry per run; content-keying them the way §3 keys an
  eval is task **#1862**. `pcl -c` is not cached either — it must transpile
  and *not* run.
- **A dependency that MOVES is covered too, since task #1860** — a name that
  starts resolving to a *different file* while the search path is unchanged
  (you create `d1/B4.pm` on an `-I` directory that was already there,
  shadowing the `d2/B4.pm` the transpile read). Nothing in the key moves and
  the recorded dependency still hashes as read, so this was the one shape a
  cached script answered with yesterday's parse — and it was **new with this
  cache**, because before it the script was re-transpiled every run. §2's
  paragraph has the mechanism.
- **A script edited in the same second its entry was written re-transpiles
  once more**: validity wants the entry *strictly* newer than the source.
  That is §2's rule, and it errs towards doing the work again.
- `runpcl`, the perl-tests sweep and the companion suite do **not** use it —
  they measure the transpile, and none of them goes through `pcl`.

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
lexicals, and the feature set in force, plus the compiler generation and
(since task #1843) the same **compiler fingerprint** a module's key carries,
so an eval entry is no more shareable between two PCL trees, or across a PPI
upgrade, than a module's is
(`%p-eval-cache-stem`, `cl/pcl-runtime.lisp` "THE STRING-EVAL DISK CACHE").
Validity uses the same `.deps` manifest as §2: an eval that `use`s a
module is re-transpiled when that module's content changes. A **failing**
eval (a syntax error in the string) writes no entry — perl re-tries a
failing eval every time, so PCL does too. This directory has no natural
size ceiling (an eval per loop iteration is ordinary Perl; three of
perl's own test files alone leave over 600 entries), so it's the one the
30-day last-use prune matters most for.

`pcl --no-cache` / `PCL_NO_CACHE=1` disables this together with the module
and script caches — one switch for all three, not three. `pcl --cache-info`
lists `evals/` beside the others (it did not until task #1335 closed in
s488b, which is why older notes warn not to read its absence there as "the
eval cache isn't working"), and `pcl --clear-cache` removes it — that glob
had never named the directory either.

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
| `pcl hello.pl` (1 line) | 0.181 s | **0.033 s** | s488b, 2026-09-17 (script cache, §2c; the cold-entry run costs 0.203 s) |
| `pcl cl/pack-impl.pl` (1,211 lines) | 6.571 s | **0.037 s** | s488b, 2026-09-17 (cold entry 7.256 s, i.e. one run's worth; `perl` itself 0.006 s) |
| the compiler fingerprint, per process | 1.15 ms | 1.95 ms | s488b, 2026-09-17 (#1843's ~95 extra stats; invisible in a run's wall time) |

These stack: pre-#1188 `use JSON::PP` cold was 13.4 s; fasl caching (§2)
alone warmed it to 1.43 s; the string-eval disk cache (§3) on top — since
JSON::PP's own loader `eval`s — warms it further to 0.41 s.

## 6. The knobs

### Environment variables

| variable | meaning | default |
|---|---|---|
| `PCL_CACHE_DIR` | root of every per-user cache: `modules/`, `scripts/`, `evals/`, `proto/`, `core/`, `xs/` | `~/.pcl-cache` |
| `PCL_COMPILE_DIRS` | colon-separated directories (`PERL5LIB` syntax); a module under one is compiled to a fasl. `*` = every directory. The **main script** is exempt from this list (§2c) | perl's installed library directories + PCL's own `lib/` |
| `PCL_NO_COMPILE_DIRS` | same syntax; a module — or the main script — under one is **never** compiled to native code (still gets the `.lisp`/manifest cache); wins over `PCL_COMPILE_DIRS` on any match. `*` = compile nothing | empty |
| `PCL_NO_FASL_CACHE=1` | kept alias of `PCL_NO_COMPILE_DIRS='*'` | |
| `PCL_NO_CACHE` / `pcl --no-cache` | this run reads and writes no module cache, **no script cache and no eval cache** — one switch | off |
| `PCL_OPT`, `PCL_NO_RAW_VERDICT`, `PCL_FACTS`, `PCL_IR_PLAIN` | not cache knobs, but they **select an emission**, so since task #1861 they are part of the compiler fingerprint: a run under one setting never reads an entry written under another. `pcl --cache-info` prints the ones in force | unset |
| `PCL_NO_CORE=1` | never build or use a saved core; always load the runtime from source | off |
| `PCL_CORE=path` | use this specific saved core | — |
| `PCL_FASL_DEBUG=1` | per-module trace: FASL HIT / fasl-build / TEXT, and why | off |
| `PCL_SHOW_SBCL=1` | print the exact `sbcl` command line a run spawns (which core, which flags) | off |
| `PCL_OPT` | switch named speed optimizations off (`PCL_OPT=none` = fully generic emission); unrelated to caching but sits beside these knobs in `pcl --help` | all on |

### Flags

| flag | what it does |
|---|---|
| `pcl --cache-info` | where the cache is, size/count per kind, which core this run would use and why, the compile policy in effect, and the compiler fingerprint with the perl binary, the PPI sources and the emission-selecting environment it hashed. **The** diagnostic for "PCL did not notice my change" |
| `pcl --clear-cache` | removes everything under `PCL_CACHE_DIR` that PCL made: modules, scripts and evals (`.lisp`/`.fasl`/`.deps`/`.failed`), prototype facts, saved cores. One flag, no sub-selection — a core rebuilds in ~20 s. **Not** the XS artifacts (`tools/pcl-xs-install --clean` for those) |
| `pcl --no-cache` | this run only: skip the module, script and eval caches entirely |
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
  using the same `$PCL_CACHE_DIR` — but only between trees that hash to the
  same **compiler fingerprint** (§2's key: the PCL tree's path and its files,
  plus perl and PPI). Two checkouts, or two branches mid-development, get
  *different* keys and cannot read each other's entries (task #1119, closed;
  before it, they could, and a gate row died calling a function that existed
  only in the sibling tree). A checkout and an install of the same source
  are two trees by path, so they do not share either. `pcl --cache-info`
  prints the fingerprint this run computed.
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
