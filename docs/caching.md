# PCL's caches

This page is for someone running `pcl`, `pl2cl` or `runpcl` on their own
Perl programs and CPAN modules. It says what PCL keeps between runs, where,
what makes each entry stale, and how to see or clear it. (How PCL's own
test suite uses the caches is in
[`test-infrastructure.md`](test-infrastructure.md).)

**None of the caches can change what a program does, only how fast a run
starts.** The one-line answer to "why did PCL not notice my change?" is
`pcl --cache-info`; the one-flag answer to "is it the cache?" is
`pcl --no-cache`.

Everything lives under `~/.pcl-cache/` (or `$PCL_CACHE_DIR`, §6):

| what | where | rebuilt when | turned off by |
|---|---|---|---|
| PCL's runtime, compiled into a saved SBCL image (§1) | `core/` | the runtime, its vendored library, SBCL or the checkout path changes | `PCL_NO_CORE=1` |
| each module a program `use`s or `require`s: its transpile and compiled form (§2) | `modules/` | the module, a module it reads facts from, the compiler, perl or PPI changes | `--no-cache` |
| the main script itself, the same way (§2c) | `scripts/` | as for a module, plus the `-I` list, the current directory and `PERL5LIB` | `--no-cache` |
| each distinct `eval "STRING"` text's transpile (§3) | `evals/` | as for a module | `--no-cache` |
| the compiled `pack`, `mro` and `warnings` extensions (§4) | `ext/` | the extension file's bytes or the runtime change | `PCL_NO_FASL_CACHE=1` |

`proto/` holds the compiler's notes on each module's prototypes and
exports, and `xs/` holds XS artifacts, which are installed rather than
cached (§6).

## 1. The saved runtime core

Every `pcl`, `pl2cl` or `runpcl` run needs PCL's runtime
(`cl/pcl-runtime.lisp`) loaded into SBCL. Loading it from source costs
about a second; PCL instead loads a pre-built SBCL image (a "core") with the
runtime already compiled in, which takes about 0.1 seconds.

- **Location and name:** `~/.pcl-cache/core/pcl-<path-hash>-<content-hash>.core`.
  The name comes from the runtime's absolute path, its source,
  `sbcl --version`, the contents of the `cl/vendor/` tree beside it (§1a),
  the size and modification time of `~/.sbclrc`, and a format version
  (`tools/lib/PCLSbcl.pm`). Editing the runtime, replacing the vendored
  library, upgrading SBCL, or using a different checkout each produce a
  *different* name. There is no stale core, only a miss that rebuilds.
- **Build:** on first use, under a file lock so that concurrent runs do not
  race, and written to a temporary file that is then renamed into place. A
  failed build leaves `<core>.failed` for one hour and falls back to loading
  from source, with a message.
- **One core per runtime that still exists.** Editing the runtime replaces
  that runtime's core. Each core also names its runtime in a
  `pcl-<path-hash>.path` file beside it, and the next core *build* removes
  any core whose runtime path no longer exists (a deleted checkout). A core
  without that file is stamped the first time it is used, and removed only
  if nothing has used it for a week. The core the current run needs is
  never removed, and the pruning happens at build time, never on a warm
  start.
- `PCL_NO_CORE=1` always runs from source, `PCL_CORE=path` uses a named
  core, and `pcl --make-core` builds the core now and exits.
- **A checkout's cached core and an installed core are different things.**
  `tools/install-pcl` compiles the runtime once, at install time, into
  `<prefix>/lib/pcl/pcl.core`, so an installed `pcl` never waits for a core
  build on first use. A plain checkout builds its own core under
  `~/.pcl-cache/core/` instead (§7 says what an install shares and what
  stays per user).
- `PCL_SHOW_SBCL=1 pcl -e 1` prints the exact `sbcl --core …` command, so
  you can see which core a run used.

### 1a. Where cl-ppcre comes from

The runtime's one external Lisp dependency is
[cl-ppcre](https://edicl.github.io/cl-ppcre/), the regex engine that `m//`,
`s///` and `split` run on. It is **vendored**: the sources live in
`cl/vendor/cl-ppcre/`, carried verbatim from upstream, and
`cl/pcl-runtime.lisp` adds that directory to ASDF's search list
(`asdf:*central-registry*`) before loading it. A machine therefore needs
SBCL and nothing else: no Quicklisp, no `~/.sbclrc`, no distribution Lisp
package. `cl/vendor/README.md` records the version, the upstream commit and
the rule that the directory is never edited here.

- **Fallback:** if `cl/vendor/cl-ppcre/` is missing (a repackaged tree, or
  a deliberate test against another version), ASDF's ordinary search runs
  instead: a distribution package, Quicklisp, or whatever a `~/.sbclrc` set
  up.
- **When neither finds it,** the load fails with a message naming which of
  the two was tried. It never falls back silently.
- Because the vendored sources are compiled *into* the saved core, they are
  part of the core's name (above): replacing them rebuilds the core.
- `sbcl … --load cl/pcl-runtime.lisp --eval '(print (asdf:system-source-directory :cl-ppcre))'`
  says which copy an image loaded. The test `Pl/t/vendored-ppcre-01.t`
  checks that it is the vendored one, with an empty `$HOME` and ASDF's
  inherited configuration ignored.

## 2. The module cache

Every module a program `use`s or `require`s is transpiled once and, when it
qualifies (below), compiled once, not on every run. Under
`~/.pcl-cache/modules/` there are three files per cached module (the format
is specified in [`ir-spec.md`](ir-spec.md) §9.2b):

| file | contents |
|---|---|
| `<key>.lisp` | the module transpiled to Common Lisp |
| `<key>.deps` | the **dependency manifest**: which other modules this transpile read facts from, and their content hashes |
| `<key>-<runtime-id>.fasl` | that text compiled to native code, for one runtime build and one SBCL |

**The key** is a hash of the module's path, the compiler's version, and a
fingerprint of the compiler, the `perl` binary and PPI; so upgrading any
of them re-transpiles. [Appendix A](#appendix-a-what-the-cache-key-holds-and-why)
says exactly what goes in and why.

**Validity is a content manifest: editing a dependency re-transpiles the
module that depends on it.** A module's own source changing is caught by
its modification time. But a module's *parse* also depends on facts about
the modules it `use`s (whether a sub has a `(&@)` prototype, what a module
exports), so editing **only** the dependency invalidates every module whose
cached transpile read from it. The check is the `.deps` file: every
dependency listed there is re-hashed (an MD5 of its content, never a
modification time, because a `git checkout` restoring an old time must not
look valid), and a mismatch, or a missing or unreadable manifest, makes the
entry invalid. For example, with a module `A3.pm` that `use`s `B2.pm` and
calls one of B2's subs without parentheses, editing *only* `B2.pm` to drop
that sub's empty prototype changes `A3`'s next-run answer to match perl's
new parse, although `A3.pm` itself did not change
(`Pl/t/module-fasl-cache-01.t` tests this shape).

**A dependency that moves counts as a change too.** The same `use`d *name*
can start resolving to a **different file**, which changes the parse just
as an edit does, while the recorded file still exists and still hashes the
same. Two ways this happens: you create `d1/B.pm` on an `-I` directory that
was already on the list, shadowing the `d2/B.pm` the transpile read; or you
change the `-I` list itself, which a script's key notices (§2c) and a
module's key cannot. Both re-transpile. The manifest records how each
dependency was found (the directories the compiler searched before the hit,
and whether the hit was in a `use lib` directory or PCL's `lib/` rather
than on perl's own search path), and the next run re-checks exactly those
facts. A module you have not touched, whose dependencies have not moved,
stays valid indefinitely, and the check costs one `stat` per searched
directory.

**The compile policy (which entries also get a `.fasl`) is two directory
lists**, read at run time (`PCL_COMPILE_DIRS` and `PCL_NO_COMPILE_DIRS`,
§6). By default it is perl's own installed library directories plus PCL's
`lib/`. A module you are working on (under `-I`, `PERL5LIB` or `.`) is
cached as readable text, never silently as an opaque compiled file. A
module that does not qualify for a `.fasl` still gets the `.lisp` cache and
the manifest check.

**The cache directory** is `$PCL_CACHE_DIR` (default `~/.pcl-cache`),
looked up once per **process** and never built into a saved core, so a
shared core built by root does not send every user's modules into root's
home. There is one resolver on each side: `PCLPaths::cache_root` in Perl
and `%p-default-cache-dir` in the runtime.

**Pruning is by last use, not age.** A cache hit re-stamps its entry (at
most once a day), and anything untouched for 30 days is deleted, in
`modules/`, `scripts/`, `evals/`, `ext/` and `proto/` alike
(`*pcl-cache-max-age*`). There is no age limit on *validity*: an untouched,
unedited entry stays a hit indefinitely. The prune never costs startup
time. It runs only on a cache **miss**, at most once per process, and the
scan itself happens at most once a day across processes (a `.last-prune`
file in the cache directory claims the day). It is one walk of those five
directories comparing modification times, and takes milliseconds.
A warm start never reaches it.

**The directory is created with mode `0700`, and an unsafe one is
refused**, because a cached module is compiled code that will be loaded and
run. PCL refuses a cache directory owned by another user, or writable by
group or others, with a normal `die` that `eval` can catch, naming the fix:

```
PCL: refusing to use the cache directory DIR: it is group- or
world-writable (mode 0775). A cached module is compiled code, so PCL keeps
its cache private. Fix it with: chmod 700 DIR -- or set PCL_CACHE_DIR to a
directory you own
```

(A directory PCL creates itself is always `0700`; this fires only on one
made by hand with `mkdir`, which follows the shell's umask.)

## 2c. The script cache: the program itself is an entry too

The program you run is cached like a module. On a 1,211-line script, a run
that transpiles and compiles everything takes about 6.6 seconds; a run
from the cache entry takes 0.037 seconds (measured 2026-09-17).

- **Where:** `~/.pcl-cache/scripts/`, with the same three files per entry
  as §2 (`.lisp`, `.deps`, `<key>-<runtime-id>.fasl`) and the same 30-day
  last-use prune. `pcl --cache-info` counts them separately.
- **Validity** is the same as §2: the entry must be newer than the script,
  and every module whose prototypes or exports its transpile read must
  still hash to what was read. Editing the script, **or editing only a
  module it uses**, re-transpiles it on the next run. A moved dependency
  (§2) is caught the same way.
- **The key holds more than a module's key**, because a program's output
  depends on more. Besides the absolute path, the generation and the
  compiler fingerprint (Appendix A), it holds **the path as you typed it** (`$0` is
  that string verbatim, so `pcl ./p.pl` and `pcl p.pl` are two entries),
  **the `-I` list, the current directory and `PERL5LIB`**. The last three
  matter: a different `-I` can resolve the same `use`d name to a
  *different* file, and which file that is changes the parse. With two
  directories whose `B.pm` differ only in an empty prototype, perl answers
  8 and then 107, and a key without the search path would answer 8 twice.
- **The main script is compiled to a `.fasl` by default**, unlike a module
  under `-I`, `PERL5LIB` or `.`. The module rule exists because such a
  module is probably being edited; but a main script is always under one of
  those directories, so applying the rule would mean paying the SBCL
  compile on every run, and the cache would buy nothing. The off switches
  still apply: `PCL_NO_FASL_CACHE=1` or a `PCL_NO_COMPILE_DIRS` match leaves
  the entry as readable text, and `--no-cache` or `PCL_NO_CACHE` skips the
  cache entirely.
- **Not cached:** `pcl -e CODE` (written to a temporary file with a fresh
  random name, so an entry keyed on the path would leave one dead entry
  behind per run), and a file run with any source-changing switch on the
  command line (`-n -p -l -0 -g -a -F -i -s -x -E -M -m -w -C -T -t -c`):
  the entry's key does not carry the switches.  A script's OWN `#!`
  switches are part of its bytes, so it is cached.  `-I` alone does not
  stop the cache (the `-I` list is part of the key).  An uncached run
  compiles the program every time: measured 2026-10-04 (median of five,
  load 1.4), `pcl -e 'print "hi\n"'` took 181 ms, `pcl -l h.pl` 184 ms
  and `pcl -lane ... file` 191 ms, against 41 ms for the cached
  `pcl h.pl` (`h.pl` is the one-line `print "hi\n";`; Appendix B).
- **A script edited in the same second its entry was written is
  re-transpiled once more,** because validity requires the entry to be
  *strictly* newer than the source. That errs towards doing the work again.
- `runpcl`, and the runners for perl's test suites, do **not** use the
  script cache: they measure the transpile, and none of them goes through
  `pcl`.

## 3. String eval has its own disk cache

Generating code with `eval "STRING"` when a module loads is a standard
pure-Perl idiom (JSON::PP, Moo and Sub::Quote, Class::Accessor, Moose,
Type::Tiny; one measured case runs 80 such evals). So each distinct eval
text's transpile is kept under `~/.pcl-cache/evals/`, one `.lisp` and one
`.deps` per entry, and a program does not pay those transpiles every time
it starts.

The key is exactly what decides the output and nothing more: the Perl text,
the caller's package, the names of any captured lexical variables and the
feature set in force, plus the compiler generation and the same **compiler
fingerprint** a module's key holds. So an eval entry is no more shareable
between two PCL trees, or across a PPI upgrade, than a module's is
(`%p-eval-cache-stem`, and the section "THE STRING-EVAL DISK CACHE" in
`cl/pcl-runtime.lisp`). Validity uses the same `.deps` manifest as §2: an
eval that `use`s a module is re-transpiled when that module's content
changes. A **failing** eval (a syntax error in the string) writes no entry:
perl retries a failing eval every time, so PCL does too. This directory has
no natural size limit (an eval per loop iteration is ordinary Perl, and a
few of perl's own test files leave hundreds of entries each), so it is the
one where the 30-day last-use prune matters most.

`pcl --no-cache` or `PCL_NO_CACHE=1` disables this together with the module
and script caches: one switch for all three. `pcl --cache-info` lists
`evals/` beside the others, and `pcl --clear-cache` removes it.

## 4. The compiled extensions (pack, mro, warnings, xs)

`cl/pcl-pack.lisp`, `cl/pcl-mro.lisp` and `cl/pcl-warnings.lisp` are
Perl-to-Lisp transpiles of PCL's `pack`, `mro` and `warnings` support,
checked into the tree so that PCL does not need perl to build them at run
time; `cl/pcl-xs.lisp` is hand-written Lisp. They load into a running
program on first use of `pack`/`unpack`, `mro::...`, `warnings::...` or an
XS module, never before ([`extensions.md`](extensions.md)).

Each is compiled once and cached, with **the same machinery as §2**: a
temporary file renamed into place, the `.failed` marker, and the 30-day
last-use prune. The saving is large: `cl/pcl-pack.lisp` takes 4.26 seconds
to load as text and 0.004 seconds as a compiled file (measured 2026-09-20).
It matters beyond `pack` users: `Sub::Quote` calls `pack("F",0)` when it
loads, and Moo loads `Sub::Quote` for any `has`, so every Moo class with an
attribute reaches `pack`.

One thing differs from a module, because an extension is not a transpile:

> **The key is the extension file's own bytes**, plus the runtime identity
> (Appendix A). An entry is `<cache>/ext/<name>-<content stem>-<runtime
> identity>.fasl`.

A module entry is keyed by its *path* and checked against a dependency
manifest. An extension has no separate source to fall out of date with, so
there is nothing left to check: **a stale extension entry cannot be
reached.** Regenerate `cl/pcl-pack.lisp`, and the next run computes a
different name, builds a new entry, and deletes the superseded one *for
this runtime* (entries built against a different runtime belong to another
tree and simply age out).

Two notes:

* `--no-cache` and `PCL_NO_CACHE` do **not** turn this off, deliberately.
  That switch answers "is it the cache?" about a transpile of *your* code.
  An extension is a checked-in file compiled against this runtime and keyed
  by its bytes, which is exactly what the saved core of §1 is, and
  `--no-cache` does not disable the core either (`PCL_NO_CORE` does).
  **`PCL_NO_FASL_CACHE=1`** is the switch, and it turns off every compiled
  file at once.
* Every failure (an unreadable file, a refused build, a broken compiled
  file) ends in loading the text, so the worst case is the slow path, never
  a wrong answer. `PCL_FASL_DEBUG=1` names the path taken for each
  extension (`FASL HIT`, `fasl-build` or `TEXT`).

## 5. Measured numbers

Moved to the end of the page: [Appendix B](#appendix-b-measured-numbers).

## 6. Settings

### Environment variables

| variable | meaning | default |
|---|---|---|
| `PCL_CACHE_DIR` | root of every per-user cache: `modules/`, `scripts/`, `evals/`, `ext/`, `proto/`, `core/`, `xs/` | `~/.pcl-cache` |
| `PCL_COMPILE_DIRS` | colon-separated directories (`PERL5LIB` syntax); a module under one is compiled to a `.fasl`. `*` = every directory. The **main script** is exempt from this list (§2c) | perl's installed library directories and PCL's own `lib/` |
| `PCL_NO_COMPILE_DIRS` | same syntax; a module, or the main script, under one is **never** compiled to native code (it still gets the `.lisp` and manifest cache); wins over `PCL_COMPILE_DIRS` on any match. `*` = compile nothing | empty |
| `PCL_NO_FASL_CACHE=1` | the older name for `PCL_NO_COMPILE_DIRS='*'`; also the one switch that turns off the **compiled extensions** of §4 | |
| `PCL_NO_CACHE` / `pcl --no-cache` | this run reads and writes no module cache, **no script cache and no eval cache**: one switch. It does *not* disable the saved core (§1) or the compiled extensions (§4), which are compiles of files in PCL's own tree, keyed by their bytes | off |
| `PCL_OPT` | switch named speed optimizations off (`PCL_OPT=none` is the fully generic output). Not a cache setting, but it selects different compiler output, so it is part of the compiler fingerprint, together with `PCL_NO_RAW_VERDICT`, `PCL_FACTS` and `PCL_IR_PLAIN`: a run under one setting never reads an entry written under another. `pcl --cache-info` prints the ones in force | all optimizations on |
| `PCL_NO_CORE=1` | never build or use a saved core; always load the runtime from source | off |
| `PCL_CORE=path` | use this specific saved core | |
| `PCL_FASL_DEBUG=1` | trace each module and extension load: `FASL HIT`, `fasl-build` or `TEXT`, and why | off |
| `PCL_SHOW_SBCL=1` | print the exact `sbcl` command line a run starts (which core, which flags) | off |

### Flags

| flag | what it does |
|---|---|
| `pcl --cache-info` | where the cache is, size and count per kind, which core this run would use and why, the compile policy in effect, and the compiler fingerprint with the perl binary, the PPI sources and the output-selecting environment it hashed. **The** diagnostic for "PCL did not notice my change" |
| `pcl --clear-cache` | removes everything under `PCL_CACHE_DIR` that PCL made: modules, scripts and evals (`.lisp`, `.fasl`, `.deps`, `.failed`), compiled extensions, prototype notes and saved cores. One flag, no selection; the core rebuilds on the next run (about 20 seconds). **Not** the XS artifacts (`tools/pcl-xs-install --clean` removes those) |
| `pcl --no-cache` | this run only: skip the module, script and eval caches entirely. Not the core (§1) or the compiled extensions (§4): `PCL_NO_CORE` and `PCL_NO_FASL_CACHE` are those |
| `pcl --version` | PCL version, cache generation, SBCL version, PPI version |
| `pcl --make-core` | build the cached core now, then exit |

`pcl --help` has the live, authoritative text.

## 7. Several users, several checkouts, and CI

- **The cache is per `$HOME`** (or per `$PCL_CACHE_DIR`): two users, or one
  user with two different `PCL_CACHE_DIR`s, share nothing.
- **Each checkout or `git worktree` gets its own runtime core:** the core's
  name includes the runtime file's absolute path, so two trees never
  collide there, even with byte-identical runtime source.
- **The module and eval caches are shared** across every tree using the
  same `$PCL_CACHE_DIR`, but only between trees with the same **compiler
  fingerprint** (Appendix A: the PCL tree's path and its files, plus perl and PPI).
  Two checkouts, or two branches in development, get *different* keys and
  cannot read each other's entries. A checkout and an install of the same
  source are two trees by path, so they do not share either.
  `pcl --cache-info` prints the fingerprint the run computed.
- **The compiled extensions (§4) are shared more widely, on purpose:**
  their key is the extension file's bytes plus the runtime identity, and
  neither includes a path. So a checkout, a `git worktree` of it and an
  install of the same source all reach the *same* `ext/` entry, and only one
  of them pays the compile. Two trees whose runtime source differs get
  different entries, and neither deletes the other's.
- **A shared, system-wide install** (root installs to `/opt/pcl` and
  several users run it) works for the module and eval caches: each user's
  `pcl` writes its own cache under their own home, never under the install
  prefix. **One known limitation:** a saved core remembers the ASDF cache
  directory of whoever *built* it, so any `asdf:load-system` work done from
  a shared install's core (recompiling the vendored cl-ppcre, say) can
  target a directory another user cannot write to. Ordinary Perl `use` and
  `require` are unaffected.
- `tools/install-pcl --uninstall` removes the installed tree and its
  wrappers only. It never touches any user's `~/.pcl-cache`; use
  `pcl --clear-cache` for that.

See also: the "Caches" paragraph in [`README.md`](../README.md) (the short
version), [`ir-spec.md`](ir-spec.md) §9.2 and §9.2b (the on-disk format,
normative), and [`test-infrastructure.md`](test-infrastructure.md) (the
saved core's development history and how PCL's own tests use it).

## Appendix A. What the cache key holds, and why

**What the key holds, and so what a change to it re-transpiles.** The
`<key>` is a hash of the module's absolute path, the cache *generation*
string (which changes whenever the compiler's output changes), and a
**fingerprint of the compiler that writes the entry**: the PCL tree's own
path, the modification time and size of every `Pl/**.pm` and of `pl2cl`,
the `perl` binary first on `$PATH`, and every one of PPI's own `.pm` files.
The `.fasl` name adds the runtime identity (`cl/pcl-runtime.lisp`'s content
plus the SBCL version), because a compiled file has this runtime's macro
expansions built in.

PPI and perl are part of the fingerprint because **a transpile's output
depends on PPI's token stream**. PCL's repairs of PPI's tokenizing are
keyed on PPI 1.291's stream, and a PPI release that fixes one of the bugs
in [`ppi-upstream-bugs.md`](ppi-upstream-bugs.md) changes that stream. A
perl *major* upgrade moves the library directories, so module paths and
keys move with them; the case this covers is the **in-place** upgrade (a
distribution PPI update, `cpanm PPI`, a same-version perl rebuild), where
nothing else in the key would move. The fingerprint uses modification time
and size rather than a version string because `$PPI::VERSION` does not
change for a patched PPI, and `$]` does not change for a rebuilt perl. The
cost, measured 2026-09-17: about 95 extra `stat` calls, about 0.8
milliseconds, paid once per process that loads a module and invisible in a
`pcl` run's wall time. The compiler's prototype notes (`proto/`) are keyed
on the same fingerprint.

## Appendix B. Measured numbers

Each row is an A/B comparison on one machine, best of several interleaved
runs. Treat them as "this cache is worth roughly this much", not as a
portable benchmark. The rows stack: `use JSON::PP` with no compiled caching
at all took 13.4 seconds; module compilation (§2) brought it to 1.43
seconds, and the string-eval cache (§3) on top, since JSON::PP's own loader
uses `eval`, to 0.41 seconds.

| what | before | after | measured | what changed |
|---|---|---|---|---|
| startup, warm core, no `use` | | ~0.17 s | 2026-09-05 | |
| `use Carp` | 0.406 s | 0.216 s | 2026-09-05 | module compilation (§2) |
| `use Scalar::Util qw(blessed)` | 0.337 s | 0.170 s | 2026-09-05 | module compilation |
| `use Moo` | 1.127 s | 0.417 s | 2026-09-05 | module compilation |
| `use JSON::PP` | 2.751 s | 1.432 s | 2026-09-05 | module compilation alone |
| `use JSON::PP` (9 dependencies) | 1.118 s | 0.999 s | 2026-09-06 | the dependency-hash check adds no measurable cost |
| `use JSON::PP; print 1` | 1.13 s | 0.41 s | 2026-09-07 | the string-eval cache (§3), on top of the above |
| `eval "1"` | 0.304 s | 0.172 s | 2026-09-07 | the string-eval cache |
| `use Moo` | 0.343 s | 0.201 s | 2026-09-07 | the string-eval cache |
| `use JSON::PP`, cold, no compiled caching at all | 13.42 s | | 2026-09-05 | the baseline |
| `pack("N",1)` | 5.588 s | **0.280 s** | 2026-09-20 | the extension cache (§4) |
| a Moo class with one `has` | 5.774 s | **1.050 s** | 2026-09-20 | the extension cache; it never mentions `pack`, but `Sub::Quote` does |
| loading `cl/pcl-pack.lisp` | 4.262 s as text | **0.004 s** compiled | 2026-09-20 | the extension cache (the one-time compile costs 4.70 s) |
| `pcl hello.pl` (1 line) | 0.181 s | **0.033 s** | 2026-09-17 | the script cache (§2c); the first run costs 0.203 s |
| `pcl cl/pack-impl.pl` (1,211 lines) | 6.571 s | **0.037 s** | 2026-09-17 | the script cache; the first run costs 7.256 s, and perl itself takes 0.006 s |
| `pcl -e 'print "hi\n"'`, `pcl -l h.pl`, `pcl -lane ... file` (switches on the command line: never cached) | 0.181 s, 0.184 s, 0.191 s | | 2026-10-04 | median of five; the cached `pcl h.pl` took 0.041 s in the same run, `perl -e` 0.002 s |
| the compiler fingerprint, per process | 1.15 ms | 1.95 ms | 2026-09-17 | the perl and PPI part of the fingerprint (Appendix A) |
