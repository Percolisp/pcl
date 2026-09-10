# PCL command reference

The commands you run, what they do, and how the two compiled things — the
runtime library and the modules a program `use`s — get built and cached.
Every option below is taken from the tools' own `--help` output; run
`pcl --help` and `pl2cl --help` for the authoritative text of the version you
have.  The design behind the caches is in [`caching.md`](caching.md).

## NAME

`pcl` — run a Perl program by compiling it to Common Lisp and executing it
under SBCL.  `pl2cl` — the compiler by itself.  `runpcl` — compile and run
one file with no options.

## SYNOPSIS

```
pcl [options] script.pl [args...]
pcl [options] -e 'code' [args...]
pcl --version | --cache-info | --make-core | --clear-cache

pl2cl [options] file.pl            # Common Lisp on stdout
pl2cl --executable -o prog file.pl # a standalone binary
pl2cl --bundle -o prog.fasl file.pl

runpcl file.pl
```

## pcl — run a program

Works like `perl` for the Perl subset PCL supports: `@ARGV`, `%ENV`, `$0`,
exit codes, `die` and `END` blocks behave as the running process's.  Your
script is compiled on every run (only modules are cached), so a large script
pays a pause before its first line.

| option | meaning |
|---|---|
| `-e CODE`, `-E CODE` | run inline code, like `perl -e` (`-E` also enables `say`) |
| `-I DIR` | prepend DIR to `@INC` (repeatable); since s473i it also reaches the transpile of every module the program loads |
| `-M MODULE` | `use MODULE` before running (repeatable; `-MList::Util=sum` imports) |
| `-c` | compile only, print `syntax OK`, exit |
| `-w` | accepted for compatibility |
| `-v`, `--verbose` | print the `sbcl` command line `pcl` runs |
| `--version` | print the PCL, cache-generation, SBCL and PPI versions |
| `--cache-info` | where the cache is, what is in it, which core this run would use, the compile policy in effect — the one diagnostic for "PCL did not notice my change" |
| `--no-cache` | this run reads and writes no module cache — the one-flag answer to "is it the cache?" |
| `--make-core` | build the cached runtime core now, then exit (every run builds one on first use anyway) |
| `--clear-cache` | remove everything PCL made under the cache directory (cached modules, prototype facts, saved cores), then exit; XS artifacts are left alone |
| `-h`, `--help` | the full text, including the environment variables |

Examples:

```bash
pcl script.pl arg1 arg2
pcl -e 'print 1 + 2, "\n"'
pcl -MList::Util=sum -E 'say sum 1 .. 10'
pcl -I lib script.pl
pcl -c script.pl
```

## pl2cl — the compiler

Reads Perl, writes Common Lisp on stdout.  `pcl` runs this for you; use it
directly to see what PCL makes of your code, or to compile a program once.

| mode / option | meaning |
|---|---|
| (default) | transpile to stdout: `pl2cl file.pl`, `pl2cl '$x = 1;'`, `echo '$x = 1;' \| pl2cl` |
| `--executable` | save a standalone binary that runs the program when started.  The compile phase (subs, packages, `use`d modules, `BEGIN`) happens at build time, as in perl; the run phase happens when the binary starts.  Not yet embedded, so the binary needs this PCL tree on the machine: a run-time `require` of a module not already loaded, and the pack/mro/warnings extensions ([`single-binary-plan.md`](single-binary-plan.md)) |
| `--bundle` | compile runtime + program into one `.fasl`; loading it runs the program (`sbcl --load out.fasl`) |
| `-o FILE`, `--output FILE` | output file (default `<input>.fasl` or `<input>`) |
| `--no-cache` | the emitted program runs without the module cache (also `PCL_NO_CACHE=1`) |
| `--cache-lisp` | cache `.lisp` instead of `.fasl` (for debugging) |
| `--module` | emit a module: no program preamble (what the runtime spawns when a `use` misses the cache) |
| `--extension` | build a checked-in `cl/*.lisp` artifact (see below) |
| `--manifest` | the program's IR manifest as JSON: uses / needs / facts ([`ir-spec.md`](ir-spec.md) §10b) |
| `--emit-sexp` | the lowered tree as portable S-expressions ([`ir-spec.md`](ir-spec.md) §12b) |
| `--facts` | annotate the emission with every optimization licence that held |
| `--deps FILE` | write the dependency manifest sidecar the runtime uses for cache validity |

To run the output by hand:

```bash
pl2cl script.pl > script.lisp
sbcl --noinform --non-interactive --load cl/pcl-runtime.lisp --load script.lisp
```

## runpcl — one file, no options

`runpcl file.pl` compiles and runs a single file.  The test suite uses it;
it is handy for quick experiments and for reproducing a bug in one file.

## The runtime library, and how it is compiled

The runtime is `cl/pcl-runtime.lisp`, one Common Lisp file every compiled
program loads.  Compiling it takes about a second, so PCL keeps it compiled
in a **saved SBCL core**:

* **In a checkout**, the first run builds the core under
  `~/.pcl-cache/core/` and every later run starts from it (startup ~1 s →
  ~0.1 s).  The core's file name is a hash of the runtime source, the
  vendored Lisp libraries beside it, the SBCL version and the checkout's
  path, so editing the runtime, replacing a vendored library or upgrading
  SBCL makes a *new* core rather than a stale one; old ones are pruned.
  `pcl --make-core` builds it early, `pcl --cache-info` names the one a run
  would use, `PCL_NO_CORE=1` runs from source instead, `PCL_CORE=path` uses
  a specific one.
* **In an installation**, `tools/install-pcl` compiles the core once, at
  install time, into `<prefix>/lib/pcl/pcl.core` — the model perl uses for
  its own library — so no user's first program pays for it.  Nothing under
  the install prefix is written at run time.

Three parts of the runtime are themselves written in Perl and checked in as
compiled artifacts (`cl/pcl-pack.lisp`, `cl/pcl-mro.lisp`,
`cl/pcl-warnings.lisp`); they load lazily on first `pack`, `mro` or
`warnings` use.  After changing the compiler, regenerate them with
`tools/rebuild-pack` and `pl2cl --extension lib/mro.pm > cl/pcl-mro.lisp`
(same for `lib/warnings.pm`); the test `Pl/t/artifact-staleness-01.t` fails
until you do.  Details: [`extensions.md`](extensions.md).

**Where cl-ppcre comes from.**  The runtime's one external Lisp dependency
is [cl-ppcre](https://edicl.github.io/cl-ppcre/), the regex engine.  It is
**vendored** under `cl/vendor/cl-ppcre/` — upstream source carried verbatim,
never edited here — and the runtime pushes that directory onto
`asdf:*central-registry*` before loading the system, so **a machine needs
SBCL and nothing else**: no Quicklisp, no `~/.sbclrc`, no distribution Lisp
package.  If the directory is missing, ASDF's ordinary search is the
fallback; if neither answers, the load fails with a message naming which of
the two was tried.  `cl/vendor/README.md` has the version and the upstream
commit; `caching.md` §1a has the details.

## Modules, and how they are compiled

A `use` or `require` is resolved through `@INC` — the directories perl
searches, plus `-I` — and the module's source is compiled the same way as
your program, then **cached** as its transpiled Lisp plus a compiled `.fasl`
under `~/.pcl-cache/modules/`.  Only the first run pays.

* **Installing a pure-Perl CPAN module** is `cpanm Module`: PCL finds it in
  perl's library and compiles it.  `pcl -MData::Dump=dump -E 'say dump [1..3]'`
  works right after `cpanm Data::Dump`.
* **Modules implemented in C** (XS) have two answers.  For the common ones —
  `List::Util`, `Scalar::Util`, `POSIX`, `Cwd`, `Fcntl`, `Socket`,
  `IO::Handle` and others, 22 in all — PCL ships a pure-Perl replacement in
  [`lib/`](../lib) and uses it automatically
  ([`shipped-modules.md`](shipped-modules.md)).  For a real XS distribution,
  `tools/pcl-xs-install <unpacked-dist-dir>` builds it against the `pclxs`
  bridge and puts the artifact where the loader looks
  (`~/.pcl-cache/xs/abi-N/auto/...`); `--list` shows what is installed,
  `--clean` drops artifacts built for other ABIs.  This needs the `pclxs`
  checkout beside PCL's.
* **A cached module is re-transpiled** when its own file changes, when any
  module whose prototypes or exports its parse read changes, and when the
  compiler itself changes (the cache key names the compiler, since s473i).
  Entries unused for 30 days are removed.  `pcl --no-cache` runs once
  without the cache; `pcl --clear-cache` empties it.
* **What gets compiled to native code** is a policy: by default modules under
  perl's installed library directories and PCL's own `lib/` are compiled to
  `.fasl`, while a module you are editing (anywhere else) is cached as
  readable text.  `PCL_COMPILE_DIRS` and `PCL_NO_COMPILE_DIRS` (colon-separated
  directories, `PERL5LIB` syntax, `*` = all) change that.
* **Running a CPAN distribution's own test suite** under PCL:
  `tools/run-dist-t.pl <dist-dir> <t-file>` runs one test file
  (`--no-dist-lib` keeps the dist's own `lib/` off `@INC`, needed where it
  would shadow a shipped replacement), and
  `tools/cpan-scoreboard.pl --jobs 8 --timeout 120 DIST...` scores whole
  distributions as PASS / PARTIAL / FAIL per file.  Always run real perl
  beside it: a file perl skips is not a PCL failure.

## Installing

```bash
tools/install-pcl --prefix ~/.local              # default prefix: $HOME/.local
tools/install-pcl --no-core --dry-run            # show the steps, build nothing
tools/install-pcl --force --prefix ~/.local      # replace an existing install
tools/install-pcl --uninstall --prefix ~/.local  # remove the tree and its wrappers
```

Installs `pcl`, `pl2cl` and `runpcl` into `<prefix>/bin` as small wrappers
and the runtime tree into `<prefix>/lib/pcl`, compiles the core there, and
refuses to finish unless the installed tools compile and run a program.
Your per-user module cache is separate from an installation and survives an
uninstall; `pcl --clear-cache` empties it.

## Developer commands

| command | what it runs |
|---|---|
| `tools/prove-core` | PCL's own regression suite (`prove -j8 Pl/t/`) on a fresh temporary core; also accepts any `prove` arguments, e.g. one file |
| `perl tools/sweep-perl-tests.pl --jobs 8` | the extracted perl test files under `perl-tests/`, compared row by row against the blessed baselines |
| `tools/run-perl-suite.pl --all --quick --jobs 4` | perl's whole `t/` tree, run in place, with real perl as the oracle |
| `tools/pcl-conform` | the XS bridge's conformance corpus (minutes) |
| `tools/ir-conform --jobs 2` | the IR conformance corpus under `ir-conform/` |
| `perl tools/bench-exec.pl [names...]` | the benchmark board, startup subtracted, best of five |
| `tools/pcl-xs-install DIR` / `--list` / `--clean` | build, list or prune XS artifacts |

## ENVIRONMENT

| variable | effect |
|---|---|
| `PCL_CACHE_DIR` | root of every per-user cache: compiled modules, prototype facts, saved cores, XS artifacts (default `~/.pcl-cache`, created `0700`) |
| `PCL_COMPILE_DIRS` | directories (colon-separated) whose modules are compiled to native code; `*` = every directory; unset = perl's library directories plus PCL's `lib/` |
| `PCL_NO_COMPILE_DIRS` | directories whose modules are never compiled; wins over `PCL_COMPILE_DIRS`; `*` = compile nothing (`PCL_NO_FASL_CACHE=1` is the kept alias) |
| `PCL_NO_CACHE=1` | the program runs without the module cache (what `--no-cache` sets) |
| `PCL_CORE=path` | use this saved core |
| `PCL_NO_CORE=1` | never build or use a cached core (an installed one still counts) |
| `PCL_OPT` | switch named optimizations off: `PCL_OPT=none` is the fully generic compiler, `PCL_OPT=-raw-numeric,-str-buffer` names individual ones; a typo dies naming the known list |
| `PCL_ROOT` | where an installed command looks for its runtime tree (a packaging escape hatch; every command finds its tree beside its own real path) |
| `PCL_MEM_CAP_MB` | address-space cap for a `pl2cl` process (default 4096) |
| `PCL_SHOW_SBCL=1` | every runner prints the exact `sbcl` command it spawns |
| `_PCL_RUNTIME_` | **set by PCL, not read by it**: true in every PCL process, its value the version `pcl --version` prints.  It is synthetic (in `%ENV`, not in the real environment), so a child process does not inherit it — `$^X` is real perl |

## FILES

| path | what |
|---|---|
| `~/.pcl-cache/core/` | saved runtime cores, one per runtime source × SBCL version × checkout path |
| `~/.pcl-cache/modules/` | transpiled modules, their `.fasl`s and dependency manifests |
| `~/.pcl-cache/proto/` | the compiler's prototype and export facts per module |
| `~/.pcl-cache/xs/abi-N/` | XS artifacts built by `tools/pcl-xs-install`, keyed by bridge ABI |
| `<prefix>/lib/pcl/pcl.core` | the core an installation compiled at install time |
| `cl/pcl-runtime.lisp` | the runtime library |
| `cl/pcl-pack.lisp`, `cl/pcl-mro.lisp`, `cl/pcl-warnings.lisp` | the three checked-in extensions, written in Perl and compiled by PCL |
| `cl/vendor/cl-ppcre/` | the vendored regex engine, upstream source carried verbatim (`cl/vendor/README.md`) |
| `lib/` | the pure-Perl replacements for C-implemented modules |

## SEE ALSO

[`caching.md`](caching.md) (the cache design and its measured numbers),
[`shipped-modules.md`](shipped-modules.md), [`extensions.md`](extensions.md),
[`not-supported.md`](not-supported.md), [`STATUS.md`](STATUS.md),
[`single-binary-plan.md`](single-binary-plan.md).
