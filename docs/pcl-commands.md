# PCL command reference

The commands you run, what each option does, and how the three compiled
things (the runtime library, the modules a program `use`s, and the program
itself) are built and cached. The options below follow the tools' own
`--help` output; run `pcl --help` and `pl2cl --help` for the authoritative
text of the version you have. The design behind the caches is in
[`caching.md`](caching.md).

## NAME

`pcl`: run a Perl program by compiling it to Common Lisp and executing it
under SBCL. `pl2cl`: the compiler by itself. `runpcl`: compile and run one
file with no options.

## SYNOPSIS

```
pcl [options] script.pl [args...]
pcl [options] -e 'code' [args...]
pcl --check [options] script.pl [args...]
pcl --version | --cache-info | --make-core | --clear-cache

pl2cl [options] file.pl            # Common Lisp on stdout
pl2cl --executable -o prog file.pl # a standalone binary
pl2cl --bundle -o prog.fasl file.pl

runpcl file.pl
```

## pcl: run a program

`pcl` works like `perl` for the Perl that PCL supports: `@ARGV`, `%ENV`,
`$0`, exit codes, `die` and `END` blocks behave as they do in perl. Your
script is compiled on its first run after an edit and cached like a module
(in `~/.pcl-cache/scripts/`), so a large script pauses once before its first
line; later runs start from the cache in about 0.04 seconds. `-e` code is
not cached. The runtime core that every run needs is a separate cache,
built once per runtime change (the message is `compiling the runtime into a
cached core`), and a one-liner can trigger that build too.

| option | meaning |
|---|---|
| `-e CODE`, `-E CODE` | run inline code, like `perl -e`. The two are the same, and both enable `say` |
| `-I DIR` | prepend DIR to `@INC` (repeatable); it also applies to the compile of every module the program loads |
| `-M MODULE` | `use MODULE` before running (repeatable; `-MList::Util=sum` imports) |
| `-c` | compile only, print `syntax OK`, exit |
| `-w` | accepted for compatibility |
| `-v`, `--verbose` | print the `sbcl` command line `pcl` runs |
| `--check` | run the program under perl and under PCL and compare the output ([`pcl-check.md`](pcl-check.md)); `--check-stdin FILE` gives both runs FILE as STDIN, `--check-keep DIR` keeps the captured output in DIR |
| `--version` | print the PCL, cache-generation, SBCL and PPI versions |
| `--cache-info` | where the cache is, what is in it, which core this run would use, and the compile policy in effect: the one diagnostic for "PCL did not notice my change" |
| `--no-cache` | this run reads and writes no module, script or eval cache: the quick answer to "is it the cache?" |
| `--make-core` | build the cached runtime core now, then exit (every run builds one on first use anyway) |
| `--clear-cache` | remove everything PCL made under the cache directory (cached modules, scripts and evals, compiled extensions, prototype facts, saved cores), then exit; XS artifacts are left alone |
| `-h`, `--help` | the full text, including the environment variables |

Examples:

```bash
pcl script.pl arg1 arg2
pcl -e 'print 1 + 2, "\n"'
pcl -MList::Util=sum -E 'say sum 1 .. 10'
pcl -I lib script.pl
pcl -c script.pl
pcl --check script.pl arg1
```

## pl2cl: the compiler

`pl2cl` reads Perl and writes Common Lisp on stdout. `pcl` runs it for you;
use it directly to see what PCL makes of your code, or to compile a program
once.

| mode / option | meaning |
|---|---|
| (default) | transpile to stdout: `pl2cl file.pl`, `pl2cl '$x = 1;'`, `echo '$x = 1;' \| pl2cl` |
| `--executable` | save a standalone binary that runs the program when started. The compile phase (subs, packages, `use`d modules, `BEGIN`) happens at build time, as in perl; the run phase happens when the binary starts. Two things are not embedded yet, so the binary needs this PCL tree on the machine: a run-time `require` of a module not already loaded, and the `pack`/`mro`/`warnings` extensions ([`single-binary-plan.md`](single-binary-plan.md)) |
| `--bundle` | compile the runtime and the program into one `.fasl`; loading it runs the program (`sbcl --load out.fasl`) |
| `-o FILE`, `--output FILE` | output file (default `<input>.fasl` or `<input>`) |
| `--no-cache` | the emitted program runs without the module, script and eval caches (also `PCL_NO_CACHE=1`) |
| `--cache-lisp` | cache `.lisp` instead of `.fasl` (for debugging) |
| `--module` | emit a module: no program preamble (what the runtime runs when a `use` misses the cache) |
| `--extension` | build a checked-in `cl/*.lisp` artifact (see below) |
| `--manifest` | print the program's IR manifest as JSON: uses, needs, facts ([`ir-spec.md`](ir-spec.md) §10b) |
| `--emit-sexp` | print the lowered tree as portable S-expressions ([`ir-spec.md`](ir-spec.md) §12b) |
| `--facts` | annotate the emission with every optimization licence that held |
| `--deps FILE` | write the dependency manifest the runtime uses to decide whether a cache entry is still valid |

To run the output by hand:

```bash
pl2cl script.pl > script.lisp
sbcl --noinform --non-interactive --load cl/pcl-runtime.lisp --load script.lisp
```

## runpcl: one file, no options

`runpcl file.pl` compiles and runs a single file. The test suite uses it,
and it is handy for quick experiments and for reproducing a bug in one file.

## The runtime library, and how it is compiled

The runtime is `cl/pcl-runtime.lisp`, one Common Lisp file that every
compiled program loads. Loading it from source takes about a second, so PCL
keeps it compiled in a **saved SBCL core**:

* **In a checkout**, the first run builds the core under
  `~/.pcl-cache/core/`, and every later run starts from it (startup drops
  from about 1 second to about 0.1 seconds). The core's file name is a hash
  of the runtime source, the vendored Lisp library beside it, the SBCL
  version and the checkout's path. So editing the runtime, replacing the
  vendored library or upgrading SBCL makes a *new* core rather than a stale
  one, and old ones are pruned. `pcl --make-core` builds it early,
  `pcl --cache-info` names the one a run would use, `PCL_NO_CORE=1` runs
  from source instead, and `PCL_CORE=path` uses a specific core.
* **In an installation**, `tools/install-pcl` compiles the core once, at
  install time, into `<prefix>/lib/pcl/pcl.core` (the model perl uses for
  its own library), so no user's first program pays for it. Nothing under
  the install prefix is written at run time.

Three parts of the runtime are written in Perl and checked in as compiled
artifacts (`cl/pcl-pack.lisp`, `cl/pcl-mro.lisp`, `cl/pcl-warnings.lisp`).
They load on first use of `pack`, `mro` or `warnings::`. After changing the
compiler, regenerate them (the commands are in
[`extensions.md`](extensions.md)); the test `Pl/t/artifact-staleness-01.t`
fails until you do.

**Where cl-ppcre comes from.** The runtime's one external Lisp dependency is
[cl-ppcre](https://edicl.github.io/cl-ppcre/), the regex engine. It is
**vendored** under `cl/vendor/cl-ppcre/`: upstream source carried verbatim,
never edited here. The runtime tells ASDF (the Lisp build tool) to look
there first, so **a machine needs SBCL and nothing else**: no Quicklisp, no
`~/.sbclrc`, no distribution Lisp package. If the directory is missing,
ASDF's ordinary search is the fallback; if neither finds it, the load fails
with a message naming which of the two was tried. `cl/vendor/README.md` has
the version and the upstream commit; [`caching.md`](caching.md) §1a has the
details.

## Modules, and how they are compiled

A `use` or `require` is resolved through `@INC` (the directories perl
searches, plus `-I` and `PERL5LIB`), and — as in perl 5.26 and later —
neither the current directory nor the script's own directory is on it: a
module beside your script is found through `use lib`, `FindBin` or `-I`, as
under perl.  The module's source is compiled the same way as
your program, then **cached** as its transpiled Lisp plus a compiled `.fasl`
under `~/.pcl-cache/modules/`. Only the first run pays. The program you run
gets an entry of the same kind under `~/.pcl-cache/scripts/`, keyed on its
path, the `-I` list, the compiler version and the compiler's own files
(`-e` code is not cached).

* **Installing a pure-Perl CPAN module** is `cpanm Module`: PCL finds it in
  perl's library and compiles it. `pcl -MData::Dump=dump -E 'say dump [1..3]'`
  works right after `cpanm Data::Dump`.
* **Modules implemented in C (XS)** have two answers. For common ones such
  as `List::Util`, `Scalar::Util`, `POSIX`, `Cwd`, `Fcntl`, `Socket` and
  `IO::Handle`, PCL ships a pure-Perl replacement in [`lib/`](../lib) and
  uses it automatically ([`shipped-modules.md`](shipped-modules.md)). For
  a real XS distribution, the experimental bridge (see
  [`STATUS.md`](STATUS.md#xs); it needs the separate `pclxs` checkout
  beside PCL's and is being reworked) is driven by
  `tools/pcl-xs-install <unpacked-dist-dir>`, which builds the distribution
  and puts the artifact where the loader looks
  (`~/.pcl-cache/xs/abi-N/auto/...`); `--list` shows what is installed,
  and `--clean` drops artifacts built for other bridge versions. Any other
  XS module fails to load with perl's "Can't locate" message.
* **A cached module or script is re-transpiled** when its own file changes,
  when any module whose prototypes or exports its parse read changes, and
  when the compiler itself changes (the cache key covers the compiler, the
  `perl` binary and PPI's files). Entries unused for 30 days are removed.
  `pcl --no-cache` runs once without the cache; `pcl --clear-cache` empties
  it.
* **What gets compiled to native code** is a policy. By default, modules
  under perl's installed library directories and PCL's own `lib/` are
  compiled to `.fasl`, while a module you are editing (anywhere else) is
  cached as readable text. `PCL_COMPILE_DIRS` and `PCL_NO_COMPILE_DIRS`
  (colon-separated directories, `PERL5LIB` syntax, `*` = all) change that.
* **Running a CPAN distribution's own test suite** under PCL:
  `tools/run-dist-t.pl <dist-dir> <t-file>` runs one test file
  (`--no-dist-lib` keeps the distribution's own `lib/` off `@INC`, needed
  where it would shadow a shipped replacement), and
  `tools/cpan-scoreboard.pl --jobs 8 --timeout 120 DIST...` scores whole
  distributions as PASS / PARTIAL / FAIL per file. Always run real perl
  beside it: a file perl skips is not a PCL failure.

## Installing

```bash
tools/install-pcl --prefix ~/.local              # default prefix: $HOME/.local
tools/install-pcl --no-core --dry-run            # show the steps, build nothing
tools/install-pcl --force --prefix ~/.local      # replace an existing install
tools/install-pcl --uninstall --prefix ~/.local  # remove the tree and its wrappers
```

This installs `pcl`, `pl2cl` and `runpcl` into `<prefix>/bin` as small
wrappers and the runtime tree into `<prefix>/lib/pcl`, compiles the core
there, and refuses to finish unless the installed tools compile and run a
program. Your per-user module cache is separate from an installation and
survives an uninstall; `pcl --clear-cache` empties it.

## Developer commands

| command | what it runs |
|---|---|
| `tools/prove-core` | PCL's own regression suite (`prove -j8 Pl/t/`) on a freshly built core; also accepts any `prove` arguments, such as one file |
| `perl tools/sweep-perl-tests.pl --jobs 8` | the extracted perl test files under `perl-tests/`, compared assertion by assertion against the blessed baselines |
| `tools/run-perl-suite.pl --all --quick --jobs 4` | perl's whole `t/` tree, run in place, with real perl as the oracle |
| `tools/pcl-conform` | the XS bridge's conformance corpus (minutes) |
| `tools/ir-conform --jobs 2` | the IR conformance corpus under `ir-conform/` |
| `perl tools/bench-exec.pl [names...]` | the benchmark board, startup subtracted, best of five |
| `tools/pcl-xs-install DIR` / `--list` / `--clean` | build, list or prune XS artifacts |

## ENVIRONMENT

| variable | effect |
|---|---|
| `PCL_CACHE_DIR` | root of every per-user cache: compiled modules, scripts and evals, prototype facts, saved cores, XS artifacts (default `~/.pcl-cache`, created with mode `0700`) |
| `PCL_COMPILE_DIRS` | directories (colon-separated) whose modules are compiled to native code; `*` = every directory; unset = perl's library directories plus PCL's `lib/` |
| `PCL_NO_COMPILE_DIRS` | directories whose modules are never compiled; wins over `PCL_COMPILE_DIRS`; `*` = compile nothing (`PCL_NO_FASL_CACHE=1` is the older name for that) |
| `PCL_NO_CACHE=1` | the program runs without the module, script and eval caches (what `--no-cache` sets) |
| `PCL_CORE=path` | use this saved core |
| `PCL_NO_CORE=1` | never build or use a cached core (an installed one still counts) |
| `PCL_OPT` | switch named optimizations off: `PCL_OPT=none` is the fully generic compiler, `PCL_OPT=-raw-numeric,-str-buffer` names individual ones; a misspelled name dies, listing the known ones |
| `PCL_ROOT` | where an installed command looks for its runtime tree (an escape hatch for packagers; normally every command finds its tree beside its own real path) |
| `PCL_MEM_CAP_MB` | address-space cap for a `pl2cl` process (default 4096) |
| `PCL_SHOW_SBCL=1` | print the exact `sbcl` command each run starts |
| `_PCL_RUNTIME_` | **set by PCL, not read by it**: true in every PCL process, and its value is the version `pcl --version` prints. It exists only in `%ENV`, not in the real environment, so a child process does not inherit it (`$^X` is real perl) |

## FILES

| path | what |
|---|---|
| `~/.pcl-cache/core/` | saved runtime cores, one per runtime source, SBCL version and checkout path |
| `~/.pcl-cache/modules/` | transpiled modules, their `.fasl`s and dependency manifests |
| `~/.pcl-cache/scripts/` | the same three files for the program `pcl FILE` runs |
| `~/.pcl-cache/evals/` | compiled string evals |
| `~/.pcl-cache/ext/` | the compiled `pack`/`mro`/`warnings` extensions |
| `~/.pcl-cache/proto/` | the compiler's prototype and export facts per module |
| `~/.pcl-cache/xs/abi-N/` | XS artifacts built by `tools/pcl-xs-install`, keyed by bridge version |
| `<prefix>/lib/pcl/pcl.core` | the core an installation compiled at install time |
| `cl/pcl-runtime.lisp` | the runtime library |
| `cl/pcl-pack.lisp`, `cl/pcl-mro.lisp`, `cl/pcl-warnings.lisp` | the three checked-in extensions, written in Perl and compiled by PCL |
| `cl/vendor/cl-ppcre/` | the vendored regex engine, upstream source carried verbatim (`cl/vendor/README.md`) |
| `lib/` | the pure-Perl replacements for modules PCL cannot load as they are |

## SEE ALSO

[`caching.md`](caching.md) (the cache design and its measured numbers),
[`pcl-check.md`](pcl-check.md), [`shipped-modules.md`](shipped-modules.md),
[`extensions.md`](extensions.md), [`not-supported.md`](not-supported.md),
[`STATUS.md`](STATUS.md), [`single-binary-plan.md`](single-binary-plan.md).
