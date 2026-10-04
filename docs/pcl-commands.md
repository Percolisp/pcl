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
pcl [switches] [--] [programfile] [arguments]
pcl [switches] -e 'code' [arguments]
pcl [switches] < programfile
pcl --check [switches] programfile [arguments]
pcl --version | --cache-info | --make-core | --clear-cache

pl2cl [options] file.pl            # Common Lisp on stdout
pl2cl --executable -o prog file.pl # a standalone binary
pl2cl --bundle -o prog.fasl file.pl

runpcl file.pl
```

## pcl: run a program

`pcl` takes the command line you would give `perl`. It compiles the
program to Common Lisp and runs it under SBCL. `@ARGV`, `%ENV`, `$0`, exit
codes, `die` and `END` blocks behave as they do in perl, and perl's own
switches work, one-liners included:

<!-- doc-example: Pl/t/pcl-doc-examples-01.t runs every command in this block -->
```console
$ pcl -e 'print "hello, world\n"'
hello, world
$ pcl -le 'print for 1 .. 3'
1
2
3
$ printf 'a b c\nd e f\n' | pcl -lane 'print $F[1]'
b
e
$ printf '3\n4\n5\n' | pcl -lne '$sum += $_; END { print $sum }'
12
$ pcl -MList::Util=sum -E 'say sum 1 .. 10'
55
```

A script runs as `pcl script.pl arg1 arg2`. Its first run after an edit
compiles it and stores the result in a cache; later runs start from the
cache (in 41 ms for a one-line script, measured 2026-10-04). A run with
`-e`, or with a switch that changes the program, is compiled each time
([What a run costs](#what-a-run-costs)).

Below is every switch perl 5.40 has and what `pcl` does with it. The
table [Where pcl differs from perl](#where-pcl-differs-from-perl) sums up
the differences.

### Running code

| switch | in `pcl` |
|---|---|
| `-e CODE` | one line of program. Give several `-e` for several lines. `$0` and `__FILE__` are `-e`, and messages say `at -e line N` |
| `-E CODE` | like `-e`, with perl 5.40's features (`say`, `state`, `fc`, `__SUB__`, signatures, `try`/`catch` ...) and builtin functions (`true`, `trim`, `reftype` ...) turned on. `strict` stays off, as in perl. |
| `programfile` | the script to run; the words after it are `@ARGV` |
| `-` | read the program from STDIN; `$0` is `-`. After `-e`, a lone `-` is an ordinary word in `@ARGV` |
| (no program, no `-e`) | read the program from STDIN, as perl does. If STDIN is a terminal, `pcl` prints its usage instead of waiting |
| `--` | ends the switches; the next word is the program, or with `-e` the first word of `@ARGV` |

### Line loops

| switch | in `pcl` |
|---|---|
| `-n` | puts `LINE: while (<>) { ... }` around the program. `BEGIN` and `END` blocks stay outside the loop |
| `-p` | like `-n`, and prints each line after the program has run on it |
| `-l[octal]` | with `-n`/`-p`, chomps each line; sets `$\` to `$/`, or to the character with that octal code. `-l` and `-0` read in order, as in perl: `-l -0040` and `-0040 -l` differ |
| `-a` | splits each line into `@F` (`split ' '`); implies `-n` |
| `-F pattern` | the pattern for `-a`: `-F:`, `-F,`, `-F'\t'`, `-F/\s*;\s*/`; implies `-a` and `-n` |
| `-0[octal]`, `-0xHEX` | sets `$/`: `-0` is the NUL character, `-00` paragraph mode, `-0777` the whole file at once, `-0x3B` a `;` |
| `-g` | the whole file at once (the same as `-0777`) |
| `-i[extension]` | edits the files named in `@ARGV` in place, keeping a backup when an extension is given. A `*` in the extension stands for the file name: `-i'orig_*'` keeps `orig_notes.txt`. With no file names, perl's warning, then STDIN to STDOUT |

### Modules and paths

| switch | in `pcl` |
|---|---|
| `-MModule` | `use Module;` before the program. `-MModule=a,b` imports `a` and `b`, `-M-Module` is `no Module;`, and `-M'Module qw(a b)'` takes the rest as written. The program keeps its own name and line numbers |
| `-mModule` | `use Module ();`: loaded, nothing imported |
| `-I DIR` | puts DIR in front of `@INC`. Several `-I` keep their command-line order, all ahead of `PERL5LIB`. They also apply to the modules the program loads |
| `-S` | looks the program up along `PATH`, with perl's messages when it is missing or not executable |
| `-x[dir]` | skips the text before the first `#!` line that contains `perl`; line numbers count from that line. With a `dir`, changes to it first |
| `-s` | the program's own leading `-name` and `-name=value` arguments are taken out of `@ARGV` and set `$main::name`; `--` ends them |

`-Mstrict` loads `strict` as `use strict` does: `strict refs` is enforced,
but a program that uses an undeclared variable still runs (see
[`not-supported.md`](not-supported.md)).

### Checking and information

| switch | in `pcl` |
|---|---|
| `-c` | runs the compile phase only (`BEGIN` and `CHECK` blocks, `use` lines), then prints `NAME syntax OK` on STDERR and exits 0. It is not a syntax checker: see the table below |
| `-v` | prints `pcl`'s version: the PCL release, the cache generation, the SBCL and the PPI versions (the same as `--version`) |
| `-V`, `-V:name` | PCL's own configuration, `%Config`, in perl's format: `pcl -V:osname` prints `osname='linux';`. A name PCL does not hold prints `name='UNKNOWN';` |
| `-h`, `-?` | `pcl`'s usage text |

### Warnings, Unicode, taint

| switch | in `pcl` |
|---|---|
| `-w` | sets `$^W` to 1 from the start of compilation. No warning is printed: PCL has no warning categories, and `use warnings` is accepted and changes nothing |
| `-W`, `-X` | accepted; they change nothing (under perl, `-W` also sets `$^W`) |
| `-C[number or letters]` | `I`, `O`, `E` and `S`: a `:utf8` layer on STDIN, STDOUT and STDERR. `A`: `@ARGV` is decoded. `i`, `o` and `D`: the default layers of `open()` in the main program. `L`: all of it only under a UTF-8 locale. A bare `-C` is `-CSDL`. `${^UNICODE}` reads the number |
| `-T`, `-t` | the program runs WITHOUT taint checks, and one line on STDERR says so (`PCL_TAINT_QUIET=1` silences it). `${^TAINT}` reads 0 |

### Not supported

| switch | in `pcl` |
|---|---|
| `-d`, `-dt`, `-d:MOD` | the debugger: refused with one line, exit 2 |
| `-u` | dump core: refused with one line, exit 2 |
| `-D[flags]` | prints perl's own message for a perl built without debugging, then runs the program, as such a perl does |
| `-U`, `-f` | accepted; there is nothing for them to do |

An unknown switch is perl's error, `Unrecognized switch: -A  (-h will show
valid options).`, with exit status 25. It is never taken for a script name.

### The script's own `#!` line

When line 1 of the program starts with `#!` and contains `perl`, its
switches count as perl counts them: `#!/usr/bin/perl -w`,
`#!/usr/bin/env perl -lan` and `#!perl -0777 -n` all work, and they add to
the command line's switches. A `#!` line that turns on `-n` or `-p`
rebuilds the loop with the switches in force by then, as in perl.

A few switches cannot be given there, as in perl: `-M` and `-m` are
`Too late`, and `-x -E -S -V -e -f` are `Can't emulate ... on #! line`,
both with exit status 255. `-d -u -v -h -?` are refused there too (exit
255), because perl would start the debugger or print a text instead of
running the program.

A `#!` line that does not name perl, such as `#!/bin/sh`, is a comment to
`pcl`: the file is compiled as Perl. perl would run `/bin/sh` on it.

### Where pcl differs from perl

| what | perl | `pcl` |
|---|---|---|
| taint, `-T` and `-t` | checks tainted data; `${^TAINT}` is 1 (or -1 for `-t`) | runs the program without taint checks and says so on STDERR; `${^TAINT}` is 0. `-T` on the `#!` line alone is not an error |
| `-d`, `-u` | start the debugger, dump core | refused, exit 2 |
| `-w` | turns on warnings | sets `$^W` and nothing else; `-W` and `-X` change nothing |
| `-C` with `i`, `o` or `D` | sets the default layers for every `open()` | for the main program's `open()` calls only, not a module's |
| `-C` on the `#!` line | `Too late` unless the command line has it too | applied |
| `-C0` and `@ARGV` | arguments stay bytes unless `-CA` | arguments arrive decoded from UTF-8, and `-C0` cannot undo that |
| `PERL5OPT` | its switches are added | not read |
| a switch error | exit status from `errno`, usually 25 | always 25 |
| error messages | | the text may differ; the failing place is the same. An uncaught `die` in a run with switches on the command line or with `-e` prints one extra line first, `While evaluating the form ...` |
| `-c` | a syntax check | runs the compile phase and says `syntax OK`, but PCL assumes the program is valid Perl: a statement it cannot compile is reported on STDERR and the verdict is still `syntax OK`. |
| `-v`, `-V`, `-h` | perl's texts and `%Config` | `pcl`'s version, PCL's own `%Config`, `pcl`'s usage |
| no program, STDIN a terminal | waits for the program | prints the usage |
| `#!` without `perl` | runs that interpreter | compiles the file as Perl |
| start-up (measured 2026-10-04) | 2 ms for `perl -e` | 181 ms for `pcl -e`, 41 ms for a cached one-line script; see below |

### What a run costs

A script you run as `pcl script.pl` is cached: its first run after an edit
compiles it, and every later run starts from the cache. A script whose own
`#!` line carries switches is cached the same way, because the switches are
part of the file.

A run with a switch on the command line that changes the program (`-e -E
-n -p -l -0 -g -a -F -i -s -x -M -m -w -C -T -t -c`) is not cached: the
cache entry's key does not carry the switches, so the program is compiled
every time. Measured 2026-10-04 on a 16-core Linux machine (median of five
runs): a cached one-line script took 41 ms, `pcl -e` 181 ms, `pcl -l
script.pl` 184 ms and `pcl -lane ... file` 191 ms; `perl -e` took 2 ms. A
`-I` alone does not stop the cache; the script gets one entry per `-I`
list.

### pcl's own options

They come before the program.

| option | meaning |
|---|---|
| `--verbose` | print the `sbcl` command line `pcl` runs (`-v` is perl's version switch) |
| `--check` | run the program under perl and under PCL and compare the output ([`pcl-check.md`](pcl-check.md)); `--check-stdin FILE` gives both runs FILE as STDIN, `--check-keep DIR` keeps the captured output in DIR. Both sides get the same switches |
| `--version` | print the PCL, cache-generation, SBCL and PPI versions |
| `--cache-info` | where the cache is, what is in it, which core this run would use, and the compile policy in effect: the one diagnostic for "PCL did not notice my change" |
| `--no-cache` | this run reads and writes no module, script or eval cache: the quick answer to "is it the cache?" |
| `--make-core` | build the cached runtime core now, then exit (every run builds one on first use anyway) |
| `--clear-cache` | remove everything PCL made under the cache directory (cached modules, scripts and evals, compiled extensions, prototype facts, saved cores), then exit; XS artifacts are left alone |
| `--help` | the full text, including the environment variables |

The runtime core that every run needs is a cache of its own, built once
per runtime change. The first run after such a change, a one-liner
included, takes a few seconds longer, and on a terminal it says so on
STDERR (`PCL: compiling the runtime into a cached core ...`).

Examples:

```bash
pcl script.pl arg1 arg2
pcl -I lib script.pl
pcl -pi.bak -e 's/foo/bar/' notes.txt
pcl -F, -lane 'print $F[1]' data.csv
pcl -0777 -ne 'print scalar(() = /\bperl\b/g), "\n"' notes.txt
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
| `--as NAME` | compile the file AS the program NAME: `$0`, `__FILE__` and `die`/`warn` locations say NAME (what `pcl -e` and `pcl -M` use) |

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
compiled program loads. Loading it from source compiles it, which is slow, so
PCL keeps it compiled in a **saved SBCL core**:

* **In a checkout**, the first run builds the core under
  `~/.pcl-cache/core/`, and every later run starts from it. Measured
  2026-10-04 on a cached one-line script (median of five): 3.3 seconds
  with `PCL_NO_CORE=1`, 38 ms from the core. The core's file name is a hash
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

A `use` or `require` is resolved through `@INC`, in perl's order: the `-I`
directories, then `PERL5LIB`, then PCL's own `lib/` (its replacements for
modules perl implements in C), then perl's own library directories.  As in
perl 5.26 and later, neither the current directory nor the script's own
directory is on it: a module beside your script is found through `use lib`,
`FindBin` or `-I`, as under perl.  A few of PCL's replacements are found
BEFORE `@INC` is searched, because perl's own copy cannot run under PCL
(`List::Util`, `POSIX`, `Carp` and the others listed in
[`shipped-modules.md`](shipped-modules.md)): a `PERL5LIB` that holds perl's
real `List/Util.pm`, as a local::lib often does, does not break a program.
`use lib` is perl's own `lib.pm`, so it removes duplicates as perl does.
The module's source is compiled the same way as
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
