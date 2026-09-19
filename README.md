# Percolisp (PCL): Perl compiled to native code

[![CI](https://github.com/Percolisp/pcl/actions/workflows/ci.yml/badge.svg)](https://github.com/Percolisp/pcl/actions/workflows/ci.yml)

Percolisp/PCL compiles Perl to Common Lisp (using the
[SBCL](https://www.sbcl.org/) compiler), working towards CPAN
compatibility.

The second main target is to make a compiler toolkit for Perl, with a
documented IR (Intermediate Representation). The compiler records
facts about every variable and call site. The IR, its facts and its
coercion rules are [specified](docs/ir-spec.md) and here is the
information about the IR
[architecture](docs/v2-target-architecture.md).

All the development files used by the AI for making Percolisp will be
in this repo. It is licensed the same as Perl.

Here is an example of compiled Perl:

```perl
use feature 'say';
my $n   = shift // 1000;
my $sum = 0;
for my $i (1 .. $n) { $sum += $i * $i }
say $sum;
```

This get compiled to:


```lisp
(p-let (($n :box (make-p-box nil)))
  (p-my-= $n (p-// (p-shift @ARGV) 1000))
  (p-let (($sum :scalar 0))
    (p-foreach-range-raw ($i 1 $n) :my t (p-incf-raw $sum (p-* $i $i)))
    (p-say $sum)))
```

`p-let` declares the variable, so the `my` was split in two. Note
that variable names are kept and that there are type declarations for
variables.

Everything with a `p-` prefix is a runtime function (/macro) named
after the Perl operator it implements. `p-incf-raw` use direct changes
of the variable (no risk of references or overloads etc).

See the documentation and test suite for more.

## What works

It is easier to list what doesn't work:

* **XS modules.**  Anything with compiled C fails. It is on the todo list,
  hopefully it will work.
* **`@_` aliasing.**  Arguments are copies; `$_[0] = 42` inside a sub does
  not change the caller's variable.  Plain lexical arguments are fine.
* **`DESTROY` is never called.**  Memory is reclaimed by the Lisp garbage
  collector, so there is no scope-exit destructor; code that relies on one
  for cleanup (guard objects, temporary files) does not get it.
* **`tie` on an array, hash or filehandle** aren't supported, for now
  (scalar ties work).
* **`format`/`write`**, **regex code blocks** `(?{ })`, **perl 5.38
  `class`/`field`/`method`**, **`given`/`when`**, **taint mode**: refused
  with a message.
* **Error message text** differs. There is no ambition to look like
  perl's errors, though errors happen in the same places and
  `die`/`$@` behave the same. `use warnings` is a no-op.

[`docs/not-supported.md`](docs/not-supported.md) has the complete list.

### Measured

A large part of the failures are from unsupported features (lack of
XS, error messages are different, `caller()` returns too few
parameters, etc). The remaining test failures are the todo list. :-)

| measurement | result | reproduce |
|---|---|---|
| PCL's own regression suite | **250 files, 8,481 assertions, all passing** | `tools/prove-core` |
| perl's test suite, extracted (108 files from perl 5.40's `t/`) | **18,687 pass / 674 fail** (96.5 %); 60 files pass completely | `perl tools/sweep-perl-tests.pl --jobs 8` |
| perl's whole `t/` tree, run in place (528 files) | 107 files identical to perl; 105 differ for a registered, explained reason; 258 differ and are the bug queue; the remaining 58 do not compile, time out, are too slow for the quick run or produce no test output | `tools/run-perl-suite.pl --all --quick --jobs 4` |
| a board of 14 pure-Perl CPAN distributions, 183 test files | **85 files pass, 48 pass partially, 50 fail** (2,274 assertions pass / 338 fail); "fail" also counts the seven files perl itself skips | `tools/cpan-scoreboard.pl` |
| statements the compiler cannot translate, over all of the above | **62 statements in 19 files**, each with a filed cause (mostly unsupported, like `:lvalue` subs) | `tools/drop-census.pl` |

The first two rows were re-measured on 2026-09-19.  The CPAN board was measured on 2026-09-18 and its four Moo-family distributions again on 2026-09-19 (one file went from partial to pass; the other 64 read the same); the remaining rows are from 2026-09-18.

Every failing assertion is recorded row by row in a baseline that the test
runner compares against, so a change that breaks something previously
passing fails the run. The numbers can only move honestly.

See [`docs/STATUS.md`](docs/STATUS.md) for more details.

### Speed

These are microbenchmarks for different Perl features, measured
2026-09-18 on a quiet machine (best of five runs, startup time
subtracted for both). A ratio below 1.00× means PCL is faster. The
linked page has the full board of 37 rows, including the ones where
PCL is still slower that are not shown here (file I/O about 3.4×,
`s///e` 3.5×, a text-processing loop 2.7×).

| benchmark | what it measures | PCL / perl |
|---|---|---:|
| collatz | `while` loop with integer arithmetic | 0.18× |
| cfor | C-style `for` loop summing integers | 0.26× |
| arith | `$s = ($s * 3 + int($i / 7)) % 1000003` | 0.27× |
| useint | `$s = ($s * 3 + $i / 7) % 1000003` under `use integer` | 0.28× |
| fib(27) | recursion | 0.29× |
| intloop= | `for (1..$n) { $s = $s + $_ }` | 0.30× |
| feread | read-only `foreach` over a 1000-element array | 0.30× |
| intloop+= | `for (1..$n) { $s += $_ }` | 0.31× |
| feread2 | `foreach` over two arrays at once | 0.31× |
| listcopy | `my @copy = @src`, 50 elements | 0.35× |
| symref | symbolic references, `${'main::g'}` | 0.52× |
| gcdrec | recursion with modulo | 0.54× |
| arrfill | `@a = (1..20, $_)` on every iteration | 0.59× |
| arrhash | one array element and one hash element, read and written | 0.64× |
| strcat | `$s .= 'x'`, twenty million times | 0.85× |
| methret | a method call on a blessed hash, `$o->bump` | 1.05× |
| sliceasgn | assignment to array and hash slices | 1.11× |
| regexg | `while ($x =~ /./g)` over a 200 kB string | 1.25× |
| slices | reading `@a[1..5]` and `@h{@k}` | 1.67× |
| ovlsub | `use overload` arithmetic and stringification on objects | 3.37× |
| moo-objs | Moo objects: constructor, accessors, a method building another object | 28× |
| pack | `pack` with two templates | 1035× |
| packunpk | `pack` followed by `unpack` | 1080× |

Numeric loops and recursion are fast. When the compiler can prove a
variable holds a machine integer for its whole life, the generated
code uses native arithmetic instead of perl's generic scalar. Such a
variable can't have a reference taken, get assigned to a string and no
string `eval` can reach it. If possible, a read-only `foreach` bind
array slots directly instead of copying each element.

Reading and writing single array or hash elements, copying a whole
array, filling one from a range, and a read-only `foreach` over one or
several arrays are faster. The compiler proves which arrays are never
written or aliased inside a loop and binds their storage
directly. (Moving several elements at once through slices is still
slower, reading at 1.67× and writing at 1.11×: PCL's per-element
checks cost more than perl's flat C arrays on bulk work.)

Method calls are level with perl, but overloading and regex matching
are slower because nothing can be proved about them when
compiling. The Moo workload is dominated by loading and compiling the
code Moo builds with string `eval`, which is why it is slow. An
overloaded operator calls a Perl sub per operation, and perl's C
implementation of that path is still faster than PCL's. `m//g` in a
loop runs a regex engine written in Lisp
([cl-ppcre](https://edicl.github.io/cl-ppcre/)) instead of perl's
hand-tuned C one.

`pack`/`unpack` is a thousand times slower and will be redone after XS
works.  (PCL's `pack` is itself Perl, compiled by PCL and kept as a
correctness oracle.)

The full table over time, and the measurements behind each optimization,
are in [`docs/faster-codegen-suggestions.md`](docs/faster-codegen-suggestions.md).


## Quick start

* The compiler uses perl, with Moo and PPI.
* Running PCL programs needs both SBCL and perl. Perl is used when a
  string `eval` is compiled to CL from Perl. (If/when XS is done, then
  Percolisp could theoretically be compiled to CL.)
* There is no standalone binary (yet). `pl2cl --executable` saves an
  image that runs the program, but it still need to load modules
  loaded with `require` or from an `eval`.

Minimum perl version is 5.20. The SBCL needs a later version than most
Linux distributions ship, the version to install depends on your
glibc. From CPAN, it is enough with PPI and Moo.

These commands run in a fresh `ubuntu:24.04` container:

```bash
sudo apt-get update
sudo apt-get install -qy perl cpanminus make gcc curl ca-certificates bzip2 git
sudo cpanm --notest PPI Moo

curl -fsSL -o /tmp/sbcl.tar.bz2 \
  https://downloads.sourceforge.net/project/sbcl/sbcl/2.6.0/sbcl-2.6.0-x86-64-linux-binary.tar.bz2
tar -xjf /tmp/sbcl.tar.bz2 -C /tmp
( cd /tmp/sbcl-2.6.0-x86-64-linux && sudo sh install.sh )

git clone https://github.com/Percolisp/pcl.git
cd pcl
./pcl -E 'my @a = (1..5); say join ",", map { $_ * 2 } @a'
```

```console
2,4,6,8,10
```

On Ubuntu 22.04 and Debian 12, put `2.5.2` where those lines say
`2.6.0`: the current SBCL binary needs a newer glibc.

The only Lisp library used is the regex engine
([cl-ppcre](https://edicl.github.io/cl-ppcre/)), which is packed with
PCL.

On the first run, the runtime library is compiled and cached in
`~/.pcl-cache/`.

To put the commands on your `PATH`, install a copy:

```bash
tools/install-pcl --prefix ~/.local     # copies the tree, builds the core, self-tests
```

That installs `pcl`, `pl2cl` and `runpcl` into `~/.local/bin`. It prints the
`export PATH=...` line to paste if that directory is not already on your
`PATH`, and it never edits a startup file for you. `tools/install-pcl
--uninstall --prefix ~/.local` removes them again. Your compiled-module
cache is separate from an installation and stays; `pcl --clear-cache`
empties that. `PCL_ROOT` overrides where an installed command looks for its
runtime tree, if you ever repackage the layout.

## Run in a container

```bash
docker run --rm ghcr.io/percolisp/pcl -E 'say 6*7'          # podman is identical
docker run --rm -v "$PWD":/work ghcr.io/percolisp/pcl script.pl
```

The image is built from the repository's [`Dockerfile`](Dockerfile) by CI
when a release is tagged, using the same installer as everything above, so
it is the same PCL. Your working directory is mounted at `/work`, which is
where the container starts, so a relative path to your script works as
expected. `--entrypoint pl2cl` runs the compiler instead and prints the
Lisp.

**It is not published yet.** :-) The workflow exists and has been run
locally, but the first push to `ghcr.io` happens with the next release
tag, so `docker run` will not find an image until then. Building it
yourself works today: `docker build -t percolisp/pcl .` in a checkout.

## Using PCL

In the shell, use `pcl` instead of `perl`:

```bash
pcl script.pl arg1 arg2         # run a script; @ARGV as usual
pcl -e 'print 1 + 2, "\n"'      # inline code (-E is the same, with `say` enabled)
pcl -MList::Util=sum -E 'say sum 1 .. 10'
pcl -I lib script.pl            # extra @INC directory
pcl -c script.pl                # compile only, then "syntax OK"
```

`pl2cl` is the compiler. It reads Perl and writes Common Lisp:

```bash
pl2cl script.pl > script.lisp   # or from stdin: pl2cl < script.pl
sbcl --noinform --non-interactive \
     --load cl/pcl-runtime.lisp --load script.lisp    # run the output by hand
```

`runpcl script.pl` compiles and runs one file with no options. Mostly
for the test suite.

**Modules.** A `use` or `require` is resolved through `@INC`, the same
directories perl would search, and the module's source is compiled the
same way as your program. So `cpanm Data::Dump` followed by `pcl
-MData::Dump=dump -E 'say dump [1 .. 3]'` just works: PCL finds
`Data/Dump.pm` and compiles it. Compiled modules are cached, so only
the first run is slow.

PCL ships pure-Perl replacements for 22 common C/XS modules:
[`lib/`](lib): `List::Util`, `Scalar::Util`, `POSIX`, `Cwd`, `Fcntl`,
`Socket`, `IO::Handle` and others.

**Unsupported.** A statement that cannot be compiled is reported on
stderr at compile time. If the statement is executed a normal Perl
exception is thrown, which `eval` can catch. A construct that is
deliberately unsupported dies the same way, naming its entry in
[`docs/not-supported.md`](docs/not-supported.md) in the message.

**Caches.** PCL caches three types of compiled binaries. A saved SBCL
core with the PCL runtime (a tenth of the startup time), modules
loaded with `use`, and the script you run. All three are stored in
`~/.pcl-cache/`. Change
the directory with `PCL_CACHE_DIR` and clear with `pcl
--clear-cache`. The directory is created `0700`. (Compiled `eval`
statements are also cached, it was needed for e.g. Moo.)

The cache's file name of the runtime core is a hash of the source plus
the SBCL version. A module's cache entry is indexed/named from the
module's path, the compiler generation and a compiler fingerprint.
The fingerprint covers PCL's own compiler files, the `perl` binary and
PPI's files, so a perl or PPI upgrade re-makes every entry. A script's
entry is named the same way, plus the `-I` directories it was run with.

So a cached module or script is re-transpiled when its own file
changes. It is also re-transpiled when modules it depends on changes
(if constants etc are declared in dependencies, the generated code
might change) — and when one of those `use`d names starts resolving to
a *different file*, which changes the parse in the same way: you added
a module earlier on the search path, or changed the search path itself.
Entries are removed after 30 days without use, so they
get recompiled if used again. A saved core is kept as long as the
runtime it was built from is still there.

`pcl --cache-info` says where the cache is and what is in it, and `pcl
--no-cache` runs once without it.

The script you run is cached the same way (since 2026-09-17), and a
newly installed or updated perl simply re-makes the entries. `pcl -e`
one-liners are not cached. (The runtime core is a separate cache that
every run needs, built once per runtime change; that is what the
message `compiling the runtime into a cached core` is about, and a
one-liner can trigger it too.)

**The knobs.** `PCL_COMPILE_DIRS` and `PCL_NO_COMPILE_DIRS` take
colon-separated directories, in `PERL5LIB` syntax, and say which modules are
compiled to native code. By default that is perl's installed library
directories plus PCL's own `lib/`, so a module you are *editing* is cached
as readable text.

`PCL_OPT=none` turns off every speed optimization and compiles the
fully generic form. A script is compiled on its first run after an
edit: a one-liner starts in under a quarter of a second, and a
thousand-line script takes a few seconds. From the cache both start in
about 0.04 seconds.

The full list of knobs is in [`docs/caching.md`](docs/caching.md) and
in `pcl --help`. Every command and flag is on one page in
[`docs/pcl-commands.md`](docs/pcl-commands.md).

**Detecting PCL from Perl code.** `$ENV{_PCL_RUNTIME_}` is true in every PCL
process. Its value is the version that `pcl --version` prints:

```perl
if ($ENV{_PCL_RUNTIME_}) { ... }              # running under PCL
print "PCL $ENV{_PCL_RUNTIME_}\n";            # e.g. PCL 0.1.0
```

`$^V` and `$]` report 5.30.0 on purpose, for compatibility. `$^X`
deliberately points at *real perl*, so that a subprocess you spawn
runs perl. `$^O` is the operating system.

`_PCL_RUNTIME_` is in `%ENV`, but *not* in the process environment,
**a child process does not inherit it**. Since `$^X` is real perl, a
perl child can't wrongly assume it is running under PCL.

## An example

This program uses the things a typical script uses: a package with
signatures, a hash, `sort`, list utilities, `eval` in both forms, and a
heredoc. Under perl and under PCL it prints the same six lines.

```perl
use strict;
use warnings;
use feature qw(say signatures);
use List::Util qw(sum max);

package Counter {
    sub new ($class, %args) { bless { count => 0, step => $args{step} // 1 }, $class }
    sub tick ($self)        { $self->{count} += $self->{step}; $self }
    sub count ($self)       { $self->{count} }
}

my %seen;
my @words = map { lc } grep { /\w/ } split /\W+/, <<'TEXT';
The quick brown fox jumps over the lazy dog. The dog sleeps; the fox does not.
TEXT
$seen{$_}++ for @words;

my @top = sort { $seen{$b} <=> $seen{$a} || $a cmp $b } keys %seen;
say "$_: $seen{$_}" for @top[0 .. 2];

my $c = Counter->new(step => 3);
$c->tick->tick;
say "count=", $c->count, " words=", scalar @words, " longest=", max(map { length } @words);

my $total = eval { sum(map { $_ * $_ } 1 .. 10) } // "error: $@";
say "sum of squares: $total";

my $code = 'my $x = 6; $x * 7';
say "eval: ", eval $code;
```

```console
$ pcl demo.pl
the: 4
dog: 2
fox: 2
count=6 words=16 longest=6
sum of squares: 385
eval: 42
```

To see what the compiler produces for a whole program, run `pl2cl demo.pl`.
The output is one Lisp form per Perl statement, in source order, with the
same shape as the five-line example at the top of this page. The loop there
is `p-foreach-range-raw`, the counting-loop form that keeps `$i` as a raw
integer for the whole loop, because nothing in the body can alias it. A
`print` of a string with an escape comes out as `(p-print (p-string-concat
$sum (p-esc "\\n")))`, with the escape kept as data. The
[IR manual](docs/ir-spec.md) documents every form.

## How it works

**The compiler** (`Pl/`) reads source with
[PPI](https://metacpan.org/pod/PPI), the CPAN Perl parser, and builds
a tree of statements and expressions. It works out how variables are
used: is a reference ever taken? Is it captured by a closure? Is it
only ever a number? Then it writes out one Lisp form per Perl
statement.

```
Perl source → PPI → Pl::Parser2 (statements) → Pl::CLForm → Common Lisp text
                        ↓                ↑                          ↓
              Pl::VarAnnotator    Pl::PExpr → Pl::ExprToCL      cl/pcl-runtime.lisp
             (scopes, captures)   (expression AST → forms)     (Perl semantics in Lisp)
```

**The runtime**, in [`cl/pcl-runtime.lisp`](cl/pcl-runtime.lisp), is about
14,500 lines of Common Lisp, not counting blank lines, comments and
docstrings. It is a library of the Perl operations: what `+` does to `3` or
to `"3 apples"`, how `local` restores a value on scope exit, how a method
call finds its target, how `sort` calls its comparator. The compiled program
is mostly calls into this library, and it is where Perl's semantics are
pinned down. Common Lisp already provides the underpinnings Perl needs:
dynamic typing, closures, dynamic binding for `local`, non-local exits for
`die`/`last`/`return`, and garbage collection. So the runtime uses those
directly, instead of rebuilding them.

**Scalars are "boxes" small data structures, unless proved
otherwise.** A Perl scalar can be aliased by `foreach`, referenced
with `\$x`, localized, or tied. So by default PCL represents each
variable as a small mutable container, a *box*, and passes the box
around where Perl would pass the variable. That is the cost that slows
the compiled code.

The compiler's analysis exists to find the variables that never need a
box, such as a counter, an accumulator, or a loop's read-only element,
and give them a plain slot instead. Every such decision is a named,
switchable optimization, controlled by `PCL_OPT`. The general form
must produce the same output, and the test suite checks that it does.

**Running.** `pcl` compiles the script into a cache entry (a temporary Lisp
file for `-e`), and starts SBCL from a saved memory image that already
contains the compiled runtime. That image is built on first use, cached, and
keyed on the runtime's source. Startup from the cache is about 0.04 seconds;
the first run after an edit adds the time to compile your script.

## Requirements

PCL implements the semantics of perl 5.40, which is where its test
suites come from. It is developed and tested on Linux. Other Unix
systems should work, but aren't tested. Running a compiled program
needs SBCL. A program that uses string `eval` compiles code while it
runs, so it needs both perl and SBCL.

* **Perl 5.20 or later**, with [PPI](https://metacpan.org/pod/PPI) 1.291 or
  later and [Moo](https://metacpan.org/pod/Moo): `cpanm PPI Moo`. Nothing
  else is needed beyond core modules. Distributions package an older PPI
  (Ubuntu 24.04 has 1.277), and the installer refuses it, because PCL's
  handling of PPI's token stream is tied to 1.291.
* **SBCL 2.5.2 or later.** Debian 12, Ubuntu 22.04 and Ubuntu 24.04 all ship
  an older version. A binary from
  [sbcl.org](https://www.sbcl.org/platform-table.html) installs in a
  minute: either into `/usr/local` with `sudo sh install.sh`, which is what
  [Quick start](#quick-start) does, or into your home with
  `INSTALL_ROOT=$HOME/sbcl sh install.sh` and no root at all. Which one to
  install depends on your glibc. The current 2.6.0 binary needs glibc 2.38,
  which Ubuntu 24.04 and Debian 13 have. Ubuntu 22.04 and Debian 12 do not,
  and need the 2.5.2 binary instead. Both combinations are installed and
  tested by the [install matrix](.github/workflows/install-matrix.yml).

  | distribution | Install SBCL binary |
  |---|---|
  | Ubuntu 22.04, Debian 12 | 2.5.2 |
  | Ubuntu 24.04, Debian 13 and newer | 2.6.0 (current) |
* **No Lisp libraries to install.** The one PCL needs,
  [cl-ppcre](https://edicl.github.io/cl-ppcre/), the regex engine, is
  carried in the tree under `cl/vendor/cl-ppcre/` as upstream source, and
  the runtime finds it there. If you would rather use your own copy, such as
  a distribution package or Quicklisp, remove that directory and PCL falls
  back to whatever ASDF can find.

## Roadmap

* **v0.2:** fix the remaining incompatibilities, by working through
  perl's `t/` tree and the CPAN board.
* **Minor features:** like the standalone binary, `pl2cl --executable`
  already saves an image that runs the program, compiling `use`d
  modules at build time as perl's does, but the module closure and the
  pack/mro/warnings extensions are not embedded yet.
* **Next target:** will probably be a second compiler (JavaScript?),
  probably just a beta version. This to verify that the IR really
  works. A second target will also be a reality check when trying to
  implement XS with pclxs after.
* **Planned, not rejected:** items, listed in
  [`docs/not-supported.md`](docs/not-supported.md). Like live
  symbol-table hashes (`%Foo::`), full `caller()` fidelity, perl 5.38
  classes, `tie` on aggregates, `format`, indirect object syntax with
  a scalar invocant, and a `use warnings` model.

## Documentation

| | |
|---|---|
| [`docs/pcl-commands.md`](docs/pcl-commands.md) | the command reference: `pcl`, `pl2cl`, `runpcl`, the installer, how the runtime, modules and scripts are compiled and cached |
| [`docs/STATUS.md`](docs/STATUS.md) | what runs, measured, with failure breakdowns |
| [`docs/not-supported.md`](docs/not-supported.md) | what does not, and why |
| [`docs/ir-spec.md`](docs/ir-spec.md) | what every form in the generated Lisp means |
| [`docs/faster-codegen-suggestions.md`](docs/faster-codegen-suggestions.md) | the benchmark board and the measurement behind each optimization |
| [`docs/shipped-modules.md`](docs/shipped-modules.md) | how `use Module` finds PCL's pure-Perl replacements |
| [`docs/extensions.md`](docs/extensions.md) | the three runtime parts written in Perl, and how they load |
| [`CHANGELOG.md`](CHANGELOG.md) | what changed since v0.1.0 |
| [`docs/`](docs) | about 150 design notes and measurements |

## Contributing

Issues and pull requests are welcome at
<https://github.com/Percolisp/pcl>. [`CONTRIBUTING.md`](CONTRIBUTING.md)
says what a useful bug report carries; the short answer is one small
program, run by `perl` and by `pcl` side by side. `tools/prove-core` runs
the test suite and must stay green. CI runs the same suite on a clean
Ubuntu machine. [`CLAUDE.md`](CLAUDE.md) records the working rules the
project follows. It is written as instructions for the AI sessions that do
much of the development, so it reads dense, but it is an honest account of
how changes are made and verified here.

## Background

PCL was largely written with Claude Code. My own Common Lisp use is
from long ago, so that side is essentially all Claude's.

An anecdote is that Differential fuzzing found real bugs cheaply (I am
not really a compiler guy, Claude showed me). Also, `pack` was hard to
get right in Lisp, it was easier to write it in Perl and letting PCL
compile it.

PCL will go on CPAN once it is closer to ready.

## License

Free software, under the same terms as Perl itself: at your option the
Artistic License 1.0 or the GNU GPL v1 or later. [`LICENSE`](LICENSE) is the
statement; the two texts ship beside it, copied verbatim from a perl source
distribution, as [`LICENSE-Artistic`](LICENSE-Artistic) and
[`LICENSE-GPL`](LICENSE-GPL).

`cl/vendor/` holds third-party source carried verbatim and is not covered by
that: today it is cl-ppcre, under its own BSD 2-clause licence
(`cl/vendor/cl-ppcre/LICENSE`).
