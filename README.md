# Percolisp (PCL) — Perl 5 compiled to native code

[![CI](https://github.com/Percolisp/pcl/actions/workflows/ci.yml/badge.svg)](https://github.com/Percolisp/pcl/actions/workflows/ci.yml)

Percolisp compiles Perl 5 to native code by way of [SBCL](https://www.sbcl.org/),
a Common Lisp compiler.  Your script runs unchanged.  Numeric loops run **three
to five times faster than perl**; whole programs are still slower (1.9× and
3.4× on the two measured); and **84 of 183 test files** from a board of
pure-Perl CPAN distributions pass exactly as their authors wrote them.  Every
number on this page is [measured](#measured) by a command you can run.  The
command is `pcl`.

**Percolisp is an experiment until the critical infrastructure runs.**  The
infrastructure is the modules a Perl deployment cannot do without — pure Perl:
Getopt::Long, JSON::PP, Data::Dumper, Path::Tiny, HTTP::Tiny, Try::Tiny, Moo,
Test::More; XS, through the [pclxs](#what-works) bridge: DBI with one DBD,
Encode, Storable, Time::HiRes, Digest::SHA, JSON::XS, POSIX, Socket;
frameworks that stress everything: Plack, Mojolicious, DateTime, Template
Toolkit, Log::Log4perl.  Each is being measured the same way: does it compile,
does its own test suite pass, what blocks it.  None is declared done yet.  The
experiment ends when every module on that list runs its own tests under
Percolisp; the count that do is the progress meter and will be kept here.

Five lines of Perl, and what the compiler makes of them:

```perl
use feature 'say';
my $n   = shift // 1000;
my $sum = 0;
for my $i (1 .. $n) { $sum += $i * $i }
say $sum;
```

```lisp
(p-let (($n :box (make-p-box nil)))
  (p-my-= $n (p-// (p-shift @ARGV) 1000))
  (p-let (($sum :scalar 0))
    (p-foreach-range-raw ($i 1 $n) :my t (p-incf-raw $sum (p-* $i $i)))
    (p-say $sum)))
```

`p-let` is `my`, and the word after each variable is what the compiler
proved about it.  `$n` is `:box`: it is handed to `shift`, so something could
alias it, and it lives in a small container.  `$sum` is `:scalar`: nothing
ever takes a reference to it, so it is a bare machine integer, and
`p-incf-raw` adds into it directly.  Everything with a `p-` prefix is a
runtime function named after the Perl operator it implements.  Those proofs
are where the speed comes from, and they are written into the output: the
generated Lisp is a documented [intermediate representation](docs/ir-spec.md)
that another tool could compile to another target.

Why Percolisp:

* **Speed where it can be proved.**  A variable the compiler can show is
  only ever a number, or is never referenced, becomes a machine value instead
  of a Perl scalar.  Where nothing can be proved — method calls, overloading,
  regexes — perl's C implementation is still faster.  The [numbers](#speed)
  say which is which.
* **Output you can read.**  The Lisp keeps your variable names, sigils and
  Perl's operator names.
* **A compiler toolkit with a documented IR.**  The compiler is written in
  Perl and records facts about every variable and call site (always a number,
  never referenced, read-only in this loop, calling convention, context).  The
  IR, its facts and its coercion rules are [specified](docs/ir-spec.md); the
  [architecture](docs/v2-target-architecture.md) is documented.

Three plain facts before you install anything:

* Compiling needs perl: the compiler is a Perl program built on PPI.
* Running a compiled program needs SBCL, and it needs perl installed too,
  because string `eval` compiles Perl while the program runs and Percolisp
  does that by calling its own compiler.
* There is no standalone binary yet.  `pl2cl --executable` saves an image
  that runs the program, but it still reads this tree for modules.

**Maturity: early.**  First tag v0.1.0, August 2026.  Most of the language
works, verified against perl's own test suite, and most CPAN modules written
in Perl compile; XS modules go through pclxs, which runs one real module end
to end and is not bundled yet.  [What works](#what-works) has the list and
the failure counts.

## Quick start

You need perl (5.20 or newer) with two CPAN modules, and SBCL.  Both minimum
versions are newer than most Linux distributions ship, so the lines below
install them; [Requirements](#requirements) has the version rules and the
one choice to make (which SBCL binary — it depends on your glibc).

**These commands were run, exactly as they stand, in a fresh `ubuntu:24.04`
container** (2026-09-10, on an image with nothing but `sudo` and an
unprivileged user).  They assume `sudo`; on a root shell, drop it.

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

On Ubuntu 22.04 or Debian 12, put `2.5.2` where those lines say `2.6.0`: the
current SBCL binary needs a newer glibc than those releases have.  Nothing
else changes, and both versions are tested.

There is no Lisp-library step.  PCL carries its regex engine
([cl-ppcre](https://edicl.github.io/cl-ppcre/)) in the tree, so SBCL is the
whole Lisp side.

The first run takes a few seconds longer than the rest: PCL compiles its
runtime library once and caches the result under `~/.pcl-cache/`.

To put the commands on your `PATH`, install a copy:

```bash
tools/install-pcl --prefix ~/.local     # copies the tree, builds the cache, self-tests
```

That installs `pcl`, `pl2cl` and `runpcl` into `~/.local/bin` (it prints the
`export PATH=...` line to paste if that directory is not on your `PATH` yet,
and never edits a startup file for you).  `tools/install-pcl --uninstall
--prefix ~/.local` removes them again; your compiled-module cache is separate
from an installation and stays — `pcl --clear-cache` is what empties that.
`PCL_ROOT` overrides where an installed command looks for its runtime tree, if
you ever repackage the layout.

## Run it in a container

If you would rather install nothing:

```bash
docker run --rm ghcr.io/percolisp/pcl -E 'say 6*7'          # podman is identical
docker run --rm -v "$PWD":/work ghcr.io/percolisp/pcl script.pl
```

The image is built from the repository's [`Dockerfile`](Dockerfile) by CI
when a release is tagged, using the same installer as everything above, so it
is the same PCL.  Your working directory is mounted at `/work`, which is where
the container starts, so a relative path to your script is what you would
expect; `--entrypoint pl2cl` runs the compiler instead and prints the Lisp.

**It is not published yet** — the workflow exists and has been run locally,
but the first push to `ghcr.io` happens with the next release tag, so that
`docker run` will not find an image until then.  Building it yourself works
today: `docker build -t percolisp/pcl .` in a checkout.

## Using PCL

`pcl` is the everyday command.  It works like `perl` for the things most
people use:

```bash
pcl script.pl arg1 arg2         # run a script; @ARGV as usual
pcl -e 'print 1 + 2, "\n"'      # inline code (-E is the same, with `say` enabled)
pcl -MList::Util=sum -E 'say sum 1 .. 10'
pcl -I lib script.pl            # extra @INC directory
pcl -c script.pl                # compile only, then "syntax OK"
```

`pl2cl` is the compiler by itself.  It reads Perl and writes Common Lisp,
which is the way to see what PCL makes of your code:

```bash
pl2cl script.pl > script.lisp   # or from stdin: pl2cl < script.pl
sbcl --noinform --non-interactive \
     --load cl/pcl-runtime.lisp --load script.lisp    # run the output by hand
```

`runpcl script.pl` compiles and runs one file with no options; the test
suite uses it, and it is handy for quick experiments.

**Modules.**  A `use` or `require` is resolved through `@INC`, the same
directories perl would search, and the module's source is compiled the
same way as your program.  So `cpanm Data::Dump` followed by
`pcl -MData::Dump=dump -E 'say dump [1 .. 3]'` just works: PCL finds
`Data/Dump.pm` and compiles it.  Compiled modules are cached, so only the
first run pays for them.  Where a common module is implemented in C, PCL
ships a pure-Perl replacement in [`lib/`](lib) — `List::Util`,
`Scalar::Util`, `POSIX`, `Cwd`, `Fcntl`, `Socket`, `IO::Handle` and others,
22 modules in all — and uses it automatically.

**When something is not supported.**  PCL does not reject or quietly skip
Perl it cannot handle.  A statement it cannot compile is reported on
stderr at compile time and, if the program reaches it, dies there as an
ordinary Perl exception — one that `eval` can catch.  A construct that is
deliberately unsupported dies in the same way, with a message naming the
entry in [`docs/not-supported.md`](docs/not-supported.md).  The compiler
never substitutes a guess for a statement it cannot translate.

**Caches.**  PCL caches two things, both purely for speed: a saved SBCL
core with the PCL runtime already compiled in (startup ~1 s → ~0.1 s), and,
for each module you `use`, its transpiled Common Lisp plus a compiled
`.fasl`.  Both live under `~/.pcl-cache/`, which `PCL_CACHE_DIR` moves and
`pcl --clear-cache` empties.  A cached module is code, so the directory is
created `0700` and PCL refuses to load out of one anybody else could write
to.

**When a cached thing is stale.**  The core's *file name* is a hash of the
runtime's source plus the SBCL version, so editing either produces a
different core rather than a stale one.  A cached module is re-transpiled
when its own file changes — and also when any module whose prototypes or
exports its parse read changes, which is what `perl` gets for free by
re-parsing everything on every run.  Entries nothing has used for 30 days
are removed.  If PCL ever seems not to notice a change, `pcl --cache-info`
says where the cache is and what is in it, and `pcl --no-cache` runs once
without it.

**The knobs.**  `PCL_COMPILE_DIRS` and `PCL_NO_COMPILE_DIRS` (colon-separated
directories, `PERL5LIB` syntax) say which modules are compiled to native
code; by default that is perl's installed library directories and PCL's own
`lib/`, so a module you are *editing* is cached as readable text.
`PCL_OPT=none` turns off every speed optimization and compiles the fully
generic form; the output must behave identically, and the test suite checks
that it does.  Your own script is compiled on every run, so a large one pays
a pause before its first line: a one-liner starts in under a quarter of a
second, a thousand-line script takes a few seconds.  The full list is in
[`docs/caching.md`](docs/caching.md) and in `pcl --help`; every command and
flag is on one page in [`docs/pcl-commands.md`](docs/pcl-commands.md).

**Detecting PCL from Perl code.**  `$ENV{_PCL_RUNTIME_}` is true in every PCL
process, and its value is the version `pcl --version` prints:

```perl
if ($ENV{_PCL_RUNTIME_}) { ... }              # running under PCL
print "PCL $ENV{_PCL_RUNTIME_}\n";            # e.g. PCL 0.1.0
```

Nothing else answers the question honestly: `$^V` and `$]` report 5.30.0 on
purpose (compatibility), `$^X` deliberately points at *real perl* so that a
subprocess you spawn runs perl, and `$^O` is the operating system.

The variable is **synthetic** — it is in `%ENV`, in `exists`, in `keys`, in
`each` and in every `%ENV` copy, but it is *not* in the process environment,
so **a child process does not inherit it**.  That is deliberate: since `$^X`
is real perl, a perl child that saw the variable would believe it runs under
PCL.  A PCL child sets its own.  Assigning to it is an ordinary `%ENV` write
(which does export it, as perl's `%ENV` always does), and deleting it removes
it for the rest of the process.

## An example

This program uses the things a typical script uses: a package with
signatures, a hash, `sort`, list utilities, `eval` in both forms, and a
heredoc.  Under perl and under PCL it prints the same six lines.

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

To see what the compiler produces for a whole program, run `pl2cl demo.pl`:
the output is one Lisp form per Perl statement, in source order, with the
same shape as the five-line example at the top of this page.  The loop
there is `p-foreach-range-raw`, the counting-loop form that keeps `$i` as a
raw integer for the whole loop because nothing in the body can alias it; a
`print` of a string with an escape comes out as `(p-print (p-string-concat
$sum (p-esc "\\n")))`, the escape kept as data.  The [IR manual](docs/ir-spec.md)
documents every form.

## What works

Most of the language, verified against perl's own test suite.  All
operators and precedence levels; scalar, list and void context propagated
correctly, including `wantarray`; closures with correct capture, including
per-iteration loop variables; `state`; `local` on scalars, aggregates,
elements and typeglobs; signatures; `try`/`catch`/`finally`; regexes with
named captures, `pos`, `\G`, `tr///`; `sprintf`, `pack`/`unpack`, `sort`
with all comparator forms; heredocs and every quoting construct; `BEGIN`
and the other phase blocks; string `eval`, which sees and can assign the
enclosing lexicals; objects with `@ISA`, C3 method resolution, `SUPER::`,
`AUTOLOAD` and `use overload`; `tie` on scalars; filehandles, pipes,
`open` in its many forms, `fork`, `system`, `%ENV`, `%SIG` handlers.

**What does not work**, in rough order of importance:

* **XS modules.**  Anything with compiled C fails to load: `DBI`,
  `JSON::XS`, `Moose`, and core modules such as `Storable`.  That rules
  out a large part of what people actually `use`.  A separate project,
  pclxs, lets unmodified XS libraries talk to PCL's runtime; one real module
  (`Digest::MD5`) runs end to end through it, but it is not bundled and not
  ready.
* **`@_` aliasing.**  Arguments are copies; `$_[0] = 42` inside a sub does
  not change the caller's variable.  Plain lexical arguments are fine.
* **`DESTROY` is never called.**  Memory is reclaimed by the Lisp garbage
  collector, so there is no scope-exit destructor; code that relies on one
  for cleanup (guard objects, temporary files) does not get it.
* **`tie` on an array, hash or filehandle** is announced and ignored
  (scalar ties work).
* **`format`/`write`**, **regex code blocks** `(?{ })`, **perl 5.38
  `class`/`field`/`method`**, **`given`/`when`**, **taint mode**: refused
  with a message.
* **`use warnings`** produces no diagnostics: PCL emits no warnings at all.
* **Error message text** is not perl's, though errors happen in the same
  places and `die`/`$@` behave the same.

[`docs/not-supported.md`](docs/not-supported.md) is the complete list, with
the reason for each entry and what you will observe instead.

### Measured

Every number below comes from a command you can run; nothing is estimated.
[`docs/STATUS.md`](docs/STATUS.md) has the same figures with the failure
breakdowns.  All were taken on 2026-09-04, except the CPAN board row, re-taken on 2026-09-09 when its failures gained per-row causes.

| measurement | result | reproduce |
|---|---|---|
| PCL's own regression suite | **195 files, 6,729 assertions, all passing** | `tools/prove-core` |
| perl's test suite, extracted (108 files from perl 5.40's `t/`) | **18,581 pass / 649 fail (96.6 %)**; 58 files pass completely | `perl tools/sweep-perl-tests.pl --jobs 8` |
| perl's whole `t/` tree, run in place (528 files) | 92 files identical to perl; 108 differ for a registered, explained reason; 275 differ and are the bug queue; the rest do not compile, time out or produce no test output | `tools/run-perl-suite.pl --all --quick --jobs 4` |
| a board of 14 pure-Perl CPAN distributions, 183 test files | **84 files pass, 50 pass partially, 49 fail** (2,213 assertions pass / 353 fail), every failing assertion with a recorded cause | `tools/cpan-scoreboard.pl` |
| statements the compiler cannot translate, over all of the above | **62 statements in 19 files**, each with a filed cause | `tools/drop-census.pl` |

Every failing assertion is recorded row by row in a baseline that the test
runner compares against, so a change that breaks something previously
passing fails the run.  The numbers can only move honestly.

### Speed

**Two whole programs first**, because "what about a real program" is the
first question.  Ratio is PCL time / perl time, best of five, process
startup subtracted; measured 2026-09-10 on a quiet machine (load 0.7) with
`perl tools/bench-exec.pl json-rt textproc`.

| program | what it does | PCL / perl |
|---|---|---:|
| json-rt | `JSON::PP` encode and decode of a 50 kB nested structure | 1.85× |
| textproc | `Text::Balanced`/`Text::Wrap`-style line processing of a 1 MB string: regexes, `pos`, `substr`, `.=`, `split`, `join` | 3.40× |

Both are slower than perl, and both got faster since they were first
measured at the start of September (json-rt 3.4×, textproc 5.1×, on a busy
machine, best of three); what moved them is on the linked page.

The rest are microbenchmarks: each isolates one Perl feature so that a
difference has one cause.  They are not a promise about whole programs; the
two rows above are.  Same ratio; below 1.00× means PCL is faster.  The
table is the board of
2026-09-08, taken on a quiet machine (§0.2m of the linked page).

| benchmark | what it measures | PCL / perl |
|---|---|---:|
| collatz | `while` loop with integer arithmetic | 0.18× |
| cfor | C-style `for` loop summing integers | 0.24× |
| arith | `$s = ($s * 3 + int($i / 7)) % 1000003` | 0.25× |
| useint | `$s = ($s * 3 + $i / 7) % 1000003` under `use integer` | 0.25× |
| feread | read-only `foreach` over a 1000-element array | 0.29× |
| intloop= | `for (1..$n) { $s = $s + $_ }` | 0.29× |
| fib(27) | recursion | 0.29× |
| feread2 | `foreach` over two arrays at once | 0.30× |
| intloop+= | `for (1..$n) { $s += $_ }` | 0.32× |
| listcopy | `my @copy = @src`, 50 elements | 0.34× |
| symref | symbolic references, `${'main::g'}` | 0.41× |
| gcdrec | recursion with modulo | 0.52× |
| arrfill | `@a = (1..20, $_)` on every iteration | 0.60× |
| arrhash | one array element and one hash element, read and written | 0.63× |
| methret | a method call on a blessed hash, `$o->bump` | 1.06× |
| sliceasgn | assignment to array and hash slices | 1.14× |
| slices | reading `@a[1..5]` and `@h{@k}` | 1.64× |
| regexg | `while ($x =~ /./g)` over a 200 kB string | 2.07× |
| strcat | `$s .= 'x'`, twenty million times | 2.16× |
| ovlsub | `use overload` arithmetic and stringification on objects | 3.33× |
| moo-objs | Moo objects: constructor, accessors, a method building another object | 32.3× |
| pack | `pack` with two templates | 1095× |
| packunpk | `pack` followed by `unpack` | 1015× |

**Numeric loops and recursion beat perl by three to five times.**  When the
compiler can prove a variable holds a machine integer for its whole life —
nothing takes a reference to it, nothing assigns a string to it, no string
`eval` can reach it — the generated code uses native arithmetic instead of
perl's generic scalar.  The same proof lets a read-only `foreach` bind
array slots directly instead of copying each element.

**Aggregate traffic now mostly beats perl.**  Reading and writing single
array or hash elements, copying a whole array, filling one from a range and
a read-only `foreach` over one or several arrays are all faster than perl:
the compiler proves which arrays are never written or aliased inside a loop
and binds their storage directly.  Moving several elements at once through
slices is still slower (reading 1.64×, writing 1.14×): PCL's per-element
checks cost more than perl's flat C arrays on bulk work.

**Method calls are level with perl; overloading and regex matching are
slower, because nothing can be proved about them ahead of time.**  A plain
method call on a blessed hash caches its target per class and now costs
within a few percent of perl's; a Moo workload is dominated by loading and
compiling the code Moo builds with string `eval`, which is where its 32× goes.
An overloaded operator calls a Perl sub per operation, and perl's C
implementation of that path is still faster than PCL's.  `m//g` in a loop runs a regex engine written in Lisp
([cl-ppcre](https://edicl.github.io/cl-ppcre/)) instead of perl's hand-tuned
C one.  Symbolic references used to be in this group; a constant name is now
resolved once per site, and they beat perl.

**`pack`/`unpack` is hundreds of times slower and is a known open item, not
a representative number.**  PCL's `pack` is itself Perl, compiled by PCL and
kept as a correctness oracle; a native fast path is planned.

The full table over time, and the measurements behind each optimization,
are in [`docs/faster-codegen-suggestions.md`](docs/faster-codegen-suggestions.md).

## Requirements

PCL implements the semantics of perl 5.40, which is where its test suites
come from.  It is developed and tested on Linux; other Unix systems should
work but are not tested.  Compiling needs perl; running a compiled program
needs SBCL, and a program that uses string `eval` compiles code while it
runs, so it needs both.

* **Perl 5.20 or later** with [PPI](https://metacpan.org/pod/PPI) 1.291 or
  later and [Moo](https://metacpan.org/pod/Moo): `cpanm PPI Moo`.  Nothing
  else beyond core modules.  Distributions package an older PPI (Ubuntu
  24.04 has 1.277) and the installer refuses it, because PCL's handling of
  PPI's token stream is tied to 1.291.
* **SBCL 2.5.2 or later.** Debian 12, Ubuntu 22.04 and Ubuntu 24.04
  all ship with an older version, but a binary from
  [sbcl.org](https://www.sbcl.org/platform-table.html) installs in a
  minute — into `/usr/local` with `sudo sh install.sh` (what [Quick
  start](#quick-start) does), or into your home with
  `INSTALL_ROOT=$HOME/sbcl sh install.sh` and no root at all.  Which one
  to install depends on your glibc: the
  current 2.6.0 binary needs glibc 2.38, which Ubuntu 24.04 and Debian
  13 have; Ubuntu 22.04 and Debian 12 do not, and need the 2.5.2
  binary.  Both combinations are installed and tested by the [install
  matrix](.github/workflows/install-matrix.yml).

  | distribution | Install SBCL binary |
  |---|---|
  | Ubuntu 22.04, Debian 12 | 2.5.2 |
  | Ubuntu 24.04, Debian 13 and newer | 2.6.0 (current) |
* **No Lisp libraries to install.**  The one PCL needs,
  [cl-ppcre](https://edicl.github.io/cl-ppcre/) (the regex engine), is
  carried in the tree under `cl/vendor/cl-ppcre/` as upstream source, and
  the runtime finds it there.  If you would rather use your own copy —
  a distribution package, or Quicklisp — remove that directory and PCL
  falls back to whatever ASDF can find.

## How it works

An introduction assuming no compiler background at all.

**The compiler** (`Pl/`) reads your source with
[PPI](https://metacpan.org/pod/PPI), the CPAN Perl parser, builds a
tree of statements and expressions, works out for every variable how
it is used (is a reference ever taken? is it captured by a closure? is
it only ever a number?), and writes out one Lisp form per Perl
statement.

```
Perl source → PPI → Pl::Parser2 (statements) → Pl::CLForm → Common Lisp text
                        ↓                ↑                          ↓
              Pl::VarAnnotator    Pl::PExpr → Pl::ExprToCL      cl/pcl-runtime.lisp
             (scopes, captures)   (expression AST → forms)     (Perl semantics in Lisp)
```

**The runtime** ([`cl/pcl-runtime.lisp`](cl/pcl-runtime.lisp), about
13,000 lines of Common Lisp, not counting blank lines, comments and
docstrings) is a library of the Perl operations: what `+` does to `3`
(or `"3 apples"`), how
`local` restores a value on scope exit, how a method call finds its
target, how `sort` calls its comparator.  The compiled program is
mostly calls into this library, and it is where Perl's semantics are
pinned down.  Common Lisp already provides the underpinnings Perl
needs — dynamic typing, closures, dynamic binding for `local`,
non-local exits for `die`/`last`/`return`, garbage collection — so the
runtime uses those directly instead of rebuilding them.

**Scalars are boxes, unless proved otherwise.**  A Perl scalar can be
aliased (by `foreach`), referenced (`\$x`), localized or tied, so by
default PCL represents each variable as a small mutable container, a *box*,
and passes the box around where Perl would pass the variable.  That is
correct and general, and it is the cost that makes naive Perl-to-anything
translations slow.  The compiler's analysis exists to find the variables
that never need a box — a counter, an accumulator, a loop's read-only
element — and give them a plain slot instead.  Every such decision is a
named, switchable optimization (`PCL_OPT`), and the general form must
produce the same output, which the test suite checks.

**Modules are Perl.**  The compiler and the runtime implement the language
and nothing module-specific.  A module that needs its own behaviour gets it
from [`lib/`](lib), as ordinary Perl compiled like user code.  Three parts
of the runtime itself are written that way and checked in as generated
Lisp: `pack`/`unpack`, `mro`, and `warnings`.

**Running.**  `pcl` compiles the script to a temporary Lisp file and starts
SBCL from a saved memory image that already contains the compiled runtime
(built on first use, cached, keyed on the runtime's source), so startup is
about a tenth of a second plus the time to compile your script.

## Roadmap

* **v0.2**: the compiler's census of untranslatable statements over all
  test populations goes to zero (62 today; see [Measured](#measured)); the queue of small correctness
  fixes found by perl's `t/` tree and the CPAN board; a measured speed story
  for whole programs rather than microbenchmarks.
* **After that:** finishing the standalone binary — `pl2cl --executable`
  already saves an image that runs the program (its compile phase, `use`d
  modules included, happens at build time, as perl's does), but the module
  closure and the pack/mro/warnings extensions are not embedded yet, so the
  binary still reads this tree for them.  Then wider XS support through
  pclxs, and the generated code as a documented target for other tools.
* **Planned, not rejected:** live symbol-table hashes (`%Foo::`), full
  `caller()` fidelity, perl 5.38 classes, `defer` blocks, `tie` on
  aggregates, `format`, indirect object syntax with a scalar invocant, and a
  `use warnings` model.  Each has an entry in
  [`docs/not-supported.md`](docs/not-supported.md) saying what it would take.

## Documentation

| | |
|---|---|
| [`docs/pcl-commands.md`](docs/pcl-commands.md) | the command reference: `pcl`, `pl2cl`, `runpcl`, the installer, how the runtime and modules are compiled and cached |
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
<https://github.com/Percolisp/pcl>; [`CONTRIBUTING.md`](CONTRIBUTING.md) says
what a useful bug report carries (the short answer: one small program, run by
`perl` and by `pcl` side by side).  `tools/prove-core` runs the test suite
and must stay green; CI runs the same suite on a clean Ubuntu machine.
[`CLAUDE.md`](CLAUDE.md) records the working rules the project follows.  It
is written as instructions for the AI sessions that do much of the
development, so it is dense reading, but it is an honest account of how
changes are made and verified here.

## Background

PCL was planned and largely written with Claude, Anthropic's Fable and Opus
models.  That includes the rewrite of the compiler's core into its present
form.  My own Common Lisp is from long ago, so that side is essentially all
Claude's.

Two things are worth passing on.  Differential fuzzing against perl found
real bugs cheaply.  And `pack` turned out to be easiest to get right by
writing it in Perl and letting PCL compile it.

PCL will go on CPAN once it is closer to ready.

## License

Free software, under the same terms as Perl itself: at your option the
Artistic License 1.0 or the GNU GPL v1 or later.  [`LICENSE`](LICENSE) is the
statement; the two texts ship beside it, copied verbatim from a perl source
distribution, as [`LICENSE-Artistic`](LICENSE-Artistic) and
[`LICENSE-GPL`](LICENSE-GPL).

`cl/vendor/` holds third-party source carried verbatim and is not covered by
that: today it is cl-ppcre, under its own BSD 2-clause licence
(`cl/vendor/cl-ppcre/LICENSE`).
