# Percolisp (PCL) — Perl 5 compiled to native code, by way of Common Lisp

[![CI](https://github.com/Percolisp/pcl/actions/workflows/ci.yml/badge.svg)](https://github.com/Percolisp/pcl/actions/workflows/ci.yml)

PCL compiles a Perl 5 program, with the modules it uses, into Common Lisp.
[SBCL](https://www.sbcl.org/) then compiles that into machine code and
runs it.  A runtime library written in Lisp supplies what perl does behind
the scenes: context, coercion, `local`, `tie`, `use overload`, string
`eval`.

perl runs the compiler.  The compiled program does not need it, except to
`eval` a string at run time.

Why?

* **Speed, where it can be proved safe.**  A variable nobody takes a
  reference to becomes a machine integer instead of a Perl scalar.  Loops,
  recursion and integer math run two to four times faster than under perl.
  Other things are slower; the [numbers](#speed) show both.
* **Output you can read.**  The Lisp keeps your variable names, sigils and
  Perl's operator names, and its meaning is [specified](docs/ir-spec.md).
* **A small implementation.**  Garbage collection, closures, `local` and
  non-local exits come from Lisp, not hand-written C.  About 65,000 lines
  of Perl and Lisp in all.

**Maturity: early.**  First tag v0.1.0, August 2026.  Pure-Perl code works
well, including most CPAN modules written in Perl.  XS modules, the ones
with a C part, do not work at all.  Read [What works](#what-works) before
depending on it.

## Quick start

You need perl (5.20 or newer), two CPAN modules, and SBCL with one Lisp
library.  [Requirements](#requirements) has the version rules; the short
version is that both minimum versions are newer than most Linux
distributions ship.

```bash
git clone https://github.com/Percolisp/pcl.git
cd pcl

cpanm PPI Moo                                  # PPI must be 1.291 or newer
sbcl --eval '(ql:quickload :cl-ppcre)' --quit  # SBCL 2.5.2 or newer, with Quicklisp

./pcl -E 'my @a = (1..5); say join ",", map { $_ * 2 } @a'
```

```console
2,4,6,8,10
```

The first run takes a few seconds longer than the rest: PCL compiles its
runtime library once and caches the result under `~/.pcl-cache/`.

To put the commands on your `PATH`, install a copy:

```bash
tools/install-pcl --prefix ~/.local     # copies the tree, builds the cache, self-tests
```

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

**Caches and switches.**  Compiled modules and the runtime cache live in
`~/.pcl-cache/` (`PCL_CACHE_DIR` moves it; `pcl --clear-cache` empties it).
The runtime cache is keyed on a hash of the runtime's source, and a cached
module is checked against its file's modification time, so neither goes
stale.
`PCL_OPT=none` turns off every speed optimization and compiles the fully
generic form; the output must behave identically, and the test suite
checks that it does.  Your own script is compiled on every run, so a large
one pays a pause before its first line: a one-liner starts in under a
quarter of a second, a thousand-line script takes a few seconds.

## A worked example

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

To see what the compiler produces, take a smaller piece:

```perl
my $n   = shift // 1000;
my $sum = 0;
for my $i (1 .. $n) { $sum += $i * $i }
print "$sum\n";
```

`pl2cl` turns the body of that into

```lisp
(p-let (($n :box (make-p-box nil)))
  (p-my-= $n (p-// (p-shift @ARGV) 1000))
  (p-let (($sum :scalar 0))
    (p-foreach-range-raw ($i 1 $n) :my t (p-incf-raw $sum (p-* $i $i)))
    (p-print (p-string-concat $sum "\n"))))
```

Reading it as a Perl programmer: `p-let` is `my`, and the word after the
variable is what the compiler decided about it.  `$n` is `:box` — it is
handed to `shift` and could be aliased, so it lives in a small container
that references and `local` can work on.  `$sum` is `:scalar` — nothing
ever takes a reference to it, so it is a bare machine value.  The loop is
`p-foreach-range-raw`, the counting-loop form that keeps `$i` as a raw
integer, and `p-incf-raw` adds into the raw slot.  Everything with a `p-`
prefix is a runtime function or macro named after the Perl operator it
implements.  That decision-making is where the speed comes from, and the
[IR manual](docs/ir-spec.md) documents every form.

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

**What does not work**, in rough order of how often it matters:

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
breakdowns.  All were taken on 2026-09-04.

| measurement | result | reproduce |
|---|---|---|
| PCL's own regression suite | **195 files, 6,729 assertions, all passing** | `tools/prove-core` |
| perl's test suite, extracted (108 files from perl 5.40's `t/`) | **18,581 pass / 649 fail (96.6 %)**; 58 files pass completely | `perl sweep-perl-tests.pl --jobs 8` |
| perl's whole `t/` tree, run in place (528 files) | 92 files identical to perl; 108 differ for a registered, explained reason; 275 differ and are the bug queue; the rest do not compile, time out or produce no test output | `tools/run-perl-suite.pl --all --quick --jobs 4` |
| a board of 14 pure-Perl CPAN distributions, 183 test files | **78 files pass, 54 pass partially, 51 fail** (2,140 assertions pass / 342 fail) | `tools/cpan-scoreboard.pl` |
| statements the compiler cannot translate, over all of the above | **62 statements in 19 files**, each with a filed cause | `tools/drop-census.pl` |

Every failing assertion is recorded row by row in a baseline that the test
runner compares against, so a change that breaks something previously
passing fails the run.  The numbers can only move honestly.

### Speed

These are microbenchmarks: each isolates one Perl feature so that a
difference has one cause.  They are not a promise about whole programs.
Ratio is PCL time / perl time, best of five runs, process startup
subtracted; below 1.00× means PCL is faster.

| benchmark | what it measures | PCL / perl |
|---|---|---:|
| cfor | C-style `for` loop summing integers | 0.26× |
| collatz | `while` loop with integer arithmetic | 0.26× |
| intloop= | `for (1..$n) { $s = $s + $_ }` | 0.29× |
| fib(27) | recursion | 0.29× |
| intloop+= | `for (1..$n) { $s += $_ }` | 0.35× |
| feread | read-only `foreach` over a 1000-element array | 0.47× |
| gcdrec | recursion with modulo | 0.52× |
| arrhash | one array element and one hash element, read and written | 0.60× |
| listcopy | `my @copy = @src`, 50 elements | 0.94× |
| feread2 | `foreach` over two arrays at once | 1.32× |
| symref | symbolic references, `${'main::g'}` | 1.37× |
| arrfill | `@a = (1..20, $_)` on every iteration | 1.46× |
| sliceasgn | assignment to array and hash slices | 1.99× |
| strcat | `$s .= 'x'`, twenty million times | 2.14× |
| regexg | `while ($x =~ /./g)` over a 200 kB string | 2.18× |
| slices | reading `@a[1..5]` and `@h{@k}` | 2.60× |
| ovlsub | `use overload` arithmetic and stringification on objects | 3.46× |
| packunpk | `pack` followed by `unpack` | 858× |
| pack | `pack` with two templates | 1174× |

**Numeric loops and recursion beat perl by two to four times.**  When the
compiler can prove a variable holds a machine integer for its whole life —
nothing takes a reference to it, nothing assigns a string to it, no string
`eval` can reach it — the generated code uses native arithmetic instead of
perl's generic scalar.  The same proof lets a read-only `foreach` bind
array slots directly instead of copying each element.

**Aggregate traffic is mixed.**  Reading and writing single array or hash
elements is faster than perl, and copying a whole array is at parity.
Building an array element by element and moving several elements at once
through slices is slower: PCL's generic hash table and its per-element
checks cost more than perl's flat C arrays on bulk work.

**Dynamic features are slower, because nothing can be proved about them
ahead of time.**  Symbolic references and overloaded operators resolve a
name at run time and call a Perl sub per operation, and perl's C
implementation of that path is still faster than PCL's.  `m//g` in a loop
runs a regex engine written in Lisp ([cl-ppcre](https://edicl.github.io/cl-ppcre/))
instead of perl's hand-tuned C one.

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
* **SBCL 2.5.2 or later.**  The runtime uses some of SBCL's internal APIs,
  so older versions do not work, and Debian 12, Ubuntu 22.04 and Ubuntu
  24.04 all ship an older one.  A binary from
  [sbcl.org](https://www.sbcl.org/platform-table.html) installs without
  root.  Which one depends on your glibc: the current 2.6.0 binary needs
  glibc 2.38, which Ubuntu 24.04 and Debian 13 have; Ubuntu 22.04 and
  Debian 12 do not, and need the 2.5.2 binary.  Both combinations are
  installed and tested by the
  [install matrix](.github/workflows/install-matrix.yml).

  | distribution | SBCL binary |
  |---|---|
  | Ubuntu 22.04, Debian 12 | 2.5.2 |
  | Ubuntu 24.04, Debian 13 and newer | 2.6.0 (current) |
* **cl-ppcre**, the regex engine, installed through
  [Quicklisp](https://www.quicklisp.org/):
  `sbcl --eval '(ql:quickload :cl-ppcre)' --quit`.

Use `pcl` and the other wrappers rather than `sbcl --script`: that flag
skips `~/.sbclrc`, so SBCL never sees the Quicklisp-installed cl-ppcre.

## How it works

For readers who know Perl but not compilers or Lisp, the pieces are these.

**The compiler** (`Pl/`, about 42,000 lines of Perl) reads your source with
[PPI](https://metacpan.org/pod/PPI), the CPAN Perl parser, builds a tree of
statements and expressions, works out for every variable how it is used
(is a reference ever taken? is it captured by a closure? is it only ever a
number?), and writes out one Lisp form per Perl statement.

```
Perl source → PPI → Pl::Parser2 (statements) → Pl::CLForm → Common Lisp text
                        ↓                ↑                          ↓
              Pl::VarAnnotator    Pl::PExpr → Pl::ExprToCL      cl/pcl-runtime.lisp
             (scopes, captures)   (expression AST → forms)     (Perl semantics in Lisp)
```

**The runtime** ([`cl/pcl-runtime.lisp`](cl/pcl-runtime.lisp), about
22,000 lines of Common Lisp) is a library of the Perl operations that
cannot be decided at compile time: what `+` does to `"3 apples"`, how
`local` restores a value on scope exit, how a method call finds its target,
how `sort` calls its comparator.  The compiled program is mostly calls into
this library, and it is where Perl's semantics are pinned down.  Common Lisp
already provides the underpinnings Perl needs — dynamic typing, closures,
dynamic binding for `local`, non-local exits for `die`/`last`/`return`,
garbage collection — so the runtime uses those directly instead of
rebuilding them.

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
  test populations goes to zero (57 today); the queue of small correctness
  fixes found by perl's `t/` tree and the CPAN board; a measured speed story
  for whole programs rather than microbenchmarks.
* **After that:** compiling a script once to a standalone binary (the
  runtime can already be saved as an image; the flag that exists today is
  not yet correct), wider XS support through pclxs, and the generated code as
  a documented target for other tools.
* **Planned, not rejected:** live symbol-table hashes (`%Foo::`), full
  `caller()` fidelity, perl 5.38 classes, `defer` blocks, `tie` on
  aggregates, `format`, indirect object syntax with a scalar invocant, and a
  `use warnings` model.  Each has an entry in
  [`docs/not-supported.md`](docs/not-supported.md) saying what it would take.

## Documentation

| | |
|---|---|
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
<https://github.com/Percolisp/pcl>.  `tools/prove-core` runs the test suite
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

Free software, under the same terms as Perl itself: the Artistic License 1.0
or the GNU GPL v1 or later.  See [`LICENSE`](LICENSE).
