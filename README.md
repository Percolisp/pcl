# Percolisp (PCL): Perl compiled to native code

[![CI](https://github.com/Percolisp/pcl/actions/workflows/ci.yml/badge.svg)](https://github.com/Percolisp/pcl/actions/workflows/ci.yml)
[![Latest tag](https://img.shields.io/github/v/tag/Percolisp/pcl)](https://github.com/Percolisp/pcl/tags)

Percolisp (PCL) compiles Perl 5 programs to Common Lisp and runs them
with the [SBCL](https://www.sbcl.org/) native-code compiler. The goal is
to run real CPAN modules unchanged. PCL is in active development. The
first release, v0.1.0, is tagged, and [`docs/STATUS.md`](docs/STATUS.md)
says what runs today.

The second main target is a compiler toolkit for Perl. The generated
Lisp is a documented IR (Intermediate Representation),
[specified](docs/ir-spec.md) here. Also, see the
[architecture](docs/v2-target-architecture.md). The aim is to allow
someone to fork this as a basis for their own Perl compiler. PCL is
licensed the same as Perl.

All AI configuration files for continuing this should be here
(including ~/.claude etc).

## A quick look

Here is an example of a compilation. The original Perl:

```perl
use feature 'say';
my $n   = shift // 1000;
my $sum = 0;
for my $i (1 .. $n) { $sum += $i * $i }
say $sum;
```

The compiler output:

```lisp
(p-let (($n :box (make-p-box nil)))
  (p-my-= $n (p-// (p-shift @ARGV) 1000))
  (p-let (($sum :scalar 0))
    (p-foreach-range-raw ($i 1 $n) :my t (p-incf-raw $sum (p-* $i $i)))
    (p-say $sum)))
```

And running it:

```console
$ pcl sum.pl 10
385
```

`p-let` declares the variable, so the `my` was split in two. Note
that variable names are kept and that there are type declarations for
variables.

Everything with a `p-` prefix is a runtime function (or macro) named
after the Perl operator it implements. `p-incf-raw` changes the
variable directly, since there is no risk of references or overloads.

## Quick start

You need **perl 5.20 or later** with **PPI 1.291 or later** and **Moo**
from CPAN, and **SBCL 2.5.2 or later**. Nothing else beyond core
modules. Linux distributions ship an older SBCL and an older PPI
(Ubuntu 24.04 has PPI 1.277, which the installer refuses), so install
both as below. PCL implements perl 5.40 semantics and is tested on
Linux. Other Unix systems should work but are not tested.

These commands set up PCL on a fresh Ubuntu 24.04 machine (in a
container where you are already root, leave out `sudo`):

<!-- doc-example: Pl/t/pcl-doc-examples-01.t runs the pcl line below -->
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

Which SBCL binary to install depends on your glibc:

| distribution | SBCL binary |
|---|---|
| Ubuntu 24.04, Debian 13 and newer | 2.6.0 |
| Ubuntu 22.04, Debian 12 | 2.5.2 (put `2.5.2` where the lines above say `2.6.0`) |

Both are installed and tested by the
[install matrix](.github/workflows/install-matrix.yml). To install SBCL
without root, use `INSTALL_ROOT=$HOME/sbcl sh install.sh`. The one Lisp
library PCL needs, the regex engine
[cl-ppcre](https://edicl.github.io/cl-ppcre/), is included in the tree.
On the first run, the runtime is compiled once and cached in
`~/.pcl-cache/`.

To put `pcl`, `pl2cl` and `runpcl` on your `PATH`, install a copy:

```bash
tools/install-pcl --prefix ~/.local     # copies the tree, builds the runtime, self-tests
```

It prints the `export PATH=...` line to paste if `~/.local/bin` is not
on your `PATH`, and it never edits your shell startup files.
`tools/install-pcl --uninstall --prefix ~/.local` removes it again.

**Container.** No image is published yet: the first push to `ghcr.io`
happens with the next release tag. Building one yourself works today,
with `docker build -t percolisp/pcl .` in a checkout (see the
[`Dockerfile`](Dockerfile)).

## Using PCL

In the shell, use `pcl` instead of `perl`:

```bash
pcl script.pl arg1 arg2         # run a script; @ARGV as usual
pcl -e 'print 1 + 2, "\n"'      # inline code (-E is the same, with `say` enabled)
pcl -MList::Util=sum -E 'say sum 1 .. 10'
pcl -I lib script.pl            # extra @INC directory
pcl -c script.pl                # compile only, then "syntax OK"
pcl --check script.pl arg1      # run under perl and under PCL, compare the output
```

Perl's own switches work too, so one-liners run as under perl:

<!-- doc-example: Pl/t/pcl-doc-examples-01.t runs this -->
```console
$ printf 'a b c\nd e f\n' | pcl -lane 'print $F[1]'
b
e
```

A script's own `#!perl -w` line counts as well.
[`docs/pcl-commands.md`](docs/pcl-commands.md) lists every switch and
where `pcl` differs from perl.

`pl2cl` is the compiler. It reads Perl and writes Common Lisp:

```bash
pl2cl script.pl > script.lisp   # or from stdin: pl2cl < script.pl
sbcl --noinform --non-interactive \
     --load cl/pcl-runtime.lisp --load script.lisp    # run the output by hand
```

`runpcl script.pl` compiles and runs one file with no options. It is
used mostly by the test suite.

**Modules.** A `use` or `require` is resolved through `@INC`, the same
directories perl would search, and the module's source is compiled the
same way as your program. So `cpanm Data::Dump` followed by `pcl
-MData::Dump=dump -E 'say dump [1 .. 3]'` just works: PCL finds
`Data/Dump.pm` and compiles it. PCL ships pure-Perl replacements for
common XS modules in [`lib/`](lib), such as `List::Util`,
`Scalar::Util`, `POSIX`, `Cwd`, `Fcntl`, `Socket` and `IO::Handle`.

**Unsupported.** A statement that cannot be compiled is reported on
stderr at compile time. If the statement is executed, a normal Perl
exception is thrown, which `eval` can catch. A construct that is
deliberately unsupported dies the same way, naming its entry in
[`docs/not-supported.md`](docs/not-supported.md) in the message.

**Caches.** Compiled modules, scripts and the runtime are cached in
`~/.pcl-cache/` and rebuilt automatically when a source file, a
dependency, perl or PPI changes. The first run after an edit pays the
compile time (a few seconds for a thousand-line script). After that a
script starts in about 0.04 seconds. `pcl --cache-info` shows the
cache, `pcl --clear-cache` empties it, and `PCL_OPT=none` turns off
every speed optimization. Details are in
[`docs/caching.md`](docs/caching.md), and every command and flag is in
[`docs/pcl-commands.md`](docs/pcl-commands.md).

**Detecting PCL from Perl code.** `$ENV{_PCL_RUNTIME_}` is true in every
PCL process. Its value is the version that `pcl --version` prints:

```perl
if ($ENV{_PCL_RUNTIME_}) { ... }              # running under PCL
print "PCL $ENV{_PCL_RUNTIME_}\n";            # e.g. PCL 0.1.0
```

`$^V` and `$]` report 5.30.0 for compatibility. `$^X` points at real
perl, and `_PCL_RUNTIME_` is not passed to child processes, so a perl
subprocess never thinks it runs under PCL.

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

`pcl --check demo.pl` confirms that perl prints the same. `pl2cl
demo.pl` prints the Lisp: one form per Perl statement, in source order.
The [IR manual](docs/ir-spec.md) documents every form.

## What does not work (yet)

* **XS modules.** Anything with compiled C fails. An experimental XS
  bridge is being developed separately; see
  [`docs/STATUS.md`](docs/STATUS.md#xs).
* **No standalone binary.** Running a program needs SBCL, and perl too
  if it uses string `eval`.
* **`@_` aliasing is partial.** `$_[0] = 42` changes the caller's
  variable, array element or hash element, as in perl. It doesn't for an
  element reached through a reference (`f($r->{k})`), a call through a
  code reference (`$f->($x)`) or a method call: those get copies.
* **`DESTROY` is never called.** Memory is reclaimed by the Lisp
  garbage collector, not by reference counting, so there is no
  scope-exit destructor. Code that relies on one for cleanup (guard
  objects, temporary files) does not get it. The same goes for
  filehandles: for now, close your filehandles explicitly.
* **`tie` on a filehandle** is announced on stderr and ignored, for now
  (`tie` on scalars, arrays and hashes works).
* **Regex code blocks** `(?{ })` are removed from the pattern with a
  warning at compile time. The match runs without them.
* **`format`/`write`**, **perl 5.38 `class`/`field`/`method`** and
  **`given`/`when`** are refused with a message. **Taint mode** is not
  supported.
* **Error message text** differs. There is no ambition for PCL errors
  to look like perl's errors, though errors happen in the same places
  and `die`/`$@` behave the same. `use warnings` is a no-op.

Not sure PCL runs *your* program correctly? `pcl --check prog.pl ARGS`
runs it under perl and under PCL and prints `IDENTICAL`, or the first
line where they differ. It runs the program twice, so do not use it on
a program whose side effects must happen only once. See
[`docs/pcl-check.md`](docs/pcl-check.md).

[`docs/not-supported.md`](docs/not-supported.md) has the complete list.

### Measured

PCL is tested against its own regression suite, perl 5.40's own test
suite and a set of pure-Perl CPAN distributions. On the tests extracted
from perl's suite, 96.6 % of assertions pass (measured 2026-10-01). A
large part of the remaining failures are deliberate non-support (no XS,
different error text); the rest are the bug queue. Every number, how it
was measured and how to reproduce it are in
[`docs/STATUS.md`](docs/STATUS.md).

### Roadmap

* **v0.2:** fewer incompatibilities, found by running perl's own test
  suite and real programs.
* **A standalone binary:** `pl2cl --executable` already builds one, but
  it still loads some modules at run time.
* **Next target:** probably a second compiler (JavaScript?), likely just
  a beta version. It will be for verifying that the IR really works, and
  it will also be good for testing when trying to implement XS.
* **Planned, not rejected:** live symbol-table hashes (`%Foo::`), full
  `caller()` fidelity, perl 5.38 classes, `tie` on filehandles, `format`,
  indirect object syntax with a scalar invocant, and a `use warnings`
  model. See [`docs/not-supported.md`](docs/not-supported.md).

## Speed

These are microbenchmarks for different Perl features, measured
2026-10-01 on a quiet machine (best of five runs, startup time
subtracted for both). A
ratio below 1.00× means PCL is faster.

| benchmark | what it measures | PCL / perl |
|---|---|---:|
| collatz | `while` loop with integer arithmetic | 0.18× |
| fib(27) | recursion | 0.31× |
| feread | read-only `foreach` over a 1000-element array | 0.31× |
| strcat | `$s .= 'x'`, twenty million times | 0.95× |
| methret | a method call on a blessed hash, `$o->bump` | 1.12× |
| regexg | `while ($x =~ /./g)` over a 200 kB string | 1.21× |
| ovlsub | `use overload` arithmetic and stringification on objects | 2.52× |
| moo-objs | Moo objects: constructor, accessors, a method building another object | 26× |
| pack | `pack` with two templates | 140× |

Plain loops, arithmetic and array work are several times faster than
perl, because the compiler proves when a variable is always an integer
or an array is never aliased, and emits native code for it. Method
calls are level with perl. Overloading, regex matching and Moo object
construction are slower: nothing about them can be proved at compile
time, the regex engine is [cl-ppcre](https://edicl.github.io/cl-ppcre/)
rather than perl's C one, and Moo generates code with string `eval` at
run time. `pack`/`unpack` is written in Perl and is about 140
times slower; it will be redone. The full table over time is in
[`docs/faster-codegen-suggestions.md`](docs/faster-codegen-suggestions.md).

**Start-up is slow, and not optimized yet.** The table above leaves
start-up out, but for a short program it is most of the time: measured
2026-09-27, a one-line program takes 43 ms under PCL against 1.4 ms under
perl, and about 34 ms of a typical run is the `pcl` launcher itself, a
Perl script (SBCL with PCL's runtime boots in 3 to 7 ms). The first run
after an edit is slower again, because building the program's compiled
file loads every module it uses from source (5.3 seconds for a script
using Getopt::Long), and code made with string `eval`, such as a Moo class's
set-up, is compiled from scratch on every run. All three are planned as
later extensions for faster start-up: a faster launcher, reusing modules'
compiled files on a first run, and a cheaper compile for `eval` strings
([details](docs/STATUS.md#speed)).

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

**The runtime**, in [`cl/pcl-runtime.lisp`](cl/pcl-runtime.lisp), is a
library of the Perl operations: what `+` does to `3` or to
`"3 apples"`, how `local` restores a value on scope exit, how a method
call finds its target, how `sort` calls its comparator. The compiled
program is mostly calls into this library, and it is where Perl's
semantics are pinned down. Common Lisp already provides the
underpinnings Perl needs: dynamic typing, closures, dynamic binding for
`local`, non-local exits for `die`/`last`/`return`, and garbage
collection. So the runtime uses those directly, instead of rebuilding
them.

**Scalars are "boxes", small data structures, unless proved
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

## Documentation

| | |
|---|---|
| [`docs/pcl-commands.md`](docs/pcl-commands.md) | the command reference: `pcl`, `pl2cl`, `runpcl`, the installer |
| [`docs/pcl-check.md`](docs/pcl-check.md) | `pcl --check`: compare your program under perl and under PCL |
| [`docs/caching.md`](docs/caching.md) | what is cached, where, and how to clear or disable it |
| [`docs/STATUS.md`](docs/STATUS.md) | what runs, measured, with failure breakdowns |
| [`docs/not-supported.md`](docs/not-supported.md) | what does not, and why |
| [`docs/ir-spec.md`](docs/ir-spec.md) | what every form in the generated Lisp means |
| [`docs/faster-codegen-suggestions.md`](docs/faster-codegen-suggestions.md) | the benchmark board and the measurement behind each optimization |
| [`docs/shipped-modules.md`](docs/shipped-modules.md) | how `use Module` finds PCL's pure-Perl replacements |
| [`docs/extensions.md`](docs/extensions.md) | the three runtime parts written in Perl, and how they load |
| [`CHANGELOG.md`](CHANGELOG.md) | what changed since v0.1.0 |
| [`docs/`](docs) | the design notes and measurements; most are internal working notes |

## Contributing

Issues and pull requests are welcome at
<https://github.com/Percolisp/pcl>. [`CONTRIBUTING.md`](CONTRIBUTING.md)
says what a useful bug report carries and how to run the tests. The
short answer is one small program, run by `perl` and by `pcl` side by
side; `pcl --check` does that for you. [`CLAUDE.md`](CLAUDE.md) holds
the project's working rules, used for the development described
under [Background](#background).

## Background

PCL was largely written with Claude Code. My own Common Lisp use is
from long ago, so that side is essentially all Claude's. All the
development files used by the AI for making Percolisp are in this
repo.

Two things I learned: differential fuzzing (running the same random
program under perl and under PCL) finds real bugs cheaply. I am not
really a compiler guy; Claude showed me. And `pack` was hard to get
right in Lisp, so it was easier to write it in Perl and let PCL compile
it.

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
