# Percolisp (PCL) — a Perl-to-Common-Lisp compiler

[![CI](https://github.com/Percolisp/pcl/actions/workflows/ci.yml/badge.svg)](https://github.com/Percolisp/pcl/actions/workflows/ci.yml)

Percolisp (PCL) compiles Perl 5 to Common Lisp, and the result runs as
native code under [SBCL](https://www.sbcl.org/).  Nothing of perl's own C
runtime is linked or reimplemented: a runtime library written in Common Lisp
supplies Perl's semantics instead.

The target is a faster Perl.  SBCL is an optimizing native compiler, and
where PCL can prove that a piece of Perl does not need Perl's flexibility, it
emits plain machine code.  Recursion, counting loops and integer arithmetic
already run several times faster than under perl.  Other things are still
slower, and the measurements below report the losses as well as the wins.

The second target is a compiler toolkit.  Common Lisp is easy to parse, so
the generated code is meant to double as an intermediate form — something
another compiler could take on to a different platform.

**Maturity: early.**  v0.1.0 is the first tag.  Pure-Perl code works well;
modules that need compiled C (XS) do not work at all.  See
[What works](#what-works) before investing in it.

## Quick start

```bash
git clone https://github.com/Percolisp/pcl.git
cd pcl

cpanm PPI Moo                                # PPI must be ≥ 1.291
sbcl --eval '(ql:quickload :cl-ppcre)' --quit # SBCL ≥ 2.5.2, via Quicklisp

echo 'my @a=(1..5); print join(",", map { $_*2 } @a), "\n";' > demo.pl
./runpcl demo.pl
```

```console
2,4,6,8,10
```

If that works, you have everything.  [Requirements](#requirements) covers the
two minimum versions — both stricter than most distributions ship — and
assumes [Quicklisp](https://www.quicklisp.org/) is installed for the third
line.

To put `pcl`, `pl2cl` and `runpcl` on your `PATH`:

```bash
tools/install-pcl --prefix ~/.local
```

## Usage

```bash
./runpcl prog.pl                            # compile and run a Perl program
echo 'print 1+2, "\n"' | ./runpcl           # … or from stdin

./pcl -MList::Util=sum -E 'say sum 1..10'   # one-liners, like perl

./pl2cl prog.pl > prog.lisp                 # compile only — readable CL
sbcl --noinform --non-interactive \
     --load cl/pcl-runtime.lisp --load prog.lisp     # … then run that output

./pl2cl < prog.pl | sbcl --noinform --non-interactive \
     --load cl/pcl-runtime.lisp --eval '(load *standard-input*)'   # … or as a pipeline

tools/prove-core                            # run PCL's own test suite
```

Modules are compiled the same way as the program that uses them:

```console
$ cpanm Data::Dump
$ ./pcl -MData::Dump=dump -E '@q=(1 .. 5); say dump [ map { $_, ":", $_ ** $_ } @q ];'
[1, ":", 1, 2, ":", 4, 3, ":", 27, 4, ":", 256, 5, ":", 3125]
```

PCL finds `Data::Dump` in `@INC` and compiles it along with the program, the
same as any other Perl source.

The compiled form is meant to be legible.  This Perl —

```perl
my $n  = shift // 10;
my @sq = map { $_ * $_ } 1 .. $n;
```

becomes this Common Lisp:

```lisp
(let (($n (make-p-box nil)))
  (p-my-= $n (p-// (p-shift @ARGV) 10))
  (let ((@sq (make-array 0 :adjustable t :fill-pointer 0)))
    (p-array-= @sq (p-list-ctx (p-map (lambda ($_) (p-* $_ $_)) (p-.. 1 $n))))
    …))
```

## What works

Most of the language.  Every operator and precedence level is there, and
scalar and list context propagate correctly.  So do closures, `state`,
`local` in all its forms, signatures, `try`/`catch`, regexes, `pack`/`unpack`,
and objects with C3 method resolution and `use overload`.  String `eval`
works too, and sees the enclosing lexicals.

**XS modules do not work.**  Anything implemented in compiled C fails to
load — `DBI` and `JSON::XS` from CPAN, and core modules such as `Storable`.
That rules out a large part of what people actually `use`.
Where a common XS module has a pure-Perl equivalent, PCL ships one and uses
it automatically ([`lib/`](lib)).  A separate project, pclxs, is exploring
support for real XS binaries.

Other deliberate divergences: `@_` aliasing, deterministic `DESTROY`,
`given`/`when`, the exact wording of error messages, and a handful of
interpreter internals.  [`docs/not-supported.md`](docs/not-supported.md)
gives the reason for each.

A statement the compiler cannot translate is reported at compile time, and
the program dies there if it reaches it, as an ordinary Perl exception that
`eval` can catch.  Nothing is skipped silently.

### Measured

[`docs/STATUS.md`](docs/STATUS.md) has these numbers in more detail, with the
failure breakdowns.

| measurement | result (2026-08-25) | reproduce |
|---|---|---|
| PCL's own regression suite | **177 files / 6,011 assertions**, all passing | `tools/prove-core` |
| perl's test suite, extracted (108 files) | **18,313 pass / 893 fail (95.4 %)**; 61 files complete | `perl sweep-perl-tests.pl --jobs 8` |
| perl's full `t/` tree, in place (528 files) | per-file results against a recorded snapshot | `tools/run-perl-suite.pl --all --quick --jobs 4` |
| pure-Perl CPAN distributions (183 of them) | 65 run their whole suite, 65 most of it, 53 fail | `tools/cpan-scoreboard.pl` |

### Speed

PCL beats perl at recursion, counting loops and integer arithmetic: `fib` by
3.3×, `cfor` by 4.1×, `collatz` by 2.5×.  It is slower at `pack`/`unpack`,
at method dispatch, and at moving data in and out of arrays and hashes.  The
current benchmark table, and the work queue behind it, are in
[`docs/faster-codegen-suggestions.md`](docs/faster-codegen-suggestions.md).

## Requirements

PCL targets the semantics of perl 5.40, which is the perl its test suites
come from.  Linux is what it is developed and tested on; other Unixes should
work but are untested.

* **Perl 5.20 or later**, with [PPI](https://metacpan.org/pod/PPI) 1.291 or
  later and [Moo](https://metacpan.org/pod/Moo): `cpanm PPI Moo`.  Nothing
  else is needed; every other module PCL uses is core.  Distributions ship
  older PPI (Ubuntu 24.04 has 1.277) and the installer refuses those.
* **SBCL 2.5.2 or later.**  The runtime uses SBCL's internal APIs, so older
  versions will not work — and Debian 12, Ubuntu 24.04 and Ubuntu 22.04 all
  ship an older one.  A current binary from
  [sbcl.org](https://www.sbcl.org/platform-table.html) installs without root.
* **cl-ppcre**, from Quicklisp:
  `sbcl --eval '(ql:quickload :cl-ppcre)' --quit`.

Compiling needs perl; running a compiled program needs SBCL.  A program that
uses string `eval` compiles code as it runs, so it needs both.

Use the wrappers rather than `sbcl --script`.  That flag skips `~/.sbclrc`,
so SBCL never sees a Quicklisp-installed cl-ppcre and the load fails.

The wrappers also keep the runtime compiled.  The first run builds a saved
SBCL core under `~/.pcl-cache/core/`, and later runs start from it in
milliseconds.  The core is named after a hash of the runtime source, so it
can never be stale.  `PCL_NO_CORE=1` runs from source instead.

Your own program is compiled every time you run it, so a large script pays a
noticeable delay before its first line executes — about five seconds for 800
statements on this machine.  Modules it loads are cached after their first
compile.

## How it works

PCL reads Perl source, builds a tree of its statements and expressions, and
writes out the equivalent Common Lisp.

```
Perl source → PPI → Pl::Parser2 (statement translation) → Pl::CLForm → Common Lisp
                        ↓                ↑                               ↓
              Pl::VarAnnotator    Pl::PExpr → Pl::ExprToCL        cl/pcl-runtime.lisp
             (scopes, captures)   (expression AST → forms)       (Perl semantics in CL)
```

Much of what Perl does happens while a program runs, not while it is being
compiled: context, coercion, `local`, ties, overloading, string `eval`.  PCL
implements that magic in Common Lisp, in
[`cl/pcl-runtime.lisp`](cl/pcl-runtime.lisp), and the compiled program calls
into it at run time, exactly as perl would.

Common Lisp already provides what Perl needs underneath: dynamic types,
closures, dynamic binding, non-local jumps and garbage collection.  PCL uses
those directly instead of building its own machinery for each of them.

The compiler and the runtime implement the language itself and nothing
module-specific.  A module that needs its own behaviour gets it from `lib/`,
as ordinary Perl that PCL compiles like user code.  Three parts of the
runtime are written that way as well: `pack` and `unpack`, `mro`, and
`warnings`.

One goal is that the generated code stays readable to a Perl programmer.
Variables keep their sigils (`$x`, `@a`, `%h`), and Perl's built-ins keep
their names behind a `pl-` or `p-` prefix.  What each construct means is
specified in [`docs/ir-spec.md`](docs/ir-spec.md), which also documents the
calling convention if you want to call compiled Perl from your own Lisp.

## Roadmap

* **v0.2** continues the compatibility work: fewer untranslatable statements,
  more of perl's `t/` tree, more CPAN distributions running clean.
* **After that**, in order: speed, then the generated code as a documented
  target for other tools, then wider XS support.
* **Planned, not rejected:** live symbol-table hashes (`%Foo::`), a fuller
  `caller()`, perl 5.38 classes, indirect object syntax with a scalar
  invocant, and a `use warnings` model.  Each is sketched in
  [`docs/not-supported.md`](docs/not-supported.md).

## Documentation

| | |
|---|---|
| [`docs/STATUS.md`](docs/STATUS.md) | what runs, measured |
| [`docs/not-supported.md`](docs/not-supported.md) | what does not, and why |
| [`docs/ir-spec.md`](docs/ir-spec.md) | what the generated Common Lisp means |
| [`docs/`](docs) | ~140 design notes and measurements; start from `STATUS.md` |

## Contributing

Issues and pull requests are welcome at
<https://github.com/Percolisp/pcl>.  `tools/prove-core` runs the test suite
and must stay green; CI runs the same suite on a clean Ubuntu machine.
[`CLAUDE.md`](CLAUDE.md) records the working rules the project follows.  It
is written as instructions for the AI sessions that do much of the
development, so it is dense reading — but it is an honest account of how
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
