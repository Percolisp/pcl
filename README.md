# PCL (Percolisp) — A Perl-to-Common-Lisp Compiler

**PCL is a from-scratch source-to-source compiler that turns Perl into readable Common Lisp.** It parses Perl with [PPI](https://metacpan.org/pod/PPI), builds an AST with correct operator precedence, and generates CL that a Perl programmer can still read.

```bash
$ echo 'my @a=(1..5); print join(",", map { $_*2 } @a), "\n";' \
    | ./pl2cl | sbcl --noinform --non-interactive --load cl/pcl-runtime.lisp --eval '(load *standard-input*)'
2,4,6,8,10
```

(Not `--script`: that flag skips `~/.sbclrc`, so a Quicklisp-installed cl-ppcre
becomes invisible to ASDF and the runtime fails to load.)

The `pcl` command runs one-liners directly, including `-M` imports of pure-Perl CPAN modules:

```bash
$ ./pcl -MData::Dump=dump -E '@q=(1 .. 5); @w = map { $_, ":", $_ ** $_ } @q; say dump \@w;'
[1, ":", 1, 2, ":", 4, 3, ":", 27, 4, ":", 256, 5, ":", 3125]
```

### Why Common Lisp?

Two reasons:

1. **Compiling to a high-level language keeps the compiler tractable.** CL is expressive enough to model Perl's semantics directly, so PCL can stay a manageable size instead of growing into a full interpreter.
2. **Lisp is trivial to parse**, which makes the generated CL a good *intermediate representation* — a stepping stone for compiling Perl onward to other environments.

Since SBCL ships a genuinely good optimizing compiler that turns the generated CL into native code, there is real performance headroom to draw on once the code generator gets smarter (see the roadmap).

#### The hard part — Perl's runtime *magic*

Perl is genuinely hard to parse — much of its behaviour only exists while the program runs, so you practically have to *execute* it. PCL sidesteps this instead of solving it: it reproduces the same magic in the Common Lisp runtime. Constructs that are hard precisely because they must execute simply execute on the CL side, with the same semantics.

### There is no bytecode engine

This is a genuinely new implementation. PCL does **not** embed, link, or reimplement Perl's runtime or opcode interpreter. It is a from-scratch source-to-source compiler: Perl code in, Common Lisp code out.

## Quick Start

```bash
# Dependencies: Perl 5.20+, PPI, Moo, SBCL 2.5.2+ (see below), cl-ppcre (via Quicklisp)
cpanm PPI Moo
sbcl --eval '(ql:quickload :cl-ppcre)' --quit

# Transpile and run
echo 'print "Hello, World!\n";' | ./pl2cl | sbcl --noinform --non-interactive --load cl/pcl-runtime.lisp --eval '(load *standard-input*)'

# Run the internal test suite (155 files, ~5,600 assertions)
prove -j8 Pl/t/

# Faster: run it against a saved SBCL core (runtime pre-compiled in).
# Each test otherwise recompiles the ~1.2s runtime on every sbcl spawn;
# a core cuts that to ~0.003s. tools/prove-core rebuilds a FRESH core on
# every run (so tests never see a stale image), then runs the suite:
tools/prove-core                 # == prove -j8 Pl/t/, but ~4x faster
tools/prove-core Pl/t/foo-01.t   # any prove args work
```

The two commands are equivalent in *what* they check — the core is purely a
speed cache. Plain `prove -j8 Pl/t/` always works and stays the reference; the
core path is opt-in via `tools/prove-core` (or by setting `PCL_TEST_CORE` to a
core path yourself).

### Installing

`tools/install-pcl` copies the runtime tree to a prefix, writes `bin/`
wrappers, and **compiles the runtime into a saved SBCL core at install
time** (the XS model — never at first use), then refuses to finish unless
the installed tools actually transpile and run a program:

```bash
tools/install-pcl --prefix ~/.local     # default prefix is $HOME/.local
tools/install-pcl --no-core --dry-run   # show what it would do
```

### Minimum SBCL version: 2.5.2

The runtime uses SBCL-internal APIs (`sb-unicode`, float bit accessors, …), and
the test suite is validated on 2.5.2 and 2.6.0 — so **SBCL 2.5.2 is the hard
floor**. The very first form of `cl/pcl-runtime.lisp` checks
`(lisp-implementation-version)` and prints a loud warning on an older host,
so the load failure that follows explains itself. Distribution packages that
qualify:
**Debian 13 “trixie”** and **Ubuntu 25.10 / 26.04 LTS** ship 2.5.2+; anything
older (Debian 12: 2.2.9, Ubuntu 24.04 LTS: 2.2.9, Ubuntu 22.04 LTS: 2.1.11)
does not — on those, install the current binary from
[sbcl.org](https://www.sbcl.org/platform-table.html) (a home-directory install
works fine and needs no root).

## Example

```perl
# input.pl
package Animal;
sub new   { bless { name => $_[1] }, $_[0] }
sub speak { "I am " . $_[0]->{name} }

package Dog;
our @ISA = ('Animal');
sub speak { $_[0]->SUPER::speak() . " and I bark" }

package main;
my $d = Dog->new("Rex");
print $d->speak(), "\n";
```

```bash
$ ./pl2cl input.pl | sbcl --noinform --non-interactive --load cl/pcl-runtime.lisp --eval '(load *standard-input*)'
I am Rex and I bark
```

## What Works

A teaser across the big areas — most of Perl's day-to-day surface is in:

- **Operators** — all 92 precedence levels, chained comparisons, string ops
- **Control flow** — `if/unless`, `while/until`, `for/foreach` (aliasing `$_` to elements, incl. hash/array elements and `substr`/`pos`/`vec` lvalues), loop labels, `next/last/redo`, perl 5.34 `try/catch/finally`
- **Subroutines** — signatures, defaults, closures, `state` variables
- **References** — `\$x`, `$$ref`, `$aref->[0]`, `@{$ref}`, anonymous constructors, postfix deref, and live lvalue refs (`\substr`, `\pos`, `\vec`, `\$#array`)
- **OO** — `bless`, method calls, `@ISA` with C3 MRO, multiple inheritance, `SUPER::`, `AUTOLOAD`, `use overload`
- **Built-ins** — `print/say`, `push/pop/shift/unshift/splice`, `map/grep/sort`, `sprintf`, `length/substr/index`, `each/keys/values`, `open/readline`, `die/eval`, `tie`, regex `m//`/`s///`/`tr///`, `pack`/`unpack`
- **Regex** via CL-PPCRE — modifiers, named captures `%+`, `$1`…
- **Modules & packages** — `package Foo { }`, `use constant`, `BEGIN`, `use`/`require`, `use parent`
- **`eval`** — block `eval { }` and string `eval "code"` (transpiled and run at runtime), with **lexical capture**: the eval'd code reads *and writes* the enclosing scope's `my` variables, and closures built inside the eval close over them
- **`local`** — scalars, arrays, hashes, hash/array elements, typeglobs

*See [`docs/STATUS.md`](docs/STATUS.md) for the measured compatibility state.*

## Not Supported

A few of the biggest items:

- **XS / C extensions** — anything requiring compiled C code (the eventual goal; see the roadmap).
- **`@_` argument aliasing** — args are copied into `@_`; `$_[0] = 42` does not write back to the caller.
- **Exact error-message text** — PCL targets correct execution, not byte-for-byte error wording or the `" at FILE line N"` suffix.
- **`DESTROY` on garbage collection** — CL's GC gives no deterministic finalizer timing.
- **Removed Perl features** — `given`/`when` and the `~~` smart-match (removed in Perl 5.42) are refused with a clear message; one-match `?pattern?` is not supported.
- **`mro` pragma** — PCL always uses C3 (it is CLOS-backed); the DFS default, ordering switch, and most of the `mro::*` API are not emulated. A minimal C3-only `mro::get_linear_isa` is provided so modules that `require mro` load. Provisional — revisit if a module is shown to depend on the missing parts.

*Full list and rationale: [`docs/STATUS.md`](docs/STATUS.md) (summary tables) and [`docs/not-supported.md`](docs/not-supported.md) (every entry with its why).*

A statement the compiler cannot lower is never silently lost: it is
**announced on stderr** (`PCL: statement dropped at FILE line N: …`) in file
mode, and in `eval STRING` it dies into `$@` the way perl's compile errors
do. The count of such statements across the two test populations is tracked
in-repo and gated ([`docs/parse-error-drop-census-s399.tsv`](docs/parse-error-drop-census-s399.tsv)).

## How It Is Tested

Testing is done with CPAN modules, a differential fuzzer and Perl's own excellent test suite (`t/op/`, `t/base/`, …): PCL compiles those test files to Common Lisp and runs them, using Perl's own expectations as the oracle for compatibility.

Some tests exercise features that are deliberately out of scope (e.g. CL-PPCRE has no executable code blocks inside regexes, `(?{...})`, and removed/experimental features aren't implemented). Those tests have to be skipped — which means some features can quietly end up *under-covered*, something that has to be reviewed with care rather than assumed away.

The safety net is the **skip-registry** ([`cl/skip-registry.lisp`](cl/skip-registry.lisp)): it records exactly *which* tests are skipped and *why*, still runs the underlying assertion, and flags itself **stale** the moment a skipped test starts passing. So "what is uncovered" is tracked, not guessed. (See [`docs/test-skip-registry.md`](docs/test-skip-registry.md).)

## Architecture

```
Perl Source → PPI → Pl::Parser2 (statement lowering) → Pl::CLForm → Common Lisp
                        ↓                ↑                              ↓
              Pl::VarAnnotator    Pl::PExpr → Pl::ExprToCL      cl/pcl-runtime.lisp
             (scopes, captures)     (expression AST → forms)   (Perl semantics in CL)
```

| Module | Purpose |
|--------|---------|
| `Pl/Parser2.pm` | Statement lowering (the v2 structured-emission pipeline) |
| `Pl/VarAnnotator.pm` | Scope/capture analysis, lexical renaming |
| `Pl/PExpr.pm` | Expression parser, operator precedence |
| `Pl/ExprToCL.pm` | Expression code generator (forms) |
| `Pl/CLForm.pm` | The emitted-form data structure and printer |
| `Pl/Passes.pm` | The named optimization registry (`PCL_OPT`) |
| `cl/pcl-runtime.lisp` | Runtime library (~17,000 lines of CL) |
| `cl/pack-impl.pl` | `pack`/`unpack`, written in Perl and transpiled to CL |

Generated code is intentionally readable: Perl variables keep their sigils (`$x`, `@array`, `%hash`), and built-ins map to `pl-`/`p-` prefixed names (`pl-print`, `p-push`, …). Today **every variable is a small data structure** (a "box") so it can carry both a numeric and a string value and be referenced — see the roadmap for where that goes next.

## Status

With temporary access to Fable 5, it was used to plan the rewrite of the compiler. This has been going well: the rewrite (the "v2" structured-emission pipeline) is complete and is now the only pipeline.

Against Perl's own test suite, PCL currently passes **18,363 assertions (95.3 % of those it runs)** across 108 extracted test files, with **62 files passing completely** — tracked row-by-row against blessed baselines, so the number can only move honestly. Several pure-Perl CPAN modules run unmodified through the full pipeline (e.g. `List::Util`, `Role::Tiny`, `Data::Dump`, and the core try/catch of `Try::Tiny`) — shaking out general compiler bugs in the process. The measured state, including what deliberately does not work, is [`docs/STATUS.md`](docs/STATUS.md).

XS is no longer purely an aspiration: a separate experimental sibling project, **pclxs**, implements a `libperl`-ABI shim that lets unmodified compiled XS modules call into PCL's runtime. Its 398-case conformance corpus passes against real perl as the oracle, and `Digest::MD5` works end-to-end (its own test file passes 256/256 under PCL). It is not bundled with this release.

As an aside - Claude suggested doing "fuzzing", between PCL and Perl. It generated different expressions and evaluated them in both environments. It was valuable in finding bugs. (It seems to be standard procedure when building compilers. :-) )

There was a fun roundabout when implementing `pack()` in CL. It proved fiddly for CLaude, even with the original C source in hand. It worked to write `pack` *in Perl* and let PCL translate it to CL. It worked — eating our own dog food.

My own Common Lisp experience is from long ago; that side of the work is essentially all Claude.

*(It'll go on CPAN later, once it's closer to ready.)*

## Roadmap — after it works reliably

These come *after* compatibility is solid:

- **A smarter code generator.** Right now every variable is a boxed data structure (so it can hold a number, a string, and be referenced). With analysis, variables that are only ever numeric can be compiled to plain native numbers — and PCL could become genuinely fast.
- **Cleaner intermediate code.** Lean on a small set of high-level CL macros for the generated output, making it an easy target for compiling Perl onward to *other* environments.
- **The Eldorado: XS / C extensions.** The experimental pclxs bridge (see Status above) proves the approach; getting a broad slice of CPAN's XS dists working is the long game. Here I'd welcome help from people who know XS and CL internals better than I do.

### Deferred language features — planned, not rejected

A handful of introspection/metaprogramming features are *implementable* but
deliberately parked until the compatibility phase is solid. They are listed in
[`docs/not-supported.md`](docs/not-supported.md) today, but unlike the
interpreter-internals items they are a matter of *when*, not *whether*:

- **Live symbol-table hashes (`%main::`, `%Foo::`).** Today the stash is a
  read-only, subs-only snapshot. The plan is a live proxy over the underlying
  CL package (read/write/`delete`/`keys`), then full typeglob slots. It is a
  pure runtime change — no new compiler analysis needed — and cheap, because
  normal `$Foo::bar` access never touches the stash.
- **`__SUB__` (outside string `eval`).** A per-sub macro can give a sub a
  reference to itself for recursion; only subs that mention `__SUB__` pay
  anything. The string-`eval` case stays unsupported (dies).
- **Richer `caller()`.** Package and sub-name depth for `caller(N)` are
  reachable via SBCL frame walking (behind a debug flag). Accurate file/line
  still waits on the source-map work that the smarter code generator brings.
- **Perl 5.38 `class`/`field`/`method` syntax.** The new core OO feature is
  planned for a future version. It is surface syntax over machinery PCL
  already has (packages + CLOS classes, per-instance fields, methods with an
  implicit `$self`, `ADJUST` ≈ constructor code, `:isa` ≈ `@ISA`/C3), so the
  work is chiefly parser desugaring. Deferred because almost no CPAN code
  targets it yet — see `docs/not-supported.md` §class for the sketch.

## License

This library is free software; you can redistribute it and/or modify it under the same terms as Perl itself — dual-licensed under the Artistic License 1.0 or the GNU GPL v1-or-later. See [`LICENSE`](LICENSE) for details.
