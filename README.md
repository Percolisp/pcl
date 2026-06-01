# PCL — A Perl-to-Common-Lisp Compiler

**PCL is a from-scratch source-to-source compiler that turns Perl into readable Common Lisp.** It parses Perl with [PPI](https://metacpan.org/pod/PPI), builds an AST with correct operator precedence, and generates CL that a Perl programmer can still read.

```bash
$ echo 'my @a=(1..5); print join(",", map { $_*2 } @a), "\n";' \
    | ./pl2cl | sbcl --load cl/pcl-runtime.lisp --script /dev/stdin
2,4,6,8,10
```

### Why Common Lisp?

Two reasons:

1. **Compiling to a high-level language keeps the compiler tractable.** CL is expressive enough to model Perl's semantics directly, so PCL can stay a manageable size instead of growing into a full interpreter.
2. **Lisp is trivial to parse**, which makes the generated CL a good *intermediate representation* — a stepping stone for compiling Perl onward to other environments.

### There is no bytecode engine

This is a genuinely new implementation. PCL does **not** embed, link, or reimplement Perl's runtime or opcode interpreter. It is a from-scratch source-to-source compiler: Perl text in, Common Lisp text out.

## Quick Start

```bash
# Dependencies: Perl 5.30+, PPI, Moo, SBCL, cl-ppcre (via Quicklisp)
cpanm PPI Moo
sbcl --eval '(ql:quickload :cl-ppcre)' --quit

# Transpile and run
echo 'print "Hello, World!\n";' | ./pl2cl | sbcl --noinform --load cl/pcl-runtime.lisp --script /dev/stdin

# Run the internal test suite (91 files, 3168 tests)
prove -j8 Pl/t/
```

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
$ ./pl2cl input.pl | sbcl --load cl/pcl-runtime.lisp --script /dev/stdin
I am Rex and I bark
```

## What Works

A teaser across the big areas — most of Perl's day-to-day surface is in:

- **Operators** — all 92 precedence levels, chained comparisons, string ops
- **Control flow** — `if/unless`, `while/until`, `for/foreach` (aliasing `$_` to elements, incl. hash/array elements and `substr`/`pos`/`vec` lvalues), loop labels, `next/last/redo`
- **Subroutines** — signatures, defaults, closures, `state` variables
- **References** — `\$x`, `$$ref`, `$aref->[0]`, `@{$ref}`, anonymous constructors, postfix deref, and live lvalue refs (`\substr`, `\pos`, `\vec`, `\$#array`)
- **OO** — `bless`, method calls, `@ISA` with C3 MRO, multiple inheritance, `SUPER::`, `AUTOLOAD`, `use overload`
- **Built-ins** — `print/say`, `push/pop/shift/unshift/splice`, `map/grep/sort`, `sprintf`, `length/substr/index`, `each/keys/values`, `open/readline`, `die/eval`, `tie`, regex `m//`/`s///`/`tr///`, `pack`/`unpack`
- **Regex** via CL-PPCRE — modifiers, named captures `%+`, `$1`…
- **Modules & packages** — `package Foo { }`, `use constant`, `BEGIN`, `use`/`require`, `use parent`
- **`eval`** — both block `eval { }` and string `eval "code"` (transpiled and run at runtime)
- **`local`** — scalars, arrays, hashes, hash/array elements, typeglobs

*See [`REMAINING.md`](REMAINING.md) for the full picture.*

## Not Supported

A few of the biggest items:

- **XS / C extensions** — anything requiring compiled C code (the eventual goal; see the roadmap).
- **`@_` argument aliasing** — args are copied into `@_`; `$_[0] = 42` does not write back to the caller.
- **Exact error-message text** — PCL targets correct execution, not byte-for-byte error wording or the `" at FILE line N"` suffix.
- **`DESTROY` on garbage collection** — CL's GC gives no deterministic finalizer timing.
- **Removed Perl features** — `given`/`when`, the `~~` smart-match, and `?pattern?` (gone in Perl 5.38).

*Full list and rationale: [`REMAINING.md`](REMAINING.md) and [`docs/not-supported.md`](docs/not-supported.md).*

## How It Is Tested

Perl ships an excellent, thorough test suite (`t/op/`, `t/base/`, …). PCL compiles those test files to Common Lisp and runs them — using Perl's own expectations as the oracle for compatibility.

Some tests exercise features that are deliberately out of scope (e.g. CL-PPCRE has no executable code blocks inside regexes, `(?{...})`, and removed/experimental features aren't implemented). Those tests have to be skipped — which means some features can quietly end up *under-covered*, something that has to be reviewed with care rather than assumed away.

The safety net is the **skip-registry** ([`cl/skip-registry.lisp`](cl/skip-registry.lisp)): it records exactly *which* tests are skipped and *why*, still runs the underlying assertion, and flags itself **stale** the moment a skipped test starts passing. So "what is uncovered" is tracked, not guessed. (See [`docs/test-skip-registry.md`](docs/test-skip-registry.md).)

## Architecture

```
Perl Source → PPI → Pl::PExpr (AST) → Pl::ExprToCL → Common Lisp
                          ↓                                ↓
                  Pl::BlockAnalyzer               cl/pcl-runtime.lisp
               (two-phase scope analysis)        (Perl semantics in CL)
```

| Module | Purpose |
|--------|---------|
| `Pl/Parser.pm` | Statement-level parser |
| `Pl/PExpr.pm` | Expression parser, operator precedence |
| `Pl/BlockAnalyzer.pm` | Two-phase block analysis (declaration scoping) |
| `Pl/ExprToCL.pm` | Code generator |
| `cl/pcl-runtime.lisp` | Runtime library (~10,000 lines of CL) |
| `cl/pack-impl.pl` | `pack`/`unpack`, written in Perl and transpiled to CL |

Generated code is intentionally readable: Perl variables keep their sigils (`$x`, `@array`, `%hash`), and built-ins map to `pl-`/`p-` prefixed names (`pl-print`, `p-push`, …). Today **every variable is a small data structure** (a "box") so it can carry both a numeric and a string value and be referenced — see the roadmap for where that goes next.

## Status

This phase is about hashing out incompatibilities with Perl. It has been slow and at times painful, but the end is visible on the horizon — and hopefully not a mirage.

Against Perl's own test suite, PCL currently passes **~95% of the tests it runs** (excluding ones skipped for unsupported features), with **66 files passing completely**.

A small illustration of how it gets done: when implementing `pack()` in CL proved fiddly even with the original C source in hand, the trick was to write `pack` *in Perl* and let PCL translate it to CL. It worked — eating our own dog food.

My own Common Lisp experience is from long ago; that side of the work is essentially all Claude.

*(It'll go on CPAN later, once it's closer to ready.)*

## Roadmap — after it works reliably

These come *after* compatibility is solid:

- **A smarter code generator.** Right now every variable is a boxed data structure (so it can hold a number, a string, and be referenced). With analysis, variables that are only ever numeric can be compiled to plain native numbers — and PCL could become genuinely fast.
- **Cleaner intermediate code.** Lean on a small set of high-level CL macros for the generated output, making it an easy target for compiling Perl onward to *other* environments.
- **The Eldorado: XS / C extensions.** Get compiled C (XS) working and the full CPAN ecosystem opens up on new platforms. Here I'd welcome help from people who know XS and CL internals better than I do.

## License

This library is free software; you can redistribute it and/or modify it under the same terms as Perl itself — dual-licensed under the Artistic License 1.0 or the GNU GPL v1-or-later. See [`LICENSE`](LICENSE) for details.
