# PCL — Perl to Common Lisp Transpiler

**PCL** converts Perl source code to Common Lisp, aiming for enough compatibility to run real CPAN modules. It parses Perl with [PPI](https://metacpan.org/pod/PPI), builds an AST with proper operator precedence, and generates readable CL code.

There are two motivations. The first is simply running Perl code in a Common Lisp environment. The second is that the generated CL serves as a **portable intermediate representation**: Common Lisp is high-level enough to express Perl semantics cleanly, yet is easy to parse — lowering the threshold for compiling Perl to new platforms.

```bash
$ echo 'my @a = (1..5); print join(", ", map { $_ * 2 } @a), "\n";' | ./pl2cl | sbcl --load cl/pcl-runtime.lisp --load /dev/stdin
2, 4, 6, 8, 10
```

## What Works

- **Operators** — all 92 Perl precedence levels, chained comparisons, string ops
- **Control flow** — `if/elsif/unless`, `while/until`, `for/foreach`, loop labels, `next/last/redo`
- **Subroutines** — signatures, defaults, prototypes, closures, `wantarray`
- **References** — `\$x`, `$$ref`, `$aref->[0]`, `@{$ref}`, anonymous constructors
- **OO** — `bless`, method calls, `@ISA`, C3 MRO, multiple inheritance, `SUPER::`
- **Built-ins** — `print/say`, `push/pop/shift/unshift/splice`, `map/grep/sort`, `sprintf`, `chomp/chop`, `length/substr/index`, `each/keys/values` (hashes and arrays), `open/close/readline`, `die/eval`, `tie/untie`, regex `m//`/`s///`/`tr///`, and more
- **Filehandles** — bareword (`F`) and lexical (`my $fh`) handles, `__DATA__`/`__END__`
- **Packages** — `package Foo { }` block scoping, `use constant`, `BEGIN`, `use`/`require`
- **Special vars** — `$_`, `@_`, `$!`, `$/`, `$\`, `$,`, `$"`, `$0`, `@INC`, `%ENV`, …
- **Regex** — full `m//`/`s///`/`tr///` with modifiers, named captures, `$1`…

## Quick Start

```bash
# Dependencies: Perl 5.30+, PPI, Moo, SBCL, cl-ppcre (via Quicklisp)
cpanm PPI Moo
sbcl --eval '(ql:quickload :cl-ppcre)' --quit

# Transpile and run
echo 'print "Hello, World!\n";' | ./pl2cl | sbcl --noinform --load cl/pcl-runtime.lisp --load /dev/stdin

# Run test suite (51 files, 2462 tests)
prove -j8 Pl/t/
```

## Architecture

```
Perl Source → PPI → Pl::PExpr (AST) → Pl::ExprToCL → Common Lisp
                                                            ↓
                                                   pcl-runtime.lisp
                                                (Perl semantics in CL)
```

| Module | Purpose |
|--------|---------|
| `Pl/Parser.pm` | Statement-level parser |
| `Pl/PExpr.pm` | Expression parser, operator precedence |
| `Pl/ExprToCL.pm` | Code generator |
| `cl/pcl-runtime.lisp` | Runtime library (~6000 lines of CL) |

Generated code is intentionally readable — Perl variables keep their sigils (`$x`, `@array`, `%hash`), and functions map to `pl-` prefixed names (`pl-print`, `pl-push`, …).

## Example

```perl
# input.pl
package Animal;
sub new { bless { name => $_[1] }, $_[0] }
sub speak { "I am " . $_[0]->{name} }

package Dog;
our @ISA = ('Animal');
sub speak { $_[0]->SUPER::speak() . " and I bark" }

package main;
my $d = Dog->new("Rex");
print $d->speak(), "\n";
```

```bash
$ ./pl2cl input.pl | sbcl --load cl/pcl-runtime.lisp --load /dev/stdin
I am Rex and I bark
```

## Status

**Beta.** The test suite runs 2462 tests comparing PCL output directly against Perl's output. A broad sweep against Perl's own internal test suite (`t/op/`, `t/base/`, etc.) passes ~3100 tests.

Known gaps: string `eval`, some `local` forms, XS/C extensions.

My Common Lisp experience is from long ago — that part is exclusively Claude.

*(If this doesn't get shot down too hard, I'll put it on CPAN later.)*

## License

This library is free software; you can redistribute it and/or modify it under the same terms as Perl itself.
