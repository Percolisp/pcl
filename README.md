# PCL — Perl to Common Lisp Transpiler

**PCL** converts Perl source code to Common Lisp, aiming for enough compatibility to run real CPAN modules. It parses Perl with [PPI](https://metacpan.org/pod/PPI), builds an AST with proper operator precedence, and generates readable CL code.

There are two motivations. The first is simply running Perl code in a Common Lisp environment. The second is that the generated CL serves as a **portable intermediate representation**: Common Lisp is high-level enough to express Perl semantics cleanly, yet is easy to parse — lowering the threshold for compiling Perl to new platforms.

```bash
$ echo 'my @a = (1..5); print join(", ", map { $_ * 2 } @a), "\n";' | ./pl2cl | sbcl --load cl/pcl-runtime.lisp --script /dev/stdin
2, 4, 6, 8, 10
```

## What Works

- **Operators** — all 92 Perl precedence levels, chained comparisons, string ops
- **Control flow** — `if/elsif/unless`, `while/until`, `for/foreach`, loop labels, `next/last/redo`
- **Subroutines** — signatures, defaults, prototypes, closures, `state` variables
- **References** — `\$x`, `$$ref`, `$aref->[0]`, `@{$ref}`, anonymous constructors, postfix deref (`->@*`, `->%*`, `->$*`)
- **OO** — `bless`, method calls, `@ISA`, C3 MRO, multiple inheritance, `SUPER::`, `AUTOLOAD`, `UNIVERSAL`, `use overload`
- **Built-ins** — `print/say`, `push/pop/shift/unshift/splice`, `map/grep/sort`, `sprintf`, `chomp/chop`, `length/substr/index`, `each/keys/values`, `open/close/readline`, `die/eval`, `tie/untie`, regex `m//`/`s///`/`tr///`, and more
- **Filehandles** — bareword (`FH`) and lexical (`my $fh`) handles, `__DATA__`/`__END__`
- **Packages** — `package Foo { }` block scoping, `use constant`, `BEGIN`, `use`/`require`, `use base`/`use parent`
- **Special vars** — `$_`, `@_`, `$!`, `$/`, `$\`, `$,`, `$"`, `$0`, `$.`, `@INC`, `%ENV`, …
- **Regex** — full `m//`/`s///`/`tr///` with modifiers, named captures `%+`, `$1`…
- **String `eval`** — `eval "code"` transpiles and runs at runtime via a persistent subprocess
- **`local`** — scalars, arrays, hashes, hash/array elements, typeglobs

## Known Gaps

- **`pack`/`unpack`** — missing several format letters
- **`wantarray`** — `eval "string"` void context not yet propagated, rest seems ok.
- **XS/C extensions** — anything that requires compiled C code won't work

## Limitations

### Deprecated or removed in modern Perl

These features were removed in Perl 5.34–5.38 and are not implemented:

| Feature | Status in Perl |
|---------|---------------|
| `given`/`when`, `~~` smart match | Removed in 5.38 |
| `?pattern?` one-match regex | Removed in 5.38 |
| `reset()` for `?pattern?` | Removed in 5.38 |

### Intentional design decisions

- **`@_` argument aliasing** — PCL copies args into `@_`; `$_[0] = 42` does not modify the caller's variable. CL function arguments are values, not aliases.
- **`caller()` location** — package name is correct; filename is always `"(unknown)"`, line always `0`. CL does not expose Perl-compatible source locations at runtime.
- **Error message text** — PCL does not guarantee error messages match Perl's wording or the `" at FILE line N"` suffix. PCL targets correct execution of valid CPAN code, not error compatibility.
- **`$SIG{__DIE__}` handler** — not invoked; would require CL condition restarts.
- **refaliasing** — experimental.

### Not emulated (Perl internals / niche features)

- **Boolean identity** — `!0`/`!1` return fresh values each call; taking `\!0` twice gives different addresses.
- **Read-only scalars** — `Internals::SvREADONLY`, `\undef` stash tricks, and read-only constants via `BEGIN { $::{z} = \undef }` are not emulated.
- **`prototype()`** — always returns `undef`; prototype strings are not stored at parse time.
- **`__SUB__`** — not recognized; use a named sub or a captured `$self = sub { ... }`.
- **Lvalue subs** — `: lvalue` attribute not implemented; use four-arg `substr` instead.
- **`format`/`write`** — Perl's report-formatting system is essentially unused in modern CPAN code; not implemented.
- **Regex code blocks** — `(?{code})` and `(??{code})` are not supported; CL-PPCRE has no equivalent hook.
- **Regex locale modifiers** — `/a`, `/d`, `/l`, `/u` are accepted but ignored; CL-PPCRE always uses Unicode semantics.
- **Hex float literals** — `0x1.8p+1` — PPI does not tokenize these correctly.
- **`use integer` edge cases** — extreme shift counts and C-level overflow corner cases differ from Perl's C runtime.

### Unicode

CL strings are always Unicode; Perl's per-scalar UTF-8 flag does not exist. Divergences:
- `utf8::encode`/`utf8::decode` — the byte/character distinction is not meaningful in PCL.
- `use bytes` — not implemented.
- `\p{IsWord}` in CL-PPCRE does not reliably match non-ASCII word characters.

## Quick Start

```bash
# Dependencies: Perl 5.30+, PPI, Moo, SBCL, cl-ppcre (via Quicklisp)
cpanm PPI Moo
sbcl --eval '(ql:quickload :cl-ppcre)' --quit

# Transpile and run
echo 'print "Hello, World!\n";' | ./pl2cl | sbcl --noinform --load cl/pcl-runtime.lisp --script /dev/stdin

# Run test suite (75 files, 2928 tests)
prove -j8 Pl/t/
```

## Architecture

```
Perl Source → PPI → Pl::PExpr (AST) → Pl::ExprToCL → Common Lisp
                          ↓                                  ↓
                  Pl::BlockAnalyzer                 pcl-runtime.lisp
               (two-phase scope analysis)       (Perl semantics in CL)
```

| Module | Purpose |
|--------|---------|
| `Pl/Parser.pm` | Statement-level parser |
| `Pl/PExpr.pm` | Expression parser, operator precedence |
| `Pl/BlockAnalyzer.pm` | Two-phase block analysis (declaration scoping) |
| `Pl/ExprToCL.pm` | Code generator |
| `cl/pcl-runtime.lisp` | Runtime library (~7000 lines of CL) |

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
$ ./pl2cl input.pl | sbcl --load cl/pcl-runtime.lisp --script /dev/stdin
I am Rex and I bark
```

## Next: AST Annotation Pass

The plan now is to find incompatibilities with real Perl, then to make more of a real compiler to generate better variable handling so they can be tagged as numeric etc.

The planned fix is a pre-generation annotation pass over the OpcodeTree:

- **Phase 0 — variable resolution** (`Pl::VarAnnotator`): a scope-stack walk that annotates every variable reference with `var_kind` (`my`/`our`/`local`/`package`/`special`) and flags closure-captured `my` declarations. Replaces the current parse-time `__lex__N` renaming.
- **Phase 0b — unboxing analysis**: marks `my $scalar` declarations that are never passed by reference, tied, or written inside a closure; codegen can then emit a plain CL `let` binding instead of a heap-allocated `p-box`.
- **Phase 1 — `returns_list` / `needs_wantarray`** (`Pl::ASTAnnotator`): bottom-up walk marks expressions that produce a vector, eliminating the hardcoded function list and the `p-=~` string match.
- **Phase 2 — `lvalue`**: top-down walk propagates the assignment context, replacing the stateful `lvalue_context` flag.

See `docs/ast-annotation-plan.md` for the full design.

## Status

**Beta.** The internal test suite runs 2928 tests across 75 files, all passing. A broad sweep against Perl's own test suite (`t/op/`, `t/base/`, etc.) passes **~88%** of tests, with 39 files passing completely.

My Common Lisp experience is from long ago — that part is exclusively Claude.

*(I'll put it on CPAN later, when it is closer to ready.)*

## License

This library is free software; you can redistribute it and/or modify it under the same terms as Perl itself.
