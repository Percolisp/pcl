# Pl::PExpr - Expression Parser for PPI

**Pl::PExpr** is an expression parser that extends [PPI](https://metacpan.org/pod/PPI). It takes PPI token arrays and produces an Abstract Syntax Tree (AST) suitable for code generation, with proper operator precedence and context annotation.

## Motivation

PPI parses Perl source code into tokens and basic structure, but doesn't build an expression tree with operator precedence. Pl::PExpr fills this gap, making it possible to:

- Generate code for other languages from Perl expressions
- Analyze expression structure with proper precedence
- Track scalar vs list context through expressions

This distribution includes **PCL** (Perl to Common Lisp), a prototype transpiler that demonstrates using Pl::PExpr for code generation. The tests transpile Perl code, execute it in both Perl and SBCL, and compare output.

## Quick Start

```perl
use Pl::PExpr;
use PPI;

# Parse an expression
my $doc    = PPI::Document->new(\'$x + $y * 2');
my @tokens = $doc->children->[0]->children;

my $parser = Pl::PExpr->new(
    e        => \@tokens,
    full_PPI => $doc,
);

my ($root_id, $declarations) = $parser->parse_expr_to_tree();

# Access the AST
my $tree      = $parser->node_tree;
my $root_node = $tree->node_data($root_id);
my $children  = $tree->children_ids($root_id);
```

## What Pl::PExpr Handles

- **Operator precedence** - all 92 Perl precedence levels
- **All operators** - arithmetic, logical, string, bitwise, ternary `?:`
- **Function and method calls** - `foo($x)`, `$obj->method()`, `Class->new()`
- **Array and hash access** - `$a[0]`, `$h{key}`, slices, nested access
- **References and dereferences** - `\$x`, `$$ref`, `$aref->[0]`, `@{$ref}`
- **String interpolation** - `"Hello $name"`, `"Value: @{[$x+1]}"`
- **Anonymous subs** - `sub { ... }`
- **Variable declarations** - `my`, `our`, `state`, `local`
- **Context annotation** - scalar vs list context tracking
- **Regex** - `m//`, `s///`, `tr///` with modifiers
- **Diamond operator** - `<FH>`, `<$fh>`

## Example: Parse Tree

```bash
$ perl examples/parse_expr.pl '$a + $b * $c'
AST for: $a + $b * $c
----------------------------------------
[4] binop(+)
  [0] Token::Symbol: $a
  [3] binop(*)
    [1] Token::Symbol: $b
    [2] Token::Symbol: $c
```

```bash
$ perl examples/parse_expr.pl '$obj->foo->bar($x)'
AST for: $obj->foo->bar($x)
----------------------------------------
[0] methodcall
  [5] methodcall
    [3] Token::Symbol: $obj
    [4] Token::Word: foo
  [2] Token::Word: bar
  [1] Token::Symbol: $x
```

## PCL Transpiler (Prototype)

The distribution includes a working Perl-to-Common-Lisp transpiler:

```bash
$ echo 'my $x = 1 + 2; print $x;' | ./pl2cl
(in-package :pcl)

;; my $x = 1 + 2
(pl-setf $x (pl-+ 1 2))

;; print $x
(pl-print $x)
```

The idea is to make a simple compiler that can make pure Perl run in
other environments, as a basis for continuing work. The "simple" part
might be failing.

It has just reached the phase of using simple CPAN code for compiling
and running, to shake out errors. Hopefully it will reach alpha stage
in a few weeks.

### Running Tests

```bash
# Run all tests (requires SBCL for CL execution tests)
prove Pl/t/

# Run a specific test
prove -v Pl/t/codegen-01.t
```

### Architecture

```
Perl Source → PPI → Pl::PExpr (AST) → Pl::ExprToCL → Common Lisp
                         ↓
               Pl::Environment (constants, prototypes)
```

| Module | Purpose |
|--------|---------|
| `Pl/PExpr.pm` | Expression parser - **the main module** |
| `Pl/Parser.pm` | Statement-level parser |
| `Pl/ExprToCL.pm` | Code generator (Perl AST → CL) |
| `Pl/Environment.pm` | Tracks constants, prototypes, packages |
| `Pl/OpcodeTree.pm` | AST node storage |

## Dependencies

- Perl 5.30+
- [PPI](https://metacpan.org/pod/PPI) - Perl parser
- [Moo](https://metacpan.org/pod/Moo) - Object system
- [SBCL](http://www.sbcl.org/) - For running transpiler tests (optional)

## Installation

### Ubuntu/Debian

```bash
# Perl dependencies
sudo apt install perl cpanminus
cpanm PPI Moo Test::More

# Common Lisp (for running transpiled code)
sudo apt install sbcl libpcre3-dev build-essential

# Install Quicklisp (CL package manager)
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp \
     --eval '(quicklisp-quickstart:install)' \
     --eval '(ql:add-to-init-file)' \
     --quit

# Install CL dependencies (run once in SBCL)
sbcl --eval '(ql:quickload :cl-ppcre)' --quit
```

### macOS

```bash
# Perl dependencies
cpanm PPI Moo Test::More

# Common Lisp
brew install sbcl pcre

# Install Quicklisp and cl-ppcre (same as above)
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp \
     --eval '(quicklisp-quickstart:install)' \
     --eval '(ql:add-to-init-file)' \
     --quit
sbcl --eval '(ql:quickload :cl-ppcre)' --quit
```

### Verify Installation

```bash
# Run tests
prove Pl/t/

# Quick transpile test
echo 'print "Hello World\n";' | ./pl2cl
```

## Status

**Pl::PExpr**: Beta level expression parser

**PCL transpiler**: Prototype - handles expressions, control flow, subroutines, OO basics. See `CLAUDE.md` for detailed implementation status.

My Common Lisp experience is from long ago, that part is exclusively Claude.

(If this doesn't get shot down too hard, I'll put it on CPAN later.)

## License

This library is free software; you can redistribute it and/or modify it under the same terms as Perl itself.
