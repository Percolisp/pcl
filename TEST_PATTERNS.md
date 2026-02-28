# Test Patterns for Pl::PExpr

## Basic Test Structure

```perl
use v5.32;
use strict;
use warnings;

use lib ".";

use Data::Dump qw/dump/;
use PPI;
use PPI::Dumper;
use Test::More tests => N;

BEGIN { use_ok('Pl::PExpr') };
```

## Helper Functions

### `_get_ppi_part($doc, $stmt_ix)` - Extract expression from PPI document

```perl
sub _get_ppi_part {
  my $doc     = shift;
  my $stmt_ix = shift // 0;

  my @stmts;
  my @parts;
  if (ref($doc) eq 'PPI::Document') {
    @stmts    = $doc->children();
    @parts    = $stmts[$stmt_ix]->children();
  } elsif (ref($doc) eq 'PPI::Statement') {
    @parts    = $doc->children();
  } else {
    die "Code is not PPI::Document?? Is: " . ref($doc);
  }

  return \@parts;
}
```

### Basic Parsing Pattern

```perl
my $code    = '$foo + 10';
my $doc     = PPI::Document->new(\$code);
my $expr    = _get_ppi_part($doc);
my $expr_o  = Pl::PExpr->new(e => $expr);
my $node_id = $expr_o->parse_expr_to_tree($expr);
```

### Inspecting Results

```perl
my($node)     = $expr_o->get_nodes($node_id);
my $kid_ids   = $expr_o->get_node_children($node_id);
my(@kid_nodes)= $expr_o->get_nodes(@$kid_ids);

# Check node type
is(ref $node, "PPIreference", "Is internal node");
is($node->{type}, "funcall", "Type is funcall");

# Check content of PPI tokens
is($kid_nodes[0]->content(), "foo", "Sub name is foo");
```

### Structured Test Pattern: `test_expr()`

Tests can specify expected tree structure as nested arrays:

```perl
test_expr('$foo . "bar" . $duh',
          ['.',
           ['.', '$foo', '"bar"'],
           '$duh',]
       );

test_expr('$foo ** 3 ** 4', ['**', '$foo', ['**', 3, 4]]); # right associative
test_expr('$foo - 3 - 4', ['-', ['-', '$foo', 3],  4]);    # left associative
```

Format: `[operator, arg1, arg2, ...]` where args can be strings (leaf content) or nested arrays (subtrees).

## Debug Output

Enable debug with:
```perl
Pl::PExpr::SET_DEBUG(1);  # Basic
Pl::PExpr::SET_DEBUG(1 + 2);  # More verbose
Pl::PExpr::SET_DEBUG(1 + 8);  # Subcall handling
```

Dump tree:
```perl
say $expr_o->debug_dump_tree($node_id);
```

## Node Types (Internal Nodes)

- `funcall` - Function call: `foo(1, 2)`
- `methodcall` - Method call: `$obj->method()`
- `ref_funcall` - Code ref call: `$ref->()`
- `a_acc` - Array access: `$arr[0]`
- `h_acc` - Hash access: `$hash{key}`
- `a_ref_acc` - Array ref access: `$ref->[0]`
- `h_ref_acc` - Hash ref access: `$ref->{key}`
- `slice_a_acc` - Array slice: `@arr[0,1]`
- `slice_h_acc` - Hash slice: `@hash{qw(a b)}`
- `arr_init` - Array initializer: `[1, 2, 3]`
- `hash_init` - Hash initializer: `{a => 1}`
- `progn` - Comma-separated list
- `tree_val` - Parenthesized expression value
- `prefix_op` - Prefix operator: `!$x`, `\$x`
- `postfix_op` - Postfix operator: `$x++`
- `ternary` - Ternary: `$x ? $y : $z`

## Checking Node Types

```perl
if ($expr_o->is_internal_node_type($node)) {
  # It's a PPIreference with ->{type}
  my $type = $node->{type};
} else {
  # It's a PPI token
  my $content = $node->content();
}
```

## PPI Token Classes (Leaves)

- `PPI::Token::Symbol` - Variables: `$x`, `@arr`, `%hash`
- `PPI::Token::Number` - Numbers: `42`, `3.14`
- `PPI::Token::Quote::Single` - Single-quoted: `'foo'`
- `PPI::Token::Quote::Double` - Double-quoted: `"foo"`
- `PPI::Token::Operator` - Operators: `+`, `->`, etc.
- `PPI::Token::Word` - Barewords/functions: `foo`, `print`
- `PPI::Token::Cast` - Deref sigils: `@`, `%`, `$` in `@$ref`
- `PPI::Token::Regexp::Match` - Match regex: `/pattern/`
- `PPI::Token::Magic` - Special vars: `$_`, `$/`, `$!`

## Investigating PPI Structure

Before writing parser code, dump PPI structure:

```bash
perl -MPPI -MPPI::Dumper -e '
my $doc = PPI::Document->new(\"$$ref\");
PPI::Dumper->new($doc)->print;
'
```

## Running Tests

Run all tests from the `pcl` directory (not from `Pl/`):

```bash
prove Pl/t/           # Run all tests
prove -v Pl/t/        # Verbose output
prove Pl/t/expr-02.t  # Run single test file
```

## Known Failing Tests

### `anon-sub-01.t` - Code Blocks in Expressions

This test file tests `map { block } @array` and `grep { block } @list` patterns.
These currently fail because the expression parser doesn't handle code blocks
inside expressions yet.

```perl
# These patterns are NOT YET SUPPORTED:
map { $_ * 2 } @array
grep { $_ > 0 } @list
sort { $a <=> $b } @array
```

**Status:** Out of scope for V1 (requires statement parser for block contents)

**Workaround:** Use the expression form instead:
```perl
# These WORK:
map($_ * 2, @array)    # Expression form (no block)
```
