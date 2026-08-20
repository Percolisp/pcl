# Plan: Expression Code Generator (Perl → Common Lisp)

## Goal
Create `Pl/ExprToCL.pm` - a code generator that transforms the PExpr AST into Common Lisp code following the conventions in CODEGEN_DESIGN.md.

## Design Conventions (from CODEGEN_DESIGN.md)
- Variables: Keep Perl sigils (`$x`, `@arr`, `%hash`)
- **ALL operators/functions: `pl-` prefix** (consistent, no confusion)
- All operators as macro calls (not functions)
- Pretty-printed output for readability

## Files to Create/Modify

### New File: `Pl/ExprToCL.pm`
Main code generator module, parallel to PExpr.pm.

## Implementation

### Core Structure
```perl
package Pl::ExprToCL;
use Moo;

has expr_o => (is => 'ro', required => 1);  # Pl::PExpr object
has indent => (is => 'rw', default => 0);   # Current indentation

sub generate {
  my ($self, $node_id) = @_;
  # Main entry point - returns CL code string
}
```

### Node Type Handlers

| AST Type | CL Output |
|----------|-----------|
| Binary op (`+`, `-`, `*`, etc.) | `(pl-+ a b)` |
| String concat (`.`) | `(pl-. a b)` |
| Comparison (`==`, `<`, etc.) | `(pl-== a b)`, `(pl-< a b)` |
| String cmp (`eq`, `lt`, etc.) | `(pl-eq a b)`, `(pl-lt a b)` |
| Logical (`&&`, `||`) | `(pl-&& a b)`, `(pl-|| a b)` |
| Assignment (`=`) | `(pl-setf var val)` |
| `prefix_op` | `(pl-OP operand)` |
| `postfix_op` | `(pl-OP-post operand)` |
| `funcall` | `(pl-FUNC args...)` |
| `methodcall` | `(pl-method-call obj 'method args...)` |
| `ternary` | `(pl-if cond then else)` |
| `a_acc` | `(pl-aref arr idx)` |
| `h_acc` | `(pl-gethash hash key)` |
| `arr_init` | `(list ...)` |
| `hash_init` | `(pl-hash ...)` |
| `progn` | `(progn ...)` |
| Variable (`$x`) | `$x` (literal symbol) |
| Number | number literal |
| String | `"string"` |

### Operator Mapping Table (ALL use `pl-` prefix)
```perl
my %OP_MAP = (
  # Arithmetic
  '+'  => 'pl-+',  '-'  => 'pl--',  '*'  => 'pl-*',  '/'  => 'pl-/',
  '%'  => 'pl-%',  '**' => 'pl-**',

  # String
  '.'  => 'pl-.',  'x'  => 'pl-x',

  # Comparison (numeric)
  '==' => 'pl-==', '!=' => 'pl-!=', '<'  => 'pl-<',  '>'  => 'pl->',
  '<=' => 'pl-<=', '>=' => 'pl->=', '<=>'=> 'pl-<=>',

  # Comparison (string)
  'eq' => 'pl-eq', 'ne' => 'pl-ne', 'lt' => 'pl-lt', 'gt' => 'pl-gt',
  'le' => 'pl-le', 'ge' => 'pl-ge', 'cmp'=> 'pl-cmp',

  # Logical
  '&&' => 'pl-&&', '||' => 'pl-||', '//' => 'pl-//',
  'and'=> 'pl-and','or' => 'pl-or', 'xor'=> 'pl-xor',
  '!'  => 'pl-!',  'not'=> 'pl-not',

  # Bitwise
  '&'  => 'pl-&',  '|'  => 'pl-|',  '^'  => 'pl-^',
  '~'  => 'pl-~',  '<<' => 'pl-<<', '>>' => 'pl->>',

  # Assignment
  '='  => 'pl-setf',
  '+=' => 'pl-+=', '-=' => 'pl--=', '*=' => 'pl-*=', '/=' => 'pl-/=',
  '.=' => 'pl-.=', '||='=> 'pl-||=','&&='=> 'pl-&&=','//='=> 'pl-//=',

  # Range
  '..' => 'pl-..',

  # Regex
  '=~' => 'pl-=~', '!~' => 'pl-!~',

  # Increment/Decrement
  '++' => 'pl-++', '--' => 'pl---',

  # Reference
  '\\' => 'pl-ref',
);
```

### Key Methods

```perl
sub generate($node_id)        # Main dispatch
sub gen_binary($op, $kids)    # Binary operators
sub gen_prefix($op, $kid)     # Prefix operators
sub gen_funcall($kids)        # Function calls
sub gen_methodcall($kids)     # Method calls
sub gen_ternary($kids)        # Ternary ?:
sub gen_access($type, $kids)  # Array/hash access
sub gen_leaf($node)           # Literals and variables
sub emit($form)               # Format output with indentation
```

### Output Format
Pretty-printed, one form per line for complex expressions:
```lisp
(pl-setf $total
  (pl-+ $price
    (pl-* $price $tax_rate)))
```

## Test Strategy

Add `Pl/t/codegen-01.t`:
- Test simple expressions: `$x + $y` → `(pl-+ $x $y)`
- Test nested: `$x * ($y + $z)` → `(pl-* $x (pl-+ $y $z))`
- Test all operator types
- Test function calls
- Test ternary

## Implementation Order

1. Core structure and dispatch
2. Leaf nodes (variables, literals)
3. Binary operators (arithmetic, comparison)
4. Prefix/postfix operators
5. Function calls
6. Access expressions (array, hash)
7. Ternary
8. Complex expressions (progn, method calls)

## Not in Scope (for now)
- Statement-level code (blocks, loops, subs)
- Declaration handling (my, our, local)
- Context-aware code generation
- Optimization passes
