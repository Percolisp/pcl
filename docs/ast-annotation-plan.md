# PCL: Two-Phase AST Annotation Plan

**Written:** 2026-04-05 (session 119)
**Status:** Planned — not yet implemented

---

## Problem

The transpiler's code generator (`Pl/ExprToCL.pm`) currently makes structural decisions *during* code generation using ad-hoc approaches:

1. **String matching on generated CL** (`gen_tree_val` line ~2015):
   ```perl
   if ($child =~ /\(p-=~\s/) { return "(let ((*wantarray* t)) $child)"; }
   ```
   Breaks if CL output format changes. Replaced correctly in session 119 for `map`/`sort`/`grep` via `_child_is_list_expr`, but `=~` still uses string matching.

2. **Hardcoded function list** in `_child_is_list_expr` (~12 functions). New list-returning builtins must be manually added there. User-defined subs that return lists are invisible.

3. **`lvalue_context` mutable flag** threaded through recursive codegen calls. Example: `gen_prefix_op` sets it to 1 before recursing into `++$arr[0]`, then `gen_array_access` reads it to choose `p-aref-box`. This is stateful and error-prone.

4. **`$$var++` shape inspection** in `gen_prefix_op` inspects AST node structure at codegen time to detect the flipped order of operators.

---

## Solution: Two-Phase Pipeline

```
Perl Source
    → PPI
    → parse_expr_to_tree()     [PExpr.pm]
    → annotate_contexts()      [PExpr.pm — already exists]
    → ASTAnnotator::annotate() [NEW: Pl/ASTAnnotator.pm]
    → ExprToCL::generate()     [reads annotations from node metadata]
    → CL output
```

The `ASTAnnotator` walks the complete AST once and stores type information as node metadata using the existing `OpcodeTree::set_metadata(node_id, key, value)` API. The codegen handlers then read these annotations with `get_metadata(node_id, key)` instead of doing ad-hoc checks.

---

## Annotation Taxonomy

### A. `returns_list` (boolean)

**Semantics:** "This expression produces a vector (list) when evaluated in list context."

**Set on** (bottom-up walk — children must be annotated before parents):
- `@arr`, `@_` leaf tokens (array sigil)
- `funcall` nodes whose first child is one of these built-in functions:
  `map grep sort split reverse keys values each unpack readdir localtime gmtime caller stat lstat getpwent getgrent getpwnam getpwuid getgrgid getgrnam`
- `=~` operator nodes when the node's context (from `annotate_contexts`) is `LIST_CTX`
- `tree_val` with multiple children (a comma list)
- `tree_val` with one child where the child has `returns_list=1` (propagation)
- `progn` (sequence of expressions — list context)
- `slice_a_acc`, `slice_h_acc`, `kv_slice_a_acc`, `kv_slice_h_acc` (slices always return lists)

**Used in** `gen_tree_val`: if a single-child parenthesized expression in list context has `returns_list=1`, skip the `(vector ...)` wrapper.

**Replaces:** `_child_is_list_expr()` method (hardcoded list) and the `p-=~` string match.

---

### B. `needs_wantarray` (boolean)

**Semantics:** "This expression needs `(let ((*wantarray* t)) ...)` wrapping to activate list behavior."

**Set on:** `=~` operator nodes in `LIST_CTX` (same condition as `returns_list` for `=~`, but `@arr` does NOT need this wrapper).

**Used in** `gen_tree_val`: when `needs_wantarray=1`, wrap with `(let ((*wantarray* t)) expr)` instead of returning bare.

**Why separate from `returns_list`:** An `@arr` already IS a vector — no wrapper needed. An `=~` match in list context returns captures, but only if `*wantarray*` is `t` at runtime. These are different.

---

### C. `lvalue` (boolean)

**Semantics:** "This expression's result is being stored into, modified in-place, or its address is being taken."

**Set on** (top-down walk — parent context propagates to children):
- LHS of `=`, `+=`, `-=`, `.=`, `||=`, `&&=`, `//=`, and all compound assigns
- Operand of prefix `\` (reference-taking: `\$x`, `\@arr`)
- Operand of prefix `++`/`--` (pre-increment/decrement)
- Operand of postfix `++`/`--` (post-increment/decrement)
- First argument of `chomp`, `chop`, `undef`, `local`, `delete`, `splice`, `push`, `pop`, `shift`, `unshift` (functions that modify their first arg)
- Does NOT propagate into: index/key children of `a_acc`/`h_acc`; RHS of assignments; most funcall arguments

**Used in:**
- `gen_array_access`: if `lvalue=1`, emit `(p-aref-box arr idx)` (boxed, writable); else `(p-aref arr idx)` (unboxed, read-only)
- `gen_hash_access`: if `lvalue=1`, emit `(p-gethash-box hash key)`; else `(p-gethash hash key)`

**Replaces:** The `lvalue_context` mutable flag in `ExprToCL` that currently gets saved/restored around recursive calls.

---

### D. `numeric_context` / `string_context` (future)

**Semantics:** "The result of this expression will be coerced to a number / string."

**Set on:** Operands of arithmetic operators (`+`, `-`, `*`, `/`, `%`, `**`) and comparison operators (`<`, `>`, `<=`, `>=`, `==`, `!=`, `<=>`, `<<`, `>>`) for numeric; string operators (`.`, `eq`, `ne`, `lt`, `gt`, `le`, `ge`, `cmp`) for string.

**Future benefit:** Could elide `to-number` / `to-string` coercions in generated CL when the operand's type is already known. **Not needed for correctness — deferred.**

---

## Implementation

### New File: `Pl/ASTAnnotator.pm`

```perl
package Pl::ASTAnnotator;
use v5.30; use strict; use warnings;
use Moo;

has expr_o => (is => 'ro', required => 1);

# Main entry point. Run after annotate_contexts().
sub annotate {
    my ($self, $root_id) = @_;
    $self->_annotate_returns_list($root_id);  # bottom-up (post-order)
    $self->_annotate_lvalue($root_id, 0);     # top-down (pre-order)
}
```

**Walk strategies:**
- `_annotate_returns_list`: iterative post-order using a two-pass stack (same pattern as `annotate_contexts`). Processes children before parents.
- `_annotate_lvalue($node_id, $is_lvalue)`: recursive pre-order. Depth is bounded (Perl expressions rarely nest > 30 levels).

### Changes to `Pl/ExprToCL.pm`

**`generate()` — add annotator call:**
```perl
sub generate {
    my ($self, $node_id) = @_;
    my $ann = Pl::ASTAnnotator->new(expr_o => $self->expr_o);
    $ann->annotate($node_id);
    return ($self->indent_str x $self->indent_level) . $self->gen_node($node_id);
}
```

**`gen_tree_val` — replace fragile checks:**
```perl
# Before:
my $child_is_list = ($ctx == 1) && $self->_child_is_list_expr($kids->[0]);
my $child = $self->gen_node($kids->[0]);
if ($ctx == 1) {
    if ($child =~ /\(p-=~\s/) { return "(let ((*wantarray* t)) $child)"; }
    return $child_is_list ? $child : "(vector $child)";
}

# After:
my $tree = $self->expr_o->node_tree;
my $rl   = $tree->get_metadata($kids->[0], 'returns_list');
my $wa   = $tree->get_metadata($kids->[0], 'needs_wantarray');
my $child = $self->gen_node($kids->[0]);
if ($ctx == 1) {
    return "(let ((*wantarray* t)) $child)" if $wa;
    return $rl ? $child : "(vector $child)";
}
```

**`gen_array_access` — annotation-based lvalue:**
```perl
my $func = $self->expr_o->node_tree->get_metadata($node_id, 'lvalue')
           ? 'p-aref-box' : 'p-aref';
```

**`gen_hash_access` — annotation-based lvalue:**
```perl
my $func = $self->expr_o->node_tree->get_metadata($node_id, 'lvalue')
           ? 'p-gethash-box' : 'p-gethash';
```

**Transition strategy:** Keep `_child_is_list_expr` as a fallback during Phase 1 development:
```perl
my $rl = $tree->get_metadata($kids->[0], 'returns_list')
         // (($ctx==1) && $self->_child_is_list_expr($kids->[0]));
```
Remove the fallback and delete `_child_is_list_expr` once Phase 1 tests pass.

---

## Critical Edge Cases

| Case | Issue | Handling |
|------|-------|----------|
| `inline_lambda` nodes | `body_cl` is pre-compiled CL, no AST children | Stop recursion when `node->{type} eq 'inline_lambda'` |
| `=~` in two AST forms | PPIreference `{type}='=~'` OR `PPI::Token::Operator` `.content eq '=~'` | Check both in annotator |
| `annotate_contexts` ordering | `=~` LIST_CTX check reads context metadata | Annotator runs AFTER `annotate_contexts` — already guaranteed by call order |
| `$$var++` | Currently needs shape inspection | After `lvalue` annotation: `$` cast does not propagate lvalue; annotation handles correctly |
| Compound assigns | Stored as `PPI::Token::Operator` not PPIreference | Check `$node->content() =~ /^[+\-*\/.%&|^]=$|^(?:&&\|\|\/\/)=$|^(?:<<|>>)=$/` |
| `tree_val` wrapping lvalue | `(++$arr[0])` — outer `tree_val` is lvalue | Propagate `lvalue` downward through `tree_val` |

---

## Files

| File | Status | Role |
|------|--------|------|
| `Pl/ASTAnnotator.pm` | **New** | All annotation walk logic |
| `Pl/ExprToCL.pm` | Modify | Use annotations; remove `lvalue_context` threading; remove `_child_is_list_expr` |
| `Pl/OpcodeTree.pm` | No change | `set_metadata`/`get_metadata` already sufficient |
| `Pl/PExpr.pm` | No change | `annotate_contexts` already sets `context` metadata |
| `Pl/t/ast-annotator-01.t` | **New** | Unit tests: verify annotation values directly |
| `Pl/t/ast-annotator-codegen-01.t` | **New** | Integration tests: verify codegen output changed correctly |

---

## Estimated Size

- `Pl/ASTAnnotator.pm`: ~130 lines
- Changes to `Pl/ExprToCL.pm`: ~40 lines changed, ~60 lines removed (lvalue threading + `_child_is_list_expr`)
- New test files: ~80 lines each

---

## Implementation Order

1. **Phase 1** (highest ROI): `returns_list` + `needs_wantarray` annotations → fix `gen_tree_val`
2. **Phase 2** (medium ROI): `lvalue` annotation → remove `lvalue_context` threading
3. **Phase 3** (deferred): `numeric_context`/`string_context` for coercion optimization
