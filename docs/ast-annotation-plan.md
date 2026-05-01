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
    → parse_expr_to_tree()       [PExpr.pm]
    → annotate_contexts()        [PExpr.pm — already exists]
    → VarAnnotator::annotate()   [NEW: Phase 0 — scope stack, var_kind, closure_captured, loop_var]
    → ASTAnnotator::annotate()   [NEW: Phase 1–2 — returns_list, needs_wantarray, lvalue]
    → ExprToCL::generate()       [reads annotations from node metadata]
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

### D. Variable Annotations (Phase 0 — prerequisite pass)

**Goal:** Resolve every variable reference in the AST to its declaration, and record how the declaration is used. This pass runs before all expression annotations (A–C) and before any code is generated. It replaces the current ad-hoc mechanisms scattered across `Parser.pm`:

| Current mechanism | Replaced by |
|---|---|
| `_vars_referenced_in_closures` (Parser.pm) | `closure_captured` annotation on `my` decl nodes |
| `$var__lex__N` renaming at parse time | Annotation-driven rename in codegen, not in AST |
| `_insert_variable_forward_declarations` pre-scan | `var_kind` annotation drives `defvar` vs `let` choice |
| Hard-coded `$a`/`$b` always-defvar rule | `sort_special` annotation on `$a`/`$b` uses inside sort blocks |

#### D1. `var_kind` (string: `'my'` | `'our'` | `'local'` | `'package'` | `'special'`)

**Semantics:** How the variable was declared (or used without declaration).

**Set on:** Every variable-leaf node (sigil + name) during a post-order walk.

- `my` — declared with `my` in enclosing lexical scope
- `our` — declared with `our`, alias to package variable
- `local` — declared with `local`, dynamic binding
- `package` — used without `my`/`our`/`local`, implicitly global
- `special` — built-in punctuation/named special vars (`$_`, `@_`, `$!`, `$@`, `$/`, `$a`, `$b`, `%ENV`, etc.)

**Used in:** Codegen decides between `let`-binding (for `my`) and `defvar`/dynamic (for everything else).

#### D2. `var_decl_node` (node ID or `undef`)

**Semantics:** Points to the AST node of the `my`/`our`/`local` declaration that introduced this variable in its current scope. `undef` for package/special variables.

**Set on:** Every variable-leaf node that resolves to a lexical declaration.

**Used in:** Detecting variable shadowing; emitting a single `(let ($x ...) ...)` at the declaration site rather than a forward `(defvar $x)` + later assignment.

#### D3. `closure_captured` (boolean)

**Semantics:** A `my` variable is `closure_captured` if it is referenced inside a nested `sub {}` or `do {}` block that outlives the declaring scope.

**Set on:** The declaration node (`my $x`) — not the use sites.

**Used in:** Codegen wraps the captured variable's `let` binding in a `cons`-cell or moves it to a `defvar` with a unique `__lex__N` suffix, so the closure's `funcall` frame can reach the live binding. Currently this renaming happens at *parse time* (`_vars_referenced_in_closures` + `__lex__N` suffix injection). Moving it here means the AST stays clean; only codegen emits the mangled name.

#### D4. `loop_var` (boolean)

**Semantics:** This `my` declaration is the iteration variable of a `for`/`foreach` loop.

**Set on:** The `my $x` node in `foreach my $x (...)`.

**Used in:** Codegen must create a fresh binding per iteration (not a single `let` hoisted outside the loop). Currently `pl-foreach` handles this implicitly; the annotation makes the intent explicit and guards against accidental hoisting in future refactors.

#### D5. `sort_special` (boolean)

**Semantics:** This use of `$a` or `$b` is inside a sort comparator block (or a named sub used as a comparator in the same file).

**Set on:** `$a` / `$b` leaf nodes inside sort blocks.

**Used in:** Forces `defvar` emission even when the variable is never assigned with `my` — same as the current hard-coded unconditional `defvar` for `$a`/`$b`, but scope-limited.

#### Implementation approach

The variable annotation pass requires a **scope stack**. It runs in two sub-passes:

1. **Declaration scan (pre-order):** Walk the AST; when a `my`/`our`/`local` declaration is seen, push `($name → decl_node_id)` onto the current scope frame. Push a new frame on entering a `sub {}` or `do {}` block; pop on exit.

2. **Use resolution (same walk):** For every variable-leaf node, look up `$name` in the scope stack (innermost frame first). Annotate with `var_kind` and `var_decl_node`. If the resolution crosses a `sub` boundary, set `closure_captured` on the decl node.

**Walk is done by** a new `Pl::VarAnnotator` module (or a sub-phase of `Pl::ASTAnnotator`), using the `OpcodeTree::set_metadata` / `get_metadata` API already available.

---

### E. `unboxable` (Phase 0b — scalar unboxing analysis)

**Goal:** Identify `my $scalar` variables that are never referenced (`\$var`) and therefore do not need to be stored in a mutable heap box. Codegen can emit a plain CL `let`-binding that holds the raw value directly instead of `(make-p-box value)`.

**Why boxing exists:** Every Perl scalar is currently stored as a `p-box` (a one-slot struct). This allows:
- `\$x` — reference-taking returns the box itself
- `pos($x)` — stores regex position in the box's metadata slot
- Tied scalars — FETCH/STORE dispatch through the box
- `local`-ization — dynamic binding saves/restores the box

For the vast majority of `my` scalars in real code, none of these apply. The box is pure overhead: `make-p-box` at declaration, `unbox` at every read, `(setf (p-box-value b) v)` at every write.

**Set on:** The declaration node of a `my $scalar` (not arrays or hashes — those are already vectors/hash-tables and don't use the box model in the same way).

Set `unboxable = true` initially for every `my $scalar` declaration. Then set `unboxable = false` if any of the following are found for that variable (via the use-site list built in Phase 0):

| Disqualifying use | Reason |
|---|---|
| `\$var` (reference-taking) | Returns the box object; box must exist |
| `pos($var)` or `pos($var) = N` | Regex position stored as box metadata |
| `tied($var)` or `tie $var, ...` | FETCH/STORE requires box dispatch |
| `local $var` in an inner scope | Dynamic restore needs the box as the binding unit |
| The variable is `closure_captured` AND it is written inside the closure | Closing over a mutable box is how captures share state |

Note: a closure-captured variable that is **read-only inside the closure** (only assigned in the outer scope before the closure is created) is still unboxable — the closure can just close over the raw CL value.

**Used in codegen:**

- Declaration: `(let ($x INIT) ...)` instead of `(let ($x (make-p-box INIT)) ...)`
- Read: `$x` directly instead of `(unbox $x)`
- Write: `(setf $x RHS)` instead of `(p-setf $x RHS)` (which calls `(setf (p-box-value $x) ...)`)
- Subscript lvalue: unaffected — `p-aref-box`/`p-gethash-box` return their own box for the slot, independent of the scalar variable's box

**Walk:** Runs as a second sub-pass within Phase 0 (after variable resolution), iterating over each `my $scalar` declaration's collected use-site list. O(N) in total variable uses.

**Scope:** Initially applies only to `my $scalar` (sigil `$`). Arrays (`my @arr`) and hashes (`my %hash`) are already stored as CL vectors / hash-tables; extending unboxing to them is a separate future concern.

---

### F. `numeric_context` / `string_context` (future)

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

# Main entry point. Run after VarAnnotator and annotate_contexts().
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
| `Pl/VarAnnotator.pm` | **New** | Phase 0: scope stack, variable resolution, closure capture detection |
| `Pl/ASTAnnotator.pm` | **New** | Phase 1–2: `returns_list`, `needs_wantarray`, `lvalue` walk logic |
| `Pl/ExprToCL.pm` | Modify | Use annotations; remove `lvalue_context` threading; remove `_child_is_list_expr` |
| `Pl/Parser.pm` | Modify | Remove `_vars_referenced_in_closures` and `__lex__N` renaming; remove `_insert_variable_forward_declarations` pre-scan; drive these from VarAnnotator output instead |
| `Pl/OpcodeTree.pm` | No change | `set_metadata`/`get_metadata` already sufficient |
| `Pl/PExpr.pm` | No change | `annotate_contexts` already sets `context` metadata |
| `Pl/t/var-annotator-01.t` | **New** | Unit tests: verify var_kind, closure_captured, loop_var, unboxable values |
| `Pl/t/ast-annotator-01.t` | **New** | Unit tests: verify returns_list, lvalue annotation values |
| `Pl/t/ast-annotator-codegen-01.t` | **New** | Integration tests: verify codegen output changed correctly |

---

## Estimated Size

- `Pl/ASTAnnotator.pm`: ~130 lines
- Changes to `Pl/ExprToCL.pm`: ~40 lines changed, ~60 lines removed (lvalue threading + `_child_is_list_expr`)
- New test files: ~80 lines each

---

## Implementation Order

0. **Phase 0** (prerequisite): Variable annotations (D1–D5) — scope stack, declaration scan, closure capture detection. Replaces `_vars_referenced_in_closures`, the `__lex__N` renaming at parse time, and the `_insert_variable_forward_declarations` pre-scan. No codegen changes in this phase; just build the annotation data.

0b. **Phase 0b** (unboxing analysis): `unboxable` annotation (E) — second sub-pass over Phase 0's use-site lists. Marks `my $scalar` declarations that need no heap box. Codegen changes: skip `make-p-box` at declaration, skip `unbox` at reads, emit raw `setf` at writes.

1. **Phase 1** (highest ROI): `returns_list` + `needs_wantarray` annotations (A, B) → fix `gen_tree_val` string matching and hardcoded function list.

2. **Phase 2** (medium ROI): `lvalue` annotation (C) → remove `lvalue_context` mutable-flag threading from codegen.

3. **Phase 3** (deferred): `numeric_context`/`string_context` (F) — coercion optimization, not needed for correctness.

---

## Relationship to `docs/two-phase-compiler.md`

`two-phase-compiler.md` (written session 158) addresses the **block/statement level**:
the concrete scoping fix for the let-hoisting bug (`docs/let-scoping-problem.md`).
It introduces `BlockAnalyzer`, `_emit_scoped_block`, `_stmt_pre_hook`, and `stmt_idx`
tracking — the emission machinery for inline-let at each `my` declaration point.

This plan (`ast-annotation-plan.md`) is the authority for the **expression level**: the
`VarAnnotator` scope stack, `closure_captured`/`unboxable` on OpcodeTree nodes,
`returns_list`, `needs_wantarray`, and `lvalue`. When both plans are implemented:

- `VarAnnotator` (this plan) should replace `BlockAnalyzer._find_closure_captures`
  and `_vars_referenced_in_closures` — it is more accurate (proper scope stack, detects
  capture across multiple nesting levels).
- `unboxable` (this plan) should replace `type_hint == 'fixnum'` from
  `two-phase-compiler.md` — it explicitly accounts for ref-taking, tie, local, and
  mutable closure captures.
- `_emit_scoped_block` + `_stmt_pre_hook` (two-phase-compiler.md) supply the codegen
  emission mechanism for inline-let that this plan leaves unspecified.
