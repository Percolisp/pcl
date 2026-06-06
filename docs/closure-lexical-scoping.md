# Closure & Lexical Scoping in PCL

## The Problem

PCL uses `defvar` for package-level `my` variables:

```perl
my $i = 1;          # Perl
```
```lisp
(defvar $i (make-pl-box nil))   ; CL
(box-set $i 1)
```

In CL, `defvar` declares a symbol as **special** = **dynamically scoped**. This means
any `let` binding of `$i` anywhere in the program creates a **dynamic** binding —
one that unwinds when the binding form exits. This destroys closure semantics:

```perl
sub make_counter {
    my $i = 0;              # intended: new $i per call, captured by lambda
    return sub { $i++ };
}
my $c1 = make_counter();
my $c2 = make_counter();
$c1->(); $c1->();   # should be 0, 1
$c2->();            # should be 0 (independent from $c1)
```

Before the lambda fix, `sub { $i++ }` compiled to `(defun --anon-block-N-- ...)`.
Each call to `make_counter` **overwrote the same global function**, so $c1 and $c2
called the same function — and even with different functions, `$i` is the
package-level SPECIAL variable, not a per-call lexical.

## What Was Fixed This Session

### Fix 1: Anonymous subs → `lambda` (DONE, WORKING)

`parse_block_as_function` now accepts a `$return_lambda` parameter (5th arg).
When `1`, instead of emitting `(defun NAME ...)` to the main output and returning
the name, it:
- redirects `_emit` to a temp section
- emits `(lambda (params) body)` instead of `(defun NAME ...)`
- collects the temp section to a string and returns it

`gen_func_ref` in ExprToCL.pm checks `$node->{raw_lambda}` first; if set,
returns the lambda string directly instead of `#'funcname`.

`handle_subcalls` in PExpr.pm calls `parse_block_as_function($next, [], 1, 1)`
for anonymous subs and stores the result in `raw_lambda` on the `func_ref` node.

**Result:** `sub { ... }` generates inline `(lambda ...)` instead of defun+funcref.
Each call to the enclosing named sub creates a fresh closure. Verified working:
`make_counter` test produces 0,1,0 correctly.

**Limitation:** Package-level `my $i = 0` is still `defvar`'d (SPECIAL). A `let`
binding of `$i` inside a named sub (like `make_counter`'s body) creates a DYNAMIC
binding that unwinds when the sub returns. The lambda then sees the package-level
`$i` (= 0), not the per-call value.

So simple cases like `make_counter` happen to work (the lambda is returned and
`$i`'s dynamic binding is already unwound, but the box persists on the heap because
CL GC doesn't collect it while the lambda holds a reference). Wait — actually does
it work? Yes, it does: the `make-pl-box` is a heap object; the let-bound `$i` holds
a reference to the box. When `let` unwinds, the box still exists on the heap.
The lambda captures the box's identity (the `$i` variable) during compilation...

Actually no. The problem is more subtle. When `$i` is SPECIAL (defvar'd),
`(let (($i box)) (lambda () $i))` compiles the lambda with a dynamic reference
to `$i`. When the lambda is called later, it reads the CURRENT DYNAMIC VALUE of
`$i`, which may have been rebound by another call to `make_counter`.

Testing shows the simple `make_counter` test works because in practice SBCL
compiles the lambda to capture the dynamic binding cell correctly for this case.
But `sub bar { my $i = shift; sub { $i... } }` fails: `bar(4); bar(5)` —
both closures share `$i`.

## The Remaining Problem: `defvar` Makes `let` Dynamic

The root cause:

```lisp
(defvar $i ...)           ; makes $i SPECIAL globally
...
(defun pl-bar (&rest %_args)
  (let (($i (make-pl-box nil)))   ; DYNAMIC binding (because $i is special)
    (pl-my-= $i (pl-shift @_))    ; $i = 4 during this call
    (lambda () $i)))              ; lambda sees DYNAMIC $i
; After let exits: $i reverts to package-level value
; Lambda called later: sees package-level $i, not the per-call copy
```

## Fix 2: Unique Lex Names for Captured `my` Vars (DONE, Session 63)

### Summary

`_vars_referenced_in_closures` scans any element tree for `sub` keywords and collects
all symbols inside those sub-blocks. `_with_declarations` calls this when `in_subroutine > 0`
and renames captured `my` vars to `$i__lex__N`. The unique names are never `defvar`'d,
so `let` creates LEXICAL bindings. Lambdas capture the correct per-call copy.

`_process_variable_statement` splits parsing of `my $var = EXPR` when `$var` is renamed:
the RHS is parsed with the old rename temporarily removed (so `my $i = $i + 1` sees the
outer `$i`), then emits `(pl-my-= $i__lex__N RHS_CL)` directly.

**KEY BUG**: PPI's `find` returns `0` (not `undef`) when nothing found. Must use `|| []`
not `// []` when deferencing the result.

**Result:** `closure.t` 38→42/50. `make_counter`, `bar(4)->()`, mutable closures all work.
Remaining 8 failures = `for my $n (0..4) { sub { $n } }` (foreach variable capture = out-of-scope).

## Fix 3: map/grep/sort block closures (DONE, Session 236c) — "Case A"

Named-sub bodies get the `$x__lex__N` rename via `_with_declarations`, but
`map`/`grep`/`sort` blocks take a *different* path — `parse_block_to_cl_string`
collects the body into a temp section and flattens it to a string for an inline
lambda — which never invoked the rename. So a block-local `my` captured by a
nested sub was `defvar`'d (global), and `map { my $x=$_; sub {$x} } qw(a b c)`
returned `"ccc"` instead of `"abc"`.

`_begin_block_closure_scope` / `_end_block_closure_scope` (`Pl/Parser.pm`)
reproduce the rename **directly in the string path** (the bucket-based
`_emit_scoped_block` does not compose with string collection — the reason the
session-235 attempt failed). For each block-local `my` captured by a nested anon
sub, mint a fresh never-`defvar`'d `$x__lex__N`, populate the four maps
codegen consults (`state_var_renames`, `_current_scope_new_renames`,
`_current_scope_old_renames`, `_let_bound_vars`), and wrap the body in
`(let (($x__lex__N …)) …)`. The block compiles to `(lambda ($_) …)` called once
per element, so the `let` mints a fresh box per element ⇒ correct per-iteration
capture. Strict no-op when no block-local `my` is closure-captured.

Also fixed `_vars_referenced_in_closures`: it only scanned
`PPI::Token::Symbol` nodes, so a var referenced **only via string
interpolation/regex** inside the closure (`sub { "v=$x" }`) was missed and stayed
shared. It now also scans interpolating quote/heredoc/regex tokens
(`_vars_in_interpolated_text`); this corrects the same bug for *named-sub*
closures too. Over-inclusion is safe (callers intersect with block-local `my`).

**Unblocks:** functional closure-table modules (e.g. Safe::Isa's
`$_isa`/`$_can`).

### Status: per-iteration capture is now complete

With Fix 3, **both** remaining cases work and `perl-tests/closure.t` passes
**50/50** (0 skips):

- `map`/`grep`/`sort { my $x=$_; sub {$x} }` — fixed here (Fix 3).
- `for my $n (…) { sub {$n} }` / `foreach my $v (@a) { sub {$v} }` — the original
  "Fix 2 remaining 8 failures" (foreach loop-variable capture). These were fixed
  by later evolution of the rename machinery and verified working in session
  236c (top-level and in-sub, C-style and list `foreach`, and capture via string
  interpolation). No separate `pl-foreach` change turned out to be needed.

## Plan for Next Session: Unique Names for Subroutine `my` Vars (SUPERSEDED — DONE)

### The Fix

For `my` variables declared **inside subroutines**, use unique CL symbol names
that are **never `defvar`'d**. Since the symbol is not special, `let` creates
a **lexical** binding, and lambdas capture the per-call copy correctly.

```lisp
(defun pl-bar (&rest %_args)
  (let (($i__lex__1 (make-pl-box nil)))   ; LEXICAL binding ($i__lex__1 never defvar'd)
    ...
    (lambda () $i__lex__1)))              ; closes over the LEXICAL $i__lex__1
```

### Implementation

#### 1. `_with_declarations` in Parser.pm

When `in_subroutine > 0`, generate unique names for each `my` var:
- `$i` → `$i__lex__N` (using a package counter `$lex_var_counter`)
- `@arr` → `@arr__lex__N`
- `%h` → `%h__lex__N`

Use the unique name in the `let` binding. Update the rename map
(`environment->state_var_renames`) so ExprToCL emits the unique name for
all references to the original `$i` within the scope.

Track unique names in `_let_bound_vars` too (so `pl-scalar-=` → `pl-my-=`
substitution fires for them, preventing `proclaim 'special` on unique names).

Save the pre-merge rename map as `_current_scope_old_renames`.

#### 2. `_process_variable_statement` in Parser.pm

The tricky case: `my $i = $i` (shadowing). With rename active, both LHS and
RHS get renamed to `$i__lex__N` → self-assignment → wrong init value.

Fix: for `my $var = EXPR` where `$var` is being newly declared in the current
scope:
- Parse `EXPR` **with the outer rename** (temporarily restore `$i → $i__lex__M`
  from the outer scope, or delete the rename entirely if `$i` wasn't renamed
  in the outer scope)
- The LHS assignment target is handled by the `let` binding (unique name
  already used there), so the body's `pl-my-= $i__lex__N EXPR_CL` needs
  `EXPR_CL` to use the OUTER `$i` reference

The challenge: `_parse_expression` processes the full `my $i = EXPR` expression
at once. Both LHS and RHS go through ExprToCL with the same rename map active.
We can't suppress the rename for only the RHS in the current architecture.

**Possible approaches:**

**A. Two-pass init**: Generate the init value CL separately (with outer rename),
inject into the `let` binding as `($i__lex__N (make-pl-box OUTER_EXPR_CL))`.
The body's `my $i = EXPR` statement then does a redundant (no-op) assignment.
Problem: expressions with side effects (e.g., `my $i = shift`) would be
evaluated twice.

**B. Skip-body marker**: After putting the init in the `let` binding, mark the
PPI statement so `_process_variable_statement` emits nothing for it.

**C. Split parsing**: In `_process_variable_statement`, detect `my $VAR = EXPR`,
parse only `EXPR` (the RHS tokens) with the outer rename, then manually emit
`(pl-my-= $renamed_var CL_EXPR)`. This avoids re-parsing the LHS.

**D. Architecture change**: Move to a top-level `let` for package-level `my`
vars instead of `defvar`. This eliminates the whole problem — inner `let`
bindings are always lexical. Requires solving the `eval-when` compatibility
issue for named subs (they need compile-time visibility, which is harder
inside a `let`).

**Recommended approach: C (split parsing)**

In `_process_variable_statement`, for `my $var = EXPR` inside a sub:
1. Detect that `$var` is in `_current_scope_new_renames`
2. Extract the RHS tokens (everything after `=` in `@parts`)
3. Parse only the RHS with the outer rename temporarily active
4. Manually emit: `(pl-my-= UNIQUE_NAME RHS_CL)`
5. Emit the comment line normally

This works because we control exactly which tokens go to `_parse_expression`.

### What the `$lex_var_counter` and rename machinery looks like

Already sketched in the code (reverted this session, needs re-implementing
correctly next session). The counter declaration is already in Parser.pm.

### Edge cases to handle

- `my ($x, $y) = @_` — list declaration: each var in the list needs renaming
- `my $x` (bare, no init) — handled by `let` binding, no body statement
- `foreach my $x (@list)` — loop var: `pl-foreach` macro needs to emit
  the unique name too (separate change, lower priority)
- State vars in anonymous subs: already use the same rename mechanism
  (`state_var_renames`); the two rename systems must coexist

### Test to verify

The `perl-tests/closure.t` file exercises all these cases. Before the rename
fix, tests 8-10 (`bar()` returning a closure over `my $i = shift`) fail.
After the fix, all simple closure tests should pass.

The generator pattern (tests 19-23 in state.t, tests in closure.t for nested
closures where the outer function creates per-call closures) requires BOTH:
1. lambda fix (done)
2. unique-name fix (planned for next session)

## Files to Change Next Session

1. **`Pl/Parser.pm`**:
   - `_with_declarations`: add unique-name logic + rename map updates
   - `_process_variable_statement`: split-parse for `my $var = EXPR` RHS

2. **No changes needed**: ExprToCL.pm (already checks `state_var_renames`),
   PExpr.pm (already uses the rename via ExprToCL), pcl-runtime.lisp

## Already Done (This Session, Committed)

- `&$scalar(args)` / `&{expr}(args)` code ref call syntax (PExpr.pm)
- `map({key=>$_}, LIST)` hash constructor in paren/block form (Parser.pm + PExpr.pm)
- Anonymous subs → `lambda` instead of `defun` (Parser.pm + ExprToCL.pm + PExpr.pm)
