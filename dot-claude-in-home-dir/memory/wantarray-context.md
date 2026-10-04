# Wantarray Three-State Context — Why It's Not Simple

## The Problem
Perl's `wantarray` has three states: undef (void), "" (scalar), 1 (list).
PCL only has two: nil (void+scalar merged) and t (list).
This causes 14 failures in wantarray.t + 2 in context.t.

## Why the Naive Fix Doesn't Work
Wrapping function calls with `(let ((*wantarray* :scalar)) ...)` in ExprToCL.pm
only helps for direct calls where PExpr.pm already annotated the context correctly.

The **real problem** is in `PExpr.pm::annotate_contexts()` — it doesn't propagate
context through `||`, `&&`, `//`, `?:` operators at sub exit positions.

Example:
```perl
sub or_context { $::f || context(shift) }
$_ = or_context('S');  # scalar context
```

The sub `or_context` is called in scalar context. Inside the sub body, the `||`
expression is the return value, so `context(...)` on its RHS should inherit the
sub's calling context. But `annotate_contexts()` doesn't know about this —
it treats `||` operands independently.

## What Actually Needs to Change
1. `PExpr.pm::annotate_contexts()` needs to propagate parent context through
   `||`, `&&`, `//`, `?:` to their arms/operands (at minimum when they're the
   last expression in a sub body)
2. The runtime `*wantarray*` encoding needs three states (nil/`:scalar`/t)
3. `pl-wantarray` needs to return different values for each state
4. All `(if *wantarray* ...)` checks need `(eq *wantarray* t)` to avoid
   treating `:scalar` as list context (5 spots in pcl-runtime.lisp)
5. ExprToCL.pm needs scalar context wrapping at funcall sites

Steps 2-5 are mechanical. Step 1 is the hard part and requires understanding
the context annotation algorithm deeply.

## Attempted Changes (Reverted)
- Changed `pl-wantarray` to return undef/""/ 1 based on three states
- Changed 5 `(if *wantarray* ...)` to `(eq *wantarray* t)` in runtime
- Added scalar wrapping in ExprToCL.pm gen_funcall/gen_method_call/gen_ref_funcall
- All reverted because without the PExpr.pm fix, they don't solve the real problem

## context.t test 8 — wantarray, NOT a BEGIN{} ordering issue
Test: `$_ = sub { context(); BEGIN { } }->()`
Expected: 'scalar' (context() should see scalar context from $_ = assignment)
Got: 'void'
This is purely a wantarray propagation issue — context() can't see that the anon
sub was called in scalar context. The empty BEGIN{} is irrelevant; the generated
CL drops it correctly. Do NOT investigate this test further.
