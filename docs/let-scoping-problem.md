# The `let` Scoping Problem: Mid-Function `my` Declarations

## Problem Statement

PCL hoists all `my` variable declarations to the top of a function as a single `let` block.
This is a structural mismatch with Perl, where `my` creates a **new lexical binding at that
exact point** in the code and that binding is destroyed when its enclosing block exits.

### Concrete example from substr.t

```perl
my $a = 'abcdefxyz';   # package global

sub run_tests {
    # ... many tests using $a ...
    
    my $a = 'zxcvbnm';  # NEW lexical $a, shadows pkg global from HERE
    # ... a few more tests using the new $a ...
}
```

PCL generates:

```lisp
(defvar |$a| "abcdefxyz")

(p-sub pl-run_tests ()
  (let (($a nil))          ; hoisted to top — $a is nil before assignment
    (progn
      ;; ... tests that use $a — but $a is NIL, not "abcdefxyz" !!
      (pl-setf $a "zxcvbnm")
      ;; ... more tests using "zxcvbnm" ...
    )))
```

The `let ($a nil)` at the top shadows the package global `$a` for the entire function, so
all the tests before the `my $a = 'zxcvbnm'` assignment see `nil` instead of `"abcdefxyz"`.

In real Perl the binding only starts at the `my` declaration, not at the function entry.

## Why This is Hard

PCL's `_with_declarations` emits a single `let` block wrapping the entire function body.
Making `my` scopes work correctly at their exact declaration point requires a fundamentally
different code structure:

```lisp
(p-sub pl-run_tests ()
  (progn
    ;; ... tests using package |$a| ...
    (let (($a "zxcvbnm"))  ; declared HERE, not at function top
      ;; ... tests using "zxcvbnm" ...
    )))
```

This means `_with_declarations` can no longer emit a flat `let` — it must emit nested `let`
forms at the exact positions where `my` declarations appear in the source.

## Related Fixes (Session 157)

The immediate symptom in `_find_all_declarations` was that `my $a` in a **bare block**
`{ my $a = ...; ... }` was being hoisted to the sub level, shadowing the package global.
Fix: bare blocks now contribute only `state` declarations to the enclosing sub's hoisting
(not `my`/`local`/`our`), since `_process_bare_block` creates its own `let` scope.

This improves the situation for bare blocks but does NOT fix the general case: a `my`
declaration that appears mid-function (not in a nested block) is still hoisted.

## Current Workaround

substr.t has 280/400 passing. Tests 1–10 fail because `run_tests()` uses a package global
`$a` but then declares `my $a` mid-function. The hoisting breaks the early tests.

The test file is authoritative — the workaround is to fix the compiler, not the test.

## Plan for Full Fix

### Option A: Inline `let` at each `my` declaration (preferred)

Change the code generator to emit nested `let` forms at each `my` declaration point rather
than hoisting all declarations to the top.

Key changes needed:
1. **`_with_declarations`** (`Parser.pm`): instead of collecting all `my` decls upfront and
   emitting one big `let`, traverse statements and wrap each `my`-introducing statement in
   a `let` that scopes from that point to the end of the enclosing block.
2. **Representation**: `(let (($x val)) (rest-of-block...))` — each `my` wraps everything
   after it in the block, creating the correct lexical scope.
3. **`state` variables** still need the old hoisting behaviour (outer persistent `let`).
4. **List form** `my ($x, $y) = ...` also needs to declare both vars at that point.

This is the "correct" fix and matches how Perl actually works. It would be a significant
refactor of `_with_declarations` and `_process_sub_statement`.

### Option B: Two-pass: detect problematic shadowing only

A lighter approach: detect when a `my $x` declaration in a sub body would shadow a
package global or a `my $x` from an outer scope that is actually used before the
declaration. In that case, rename the inner variable to `$x__shadow__N`.

Simpler to implement but only fixes the shadowing case, not general scope ordering issues.

### Option C: Alpha-rename conflicting vars

When `_find_all_declarations` finds a `my $a` that would shadow `$a` from an outer scope,
rename the inner `my $a` to `$a__inner__N` throughout its syntactic scope. This avoids
the scoping problem by ensuring the two `$a` references are distinct CL variables.

The tricky part is determining the lexical scope of the inner `$a` in PPI.

## Files Affected

- `Pl/Parser.pm`: `_with_declarations`, `_find_all_declarations`, `_process_sub_statement`
- substr.t: 120+ failing tests from this issue
- Potentially sub.t, lex.t, and others that re-use popular variable names like `$a`, `$b`
  inside functions after using package globals of the same name.

## See Also

- `docs/declaration-ordering.md` — `defvar` vs `defun` ordering at module load time
- `docs/closure-lexical-scoping.md` — related `$x__lex__N` renaming for closure captures
