# `p-list-=` List Context vs `p-readline` Scalar Idiom Conflict

## Problem Statement

There are two competing requirements that cannot both be satisfied by a single approach:

1. **`p-list-=` must force list context on its RHS** so that functions like `p-unpack`, `p-stat`, `p-localtime` return all their values when used in list assignment (`($a,$b,$c) = func()`).

2. **`p-readline` in while-condition list assignments must use scalar mode** so the common Perl idiom `while (($seen ? $dummy : $name) = <FILE>)` works correctly (reads one line per iteration).

## Root Cause

### Perl's Behavior

Perl's compiler inserts a special `defined()` check and scalar context for `readline`/`<>` in while conditions:

```perl
# Perl compiles this:
while (($seen ? $dummy : $name) = <FILE>) { ... }

# As if it were:
while (defined(($seen ? $dummy : $name) = readline(FILE))) { ... }
# where readline() is called in SCALAR context (1 line per call)
```

This is visible via `B::Deparse -p`:
```
while (defined((($seen ? $dummy : $name) = readline(FILE)))) { ... }
```

For the simple `while (($x) = <FILE>)` form, Perl does NOT insert `defined()`, and readline runs in list context (reads all remaining lines; loop terminates when list is empty).

### PCL's Generated Code

PCL generates:
```lisp
(p-while (let ((*wantarray* nil)) 
           (p-list-= (vector (p-if $seen $dummy $name)) 
                     (p-readline 'FILE))))
```

The outer `(let ((*wantarray* nil)) ...)` sets scalar context for the while condition's boolean result.

### The Conflict

**Before the session-204 fix**: `p-list-=` propagated the outer `*wantarray* = nil` to `p-readline` → scalar mode → reads 1 line per call → **defins.t passes** but **pack.t fails** (`($z,$x,$y) = unpack(...)` in void context only gets 1 value).

**After the session-204 fix** (adding `(let ((*wantarray* t)) ,value)` in `p-list-=`): `p-readline` sees `*wantarray* = t` → reads ALL remaining lines in first iteration → **pack.t better** (24 more tests pass, 117→93) but **defins.t fails** (test 84 of 27 fails; `$seen` is 1 instead of expected 2).

## The defins.t Failure

File: `perl-tests/defins.t`, test at line 84:
```perl
seek(FILE, 0, 0);
$seen = 0;
while (($seen ? $dummy : $name) = <FILE>) {
    chomp($name);
    $seen++ if $name eq '0';
}
cmp_ok($seen, '==', 2, 'seen in while() ternary');
```

File `./0` contains two lines: `"0\n"` and `"1\n"`.

**Expected Perl behavior** (scalar readline, 1 line per iteration):
- Iter 1: $seen=0 → LHS=$name → readline→"0\n" → $name="0\n" → defined=truthy. Loop body: chomp→"0", $seen++ → 1.
- Iter 2: $seen=1 → LHS=$dummy → readline→"1\n" → $dummy="1\n" → defined=truthy. Loop body: $name still "0", $seen++ → 2.
- Iter 3: readline→undef → defined(undef)=false → exit.
- Result: `$seen = 2` ✓

**After my fix** (list readline, reads all in first call):
- Iter 1: readline reads ALL lines ("0\n","1\n"). $name="0\n". p-list-= returns box(2)→truthy. chomp, $seen++ → 1.
- Iter 2: readline reads EMPTY (EOF). $dummy=undef. p-list-= returns box(0)→falsy → exit.
- Result: `$seen = 1` ✗

## The Pack.t Fix That Was Made

The fix was needed because `($z,$x,$y) = unpack($tmpl, $data)` in void context gave wrong results:
- `p-list-=` RHS was evaluated with `*wantarray* = :void` 
- `pl-p_unpack` captured `*pcl-caller-wantarray* = :void` at function entry
- `p-wantarray` returned false → `pl-p_unpack` returned only `$result[0]` (first element)
- So `$z` got the value, but `$x` and `$y` stayed undef

The fix `(let ((*wantarray* t)) ,value)` in `p-list-=` forces list context → `p-unpack` returns all values.

## What's Different About `p-readline` vs `p-unpack`

`p-unpack` uses `*wantarray*` to decide **how many values to return from an already-computed list**. Forcing list context is semantically correct: the entire list of unpacked values is computed either way, we just decide how many to return.

`p-readline` uses `*wantarray*` to decide **how many lines to READ FROM THE FILE**. Forcing list context changes the I/O behavior: it reads the entire file at once instead of one line. This is semantically different and breaks the while-loop idiom.

## Solutions to Consider

### Option A: `*p-in-list-assign-rhs*` Flag

Add a new dynamic variable:
```lisp
(defvar *p-in-list-assign-rhs* nil)
```

In `p-list-=`, bind both:
```lisp
(let ((*wantarray* t) (*p-in-list-assign-rhs* t)) ,value)
```

In `p-readline`, use scalar mode when `*p-in-list-assign-rhs*` is set:
```lisp
(if (and (eq *wantarray* t) (not *p-in-list-assign-rhs*))
    (%p-readline-all fh)
    scalar-mode...)
```

**Pro**: Minimal change, fixes the conflict.  
**Con**: `($a, $b) = <FILE>` would read only 1 line (wrong). Acceptable if not tested.  
**Note**: `p-array-= + p-readline` already has the same bug (`@lines = <FILE>` only reads 1 line because `p-array-=` doesn't force list context — pre-existing).

### Option B: Separate codegen for readline vs other RHS

In the codegen (`Pl/ExprToCL.pm`), when generating the RHS of a list assignment:
- If RHS is a readline (`<FH>` operator), wrap with `(let ((*wantarray* nil)) ...)` 
- Otherwise wrap with `(let ((*wantarray* t)) ...)`

This matches Perl's compiler optimization precisely.  
**Pro**: Semantically correct for all cases.  
**Con**: Requires recognizing `<FH>` in the AST at the list-assignment generation point.

### Option C: Change `p-readline` to not use `*wantarray*`

Remove the `*wantarray*` check from `p-readline`. Add a separate `p-readline-list` that reads all lines. The codegen emits `p-readline-list` only for `@array = <FH>` forms.

**Pro**: Clean separation of concerns.  
**Con**: Requires codegen changes + updating `p-array-=` generated patterns.

### Option D: Fix `p-unpack` directly, revert `p-list-=`

Revert the `p-list-=` change. Instead, make `p-unpack`'s scalar behavior check a different condition.

For example: `pl-p_unpack` could check `(or (eq *wantarray* t) (eq *wantarray* :void))` → return all values unless explicitly in scalar context. This makes void-context list assignments work.

**Pro**: Doesn't touch `p-readline`.  
**Con**: `:void` is the "statement" context. Functions in statement context that return lists SHOULD be in list context. But this might have other side effects.

**Actually dangerous**: If `p-unpack` is called in void context in a non-list-assignment (e.g., `unpack($tmpl, $data);` as a standalone statement just for side effects — rare but valid), it would compute all values unnecessarily. Minor waste but not wrong.

## Recommended Fix

**Option A** (add `*p-in-list-assign-rhs*` flag) is the quickest and safest fix. The known limitation (1-line behavior for `($a,$b) = <FILE>`) is pre-existing for `p-array-=` anyway, and no tests currently check the multi-var list-readline pattern.

**Option B** (codegen fix) is the most correct but requires more investigation into the AST at the list-assignment generation point.

## Current State (after session 204)

The session-204 fix (`p-list-=` forces list context) is **IN PLACE** in `pcl-runtime.lisp` at line 2895. This gives:
- pack.t: 117 → ~93 failures (24 fixed, net improvement)
- defins.t: 27 → 26 passing (1 regression)
- Net sweep: 27811→27710 passing (improvement overall, but defins.t regressed)

The defins.t failure is test 84: `'seen in while() ternary'`.

## Files to Touch

- `cl/pcl-runtime.lisp`: `p-list-=` macro (line 2895), `p-readline` macro (line 5937)
- If Option B: `Pl/ExprToCL.pm`: list assignment RHS code generation
