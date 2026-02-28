# Session State - 2026-02-22 (Session 44)

---

## Session 44: Quick-win bug fixes (COMPLETE)

### Summary
Fixed a set of small, targeted bugs across the Perl test suite.
PCL suite: 2402 tests, all passing ✓

### New Fully-Passing Tests
- **negate.t**: 24/24 ✓ (was 23/24)
- **pow.t**: 268/268 ✓ (was 75/77)

### Improved Tests
- **lc.t**: 76/88 (was 55/56)
- **list.t**: 37/55 (was 36/55)
- **oct.t**: 77/79 (unchanged — 2 failures need real string eval)

---

## Changes Made

### 1. `perl-tests/t/test.pl` — Added `within()` helper
Used by pow.t and potentially others. Checks if a value is within a
range of an expected value.

### 2. `cl/pcl-runtime.lisp` — Non-ASCII string negation fix
`pl--` used CL's `alpha-char-p` which returns true for Unicode alphabetic
chars like Ā. Perl's string negation (`-"foo"` → `"-foo"`) only applies
to ASCII `[A-Za-z_]`. Fixed by adding `(< (char-code ch) 128)` guard.

### 3. `cl/pcl-runtime.lisp` — Wide character detection in oct/hex
Added `%check-wide-chars` helper, called from `pl-oct` and `pl-hex`.
Errors with "Wide character in oct/hex" when string contains code point > 255.
(Tests 78-79 in oct.t still fail because they go through string `eval` which
is a stub and doesn't execute the code.)

### 4. `cl/pcl-runtime.lisp` — `pl-uni_to_native` moved to PCL package
Was defined locally in UTF8 package, so MAIN couldn't see it. Moved to
PCL package (exported in defpackage :export list), and removed the
duplicate from UTF8 (which now inherits it via `(:use :pcl)`). Fixes
lc.t which calls `uni_to_native()` imported from charset_tools.pl.

### 5. `Pl/PExpr.pm` — `-N**exp` precedence fix
In Perl, `**` has higher precedence than unary minus, so `-3**2 = -(3**2) = -9`.
But PPI merges `-3` into a single negative number token in some contexts (like
after `(`). `_fix_ppi_negative_number_bug` already split negative numbers after
expression-ending tokens; extended it to ALSO split when a negative literal is
immediately followed by `**`.

### 6. `cl/pcl-runtime.lisp` — `pl-array-=` scalar case
`@arr = scalar_expr` was silently producing an empty array because the
`add-items` function in `pl-array-=` had no case for plain scalars/numbers.
Added `(t (when src (vector-push-extend (make-pl-box (unbox src)) ,place)))`.

---

## list.t Remaining Failures (18 of 55)

### Tests 30-38: `do { if-elsif-else }` returning list
```perl
($a, $b, $c) = do {
    if ($x == 0) { ('a','b','c'); }
    elsif ($x == 1) { ('d','e','f'); }
    else { ('g','h','i'); }
};
```
Generates PARSE ERROR: "Handle single node of unknown type."
The `do { }` block with embedded if/elsif/else isn't being parsed correctly.

### Test 39: `||` with list on RHS
```perl
@a = ($x == 12345 || (1,2,3));
```
Generated: `(pl-array-= @a (vector (pl-|| (pl-== $x 12345) (progn 1 2 3))))`
The `(1,2,3)` inside `||` becomes `(progn 1 2 3)` which evaluates to `3`.
Should be `(vector 1 2 3)` so the whole list is returned when `||` takes its RHS.
This is a list context propagation issue.

### Test 8: Chained list assignment
```perl
($a, $b) = ($b, $a) = ($a, $b);
```
Complex right-to-left chained assignment.

### Tests 48-55: List slice issues
Need investigation.

---

## Next Steps

1. **list.t tests 30-38**: Fix `do { if-elsif-else }` block parse error
2. **list.t test 39**: Fix `(list) || (list)` — list context propagation through `||`
3. **list.t tests 48-55**: Investigate slice failures
4. **Continue session 43 high-value list**: local.t (41 fails), array.t (49 fails), switch.t (44 fails)
