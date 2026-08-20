# Perl Test Suite Integration - Session Notes

## Current Status: PROGRESS

Running Perl's own test suite (`t/op/*.t`) with PCL to find and fix bugs.

## What's Fixed This Session

### 1. Boolean Values (T/NIL handling)
**Problem:** Perl comparison operators return values that stringify to "1" (true) or "" (false). CL's T was stringifying to "T" and numifying to 0.

**Fixed in `cl/pcl-runtime.lisp`:**
- `stringify-value`: Added `((eq v t) "1")`
- `to-number`: Added `((eq val t) 1)`
- `box-nv`: Added `((eq v t) 1)` for boxed values

### 2. 4-arg `substr` in-place modification
**Problem:** `substr($str, 0, 1, "X")` didn't modify the variable in place.

**Fixed:** `pl-substr` now modifies the box when 4 args given.

### 3. Array auto-extension
**Problem:** `$arr[5] = x` on empty array failed (didn't auto-extend).

**Fixed:** `(setf pl-aref)` now extends array with `*pl-undef*` values.

### 4. Magical string increment
**Problem:** `++$x` where `$x = "a9"` should become "b0", not numeric 1.

**Fixed:** Added `magical-string-increment` and `perl-increment` functions. Updated `pl-pre++` and `pl-post++` to use them.

### 5. Range operator `..`
**Problem:** `1..5` was generating undefined `pl-..`.

**Fixed:** Added `pl-..` and `pl-...` functions to runtime.

### 6. Array/hash slices with ranges
**Problem:** `@arr[0..5]` failed because slice functions didn't handle list arguments.

**Fixed:** `pl-aslice` and `pl-hslice` now flatten list arguments.

### 7. `join` with lists
**Problem:** `join(",", @slice)` failed when slice returned a list (not vector).

**Fixed:** `pl-join` now handles both vectors and lists.

### 8. `sort` without comparator
**Problem:** `sort @arr` (no block) failed - expected comparator argument.

**Fixed:** `pl-sort` now defaults to string comparison when no comparator given.

## Tests Passing

| Test File | Tests | Status |
|-----------|-------|--------|
| perl-tests/bool.t | 8 | PASS |
| perl-tests/cond.t | 5 | PASS |
| perl-tests/append.t | 6 | PASS |
| perl-tests/chr.t | 21 | PASS |
| perl-tests/auto.t | 35 | PASS |
| perl-tests/array.t | 22 | PASS |
| perl-tests/hash.t | 14 | PASS |
| **Total** | **111** | **PASS** |

Plus all 2121 existing PCL tests still pass.

## Known Issues (need fixing)

### `exists` and `delete` code generation
`exists $h{key}` generates wrong code:
```lisp
(pl-exists (pl-gethash %h "key"))  ; evaluates access first - WRONG
```
Should be:
```lisp
(pl-exists %h "key")  ; pass hash and key separately
```

**Tracked in:** REMAINING.md under "Code Generation Gaps"

## Test Command

```bash
./pl2cl perl-tests/foo.t > /tmp/foo.lisp && \
sbcl --noinform --non-interactive \
  --load cl/pcl-runtime.lisp \
  --load cl/pcl-test.lisp \
  --load /tmp/foo.lisp 2>&1 | \
  grep -v '^;' | grep -v STYLE-WARNING | grep -v 'PCL'
```

## Next Steps

1. Fix `exists`/`delete` code generation in ExprToCL.pm
2. Continue with more tests from Perl's `t/op/` directory
3. Focus on tests that exercise features CPAN modules commonly use
