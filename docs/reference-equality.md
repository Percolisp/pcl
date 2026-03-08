# Reference Equality (`==` on refs) and the Boxing Problem

## The Symptom

`warn.t` tests 3, 6, 9, 10, 11 fail. All involve `warn $ref` or `warn` with `$@ = $ref`
where `$ref = []` (an array reference). The failing assertion pattern is:

```perl
my $wa = [];
warn $wa;
ok @warnings==1 && ref($warnings[0]) eq "ARRAY" && $warnings[0] == $wa;
```

The `ref(...)` check passes ("ARRAY" is returned correctly), but the `==` identity
check fails: `$warnings[0] == $wa` gives false.

---

## What the Diagnostic Reveals

Running a direct SBCL check (see `/tmp/check-boxing.lisp`):

```
wa value type:      (VECTOR T 0)   ← CL vector, NOT a pl-box
wa value is box:    NIL
wa to-num:          68766975087    ← object-address of the CL vector
unboxed wa type:    (VECTOR T 0)
unboxed wa is box:  NIL
to-num of unboxed:  0              ← LENGTH of the CL vector (empty array!)
wa address:         68766974515
unboxed address:    68766975087    ← same CL vector as wa's value
```

This tells us everything:

### `$wa`'s internal structure

```
$wa  =  pl-box  {value = CL-vector}
```

`box-set` **strips** the intermediate array-ref-box when assigning `$wa = []`:
- `[]` generates `(make-pl-box (make-array 0 ...))` = `array-ref-box {value=CL-vector}`
- `box-set($wa, array-ref-box)`:
  - `inner = (pl-box-value array-ref-box)` = CL-vector (not a pl-box)
  - takes the `inner` path → `v = CL-vector`
  - stores CL-vector directly in `$wa`

So the intermediate reference box (`array-ref-box`) is **thrown away** by box-set.

### `to-number($wa)`

`box-nv($wa)`: `v = CL-vector`, `(vectorp v)` = T → `object-address(CL-vector)`.
Gives the memory address of the CL-vector (large integer like 68766975087).

### What the handler receives

1. `pl-warn` detects `$wa` is a reference (via `pl-warn-is-reference`, which now works
   after the `(pl-box-p v)` fix added this session).
2. Calls the `$SIG{__WARN__}` handler with `$wa` as the argument.
3. Inside the handler: `$_[0]` = `(pl-aref @_ 0)` = `(unbox $wa)` = **raw CL-vector**.
4. `push @warnings, CL-vector` → `pl-push-impl` wraps it: `(make-pl-box CL-vector)`.
5. `$warnings[0]` = `(pl-aref @warnings 0)` = `(unbox (make-pl-box CL-vector))` = **raw CL-vector**.
6. `to-number(raw CL-vector)` uses the **non-box path**: `(length CL-vector)` = **0**.

### The mismatch

```
$wa            → pl-box{CL-vector} → to-number = object-address(CL-vector) ≈ 6×10¹⁰
$warnings[0]   → raw CL-vector    → to-number = length(CL-vector)          = 0
```

---

## Why This Happens: Two Paths in `to-number`

```lisp
(defun to-number (val)
  (if (pl-box-p val)
      (box-nv val)          ; box path: (vectorp inner) → object-address(inner)
      (cond
        ...
        ((and (vectorp val) (adjustable-array-p val)) (length val))  ; RAW path: length!
```

- `$wa` (a pl-box) → **box path** → `object-address(CL-vector)` — reference address ✓
- Raw CL-vector (after `unbox` + `push` + `pl-aref`) → **raw path** → array length ✗

---

## Root Cause

The chain from `pl-warn` → handler → `@_` → `$_[0]` → `push` → `$warnings[0]`
passes through **two unbox operations** that strip the pl-box wrapper:

1. `pl-aref @_ 0` calls `(unbox elem)` → CL-vector (first strip)
2. `pl-push-impl` calls `(unbox item)` → CL-vector again (second strip, redundant)
3. Storing `(make-pl-box CL-vector)` → `pl-aref` strips it again → raw CL-vector

At the end, `$warnings[0]` is a **raw CL-vector**, not a pl-box, so `to-number` uses
the array-length path instead of the reference-address path.

---

## Possible Solutions

### Option A: Fix `box-nv` to use `object-address(box)` for vector/hash cases

In `box-nv`, change:
```lisp
((and (vectorp v) (not (stringp v))) (object-address v))
((hash-table-p v)                    (object-address v))
```
to:
```lisp
((and (vectorp v) (not (stringp v))) (object-address box))  ; address of the ref box, not the array
((hash-table-p v)                    (object-address box))
```

This makes `to-number(box{CL-vector})` return the address of the **box** (the reference),
not the address of the array. The identity then depends on using the SAME pl-box for
both `$wa` and `$warnings[0]`.

**Problem**: After `push` + `pl-aref`, `$warnings[0]` is a **raw CL-vector** (not any pl-box).
`to-number(raw CL-vector)` still takes the raw path → length = 0.
This fix alone is **not enough**.

### Option B: Fix `pl-push-impl` to preserve reference boxes

When `item = array-ref-box` (a pl-box whose value is a non-scalar CL-vector or hash-table),
don't unbox it — store `item` itself in the new wrapper box. Then `pl-aref` returns the
reference box, which numifies correctly.

**Problem**: `$wa.value = CL-vector` (box-set strips the array-ref-box). So by the time
push is called with a pl-aref result, item = CL-vector (raw, not a box). There is no
pl-box to preserve at that point.

The push fix only works if `pl-aref` returns a pl-box. `pl-aref` returns `(unbox elem)`,
which for `elem = pl-box{CL-vector}` gives CL-vector (raw). So push sees a raw CL-vector.

### Option C: Fix `box-set` to preserve array-ref-box, plus fix `box-nv`

Change `box-set` so that when `value` is a pl-box containing a **non-scalar** (vector,
hash-table, function), it preserves the full reference box instead of stripping to the inner:

```lisp
;; Current:
(if (pl-box-p inner) value inner)
;; Proposed:
(if (or (pl-box-p inner)
        (and (vectorp inner) (not (stringp inner)))
        (hash-table-p inner)
        (functionp inner))
    value   ; preserve the reference box
    inner)  ; strip to primitive
```

After this, `$wa.value = array-ref-box` (not CL-vector directly).
- `to-number($wa)` = `box-nv($wa)`: `v = array-ref-box` (pl-box-p) → `object-address(array-ref-box)` ✓
- `pl-aref @_ 0` = `(unbox $wa)` = `array-ref-box` (a pl-box now!)
- `pl-push-impl item = array-ref-box`:
  - `val = (unbox array-ref-box)` = CL-vector → stores `(make-pl-box CL-vector)` (still loses box!)

Still need fix #2 (box-nv): when v = CL-vector, return `object-address(box)`:
- `to-number(array-ref-box)`: v = CL-vector → `object-address(array-ref-box)` (using `box`)
- `to-number($wa)`: v = array-ref-box → `object-address(array-ref-box)` (using `v`, which IS the box)
- **EQUAL** ✓

But pl-push-impl STILL stores CL-vector in a new box, so `$warnings[0]` = CL-vector (raw)
after pl-aref. Still raw path → length.

Still need a fix to **pl-push-impl** to not strip the reference box.

### Option D: Detect "reference" items in `pl-push-impl` and preserve them

After Option C fixes box-set, `pl-aref @_ 0` returns `array-ref-box` (a pl-box).
Then in pl-push-impl:

```lisp
(let ((stored
       (cond
         ;; Variable box with inner pl-box → take the inner ref
         ((and (pl-box-p item) (pl-box-p (pl-box-value item)))
          (pl-box-value item))
         ;; Reference box (pl-box with non-scalar value) → store as-is
         ((and (pl-box-p item)
               (let ((v (pl-box-value item)))
                 (or (and (vectorp v) (not (stringp v)))
                     (hash-table-p v)
                     (functionp v))))
          item)
         ;; Variable box with primitive → unbox
         ((pl-box-p item) (pl-box-value item))
         ;; Raw primitive
         (t item))))
  (vector-push-extend (make-pl-box stored) arr))
```

After this, when item = array-ref-box (from pl-aref), stored = array-ref-box.
`pl-aref @warnings 0` = `(unbox (make-pl-box array-ref-box))` = `array-ref-box`.
With fix #2: `to-number(array-ref-box)` = `object-address(array-ref-box)` ✓

### Option E: Fix the pl-warn call — pass a stable wrapper

A targeted, minimal fix: instead of calling the handler with `$wa` directly, pl-warn
could pass the value via a fresh wrapper box that preserves identity through the
flatten/aref chain. This is tricky because the unboxing happens deep inside the call chain.

---

## Recommended Approach

Option C + D + fix #2 (box-nv) together form a coherent fix:
1. **box-set**: preserve reference boxes (non-scalar inner values)
2. **pl-push-impl**: when item is a reference box (pl-box with non-scalar value), store as-is
3. **box-nv**: for the `(vectorp v)` and `(hash-table-p v)` cases, return `object-address(box)` not `object-address(v)`

These together make reference identity consistent throughout push/retrieve/compare cycles.

**Risk**: box-set is central to the entire runtime. Changing it could break many
existing tests. The fix must be tested carefully with the full suite.

**Alternative (lower risk)**: Only fix `pl-warn` specifically — don't pass the inner
value via the normal @_ mechanism, instead call the handler in a way that bypasses
the reference-stripping unbox. This is narrower but only fixes warn.t, not the general case.

---

## Files to Touch

- `cl/pcl-runtime.lisp`:
  - `box-set` (line ~415) — preserve reference boxes
  - `box-nv` (line ~526) — use `object-address(box)` for vector/hash cases
  - `pl-push-impl` (line ~2816) — detect and preserve reference items
  - `pl-warn` (line ~3715) — possibly the narrowest fix point
