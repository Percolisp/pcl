# Plan: Proper L-Value Semantics for Arrays and Hashes

## Problem Statement

Perl array and hash elements are SVs (scalar containers). When you access `$arr[0]` or `$hash{key}`, you get the SV itself, not a copy. This enables:

```perl
chop($arr[0]);           # Modifies array element in place
++($hash{key} = 5);      # Assignment returns l-value
my $ref = \$arr[0];      # Reference to the element
$$ref = 100;             # Modifies original
sub modify { $_[0] = 1 } # Subroutine argument aliasing
modify($arr[0]);         # Changes $arr[0]
```

**Current PCL behavior:** Arrays and hashes store raw values (integers, strings), not boxes. Operations like `chop($arr[0])` receive a copy and cannot modify the original.

## Solution: Store Boxes in Arrays and Hashes

Arrays and hashes must store boxes (PCL's SV equivalent). This matches Perl's internal model.

## Implementation Steps

### Phase 1: Array Element Boxing

**File: `cl/pcl-runtime.lisp`**

#### 1.1 Modify `(setf pl-aref)` (~line 1729)

Current:
```lisp
(defun (setf pl-aref) (value arr idx)
  ...
  (setf (aref arr actual-idx) value)
  value))  ; Returns value
```

Change to:
```lisp
(defun (setf pl-aref) (value arr idx)
  ...
  (let ((box (aref arr actual-idx)))
    ;; If no box exists, create one
    (unless (pl-box-p box)
      (setf box (make-pl-box nil))
      (setf (aref arr actual-idx) box))
    ;; Set the box's value
    (box-set box value)
    ;; Return the box (l-value)
    box))
```

#### 1.2 Modify `pl-aref` (~line 1711)

Current returns raw value. Change to unbox:
```lisp
(defun pl-aref (arr idx)
  ...
  (let ((elem (aref a actual-idx)))
    (if (pl-box-p elem)
        (pl-box-value elem)  ; Unbox for value context
        elem)))              ; Backward compat for non-box
```

#### 1.3 Add `pl-aref-box` for l-value contexts

```lisp
(defun pl-aref-box (arr idx)
  "Get the box at array index (for l-value operations).
   Creates box if needed."
  (let* ((a (if (pl-box-p arr) (pl-box-value arr) arr))
         (i (truncate (to-number idx)))
         (len (if (vectorp a) (length a) 0))
         (actual-idx (if (< i 0) (+ len i) i)))
    ;; Auto-extend if needed
    (when (and (vectorp a) (>= actual-idx len))
      (dotimes (n (1+ (- actual-idx len)))
        (vector-push-extend (make-pl-box *pl-undef*) a)))
    ;; Ensure box exists
    (let ((elem (aref a actual-idx)))
      (unless (pl-box-p elem)
        (setf elem (make-pl-box elem))
        (setf (aref a actual-idx) elem))
      elem)))
```

#### 1.4 Array initialization

When creating arrays, initialize with boxes:
- `vector-push-extend` calls should push boxes
- Array literals should contain boxes

### Phase 2: Hash Element Boxing

#### 2.1 Modify `(setf pl-gethash)` (~line 1861)

```lisp
(defun (setf pl-gethash) (value hash key)
  (let* ((h (if (pl-box-p hash) (pl-box-value hash) hash))
         (k (to-string key)))
    ;; Get or create box
    (multiple-value-bind (box found) (gethash k h)
      (unless (and found (pl-box-p box))
        (setf box (make-pl-box nil))
        (setf (gethash k h) box))
      (box-set box value)
      box)))  ; Return box for l-value
```

#### 2.2 Modify `pl-gethash` (~line 1843)

```lisp
(defun pl-gethash (hash key)
  ...
  (multiple-value-bind (val found) (gethash k h)
    (cond
      ((not found) *pl-undef*)
      ((pl-box-p val) (pl-box-value val))  ; Unbox
      (t val))))  ; Backward compat
```

#### 2.3 Add `pl-gethash-box` for l-value contexts

```lisp
(defun pl-gethash-box (hash key)
  "Get the box at hash key (for l-value operations).
   Creates box if needed (autovivification)."
  (let* ((h (if (pl-box-p hash) (pl-box-value hash) hash))
         (k (to-string key)))
    (multiple-value-bind (box found) (gethash k h)
      (unless (and found (pl-box-p box))
        (setf box (make-pl-box *pl-undef*))
        (setf (gethash k h) box))
      box)))
```

### Phase 3: Code Generator Changes

**File: `Pl/ExprToCL.pm`**

#### 3.1 L-value context detection

Add tracking for when an expression is in l-value context:
- Argument to `chop`, `chomp`
- Argument to `++`, `--` (pre and post)
- Target of `=~` with `s///`
- Argument to `\` (reference)

#### 3.2 Generate `pl-aref-box` / `pl-gethash-box`

When array/hash access is in l-value context, generate the `-box` variant:

```perl
# Current: chop($arr[0])
(pl-chop (pl-aref @arr 0))

# New: chop($arr[0])
(pl-chop (pl-aref-box @arr 0))
```

#### 3.3 Update `pl-setf` macro (~line 985)

The `pl-setf` macro handles assignment. Update cases for array/hash to use the box-returning setters.

### Phase 4: Subroutine Argument Aliasing

This is the most complex part. In Perl, `@_` contains aliases to actual arguments.

#### 4.1 Function call code generation

When calling a sub with array/hash element arguments, pass the box:

```perl
# modify($arr[0])
# Current generates:
(pl-foo (pl-aref @arr 0))

# Should generate:
(pl-foo (pl-aref-box @arr 0))
```

**Challenge:** We don't always know if a sub will modify `$_[0]`. Conservative approach: always pass boxes for array/hash element arguments.

#### 4.2 `@_` handling in subs

`@_` should contain the boxes directly, not copies. When accessing `$_[0]`:
- Value context: unbox
- L-value context: use box

### Phase 5: Reference to Array/Hash Elements

```perl
my $ref = \$arr[0];
```

Currently `\` creates a reference. With boxes in arrays:
- `\$arr[0]` should return a reference to the box
- `$$ref` accesses/modifies the box

This should work naturally once arrays store boxes.

### Phase 6: Autovivification Updates

The autovivification code (`pl-autoviv-set`, `pl-autoviv-aref-set`) needs updating to work with boxes.

## Testing Strategy

### Unit Tests to Add

```perl
# Array l-value tests
my @arr;
$arr[0] = "hello";
chop($arr[0]);
is($arr[0], "hell", "chop modifies array element");

++($arr[1] = 5);
is($arr[1], 6, "pre-increment on array assignment");

# Hash l-value tests
my %h;
$h{key} = "world";
chop($h{key});
is($h{key}, "worl", "chop modifies hash element");

# Reference tests
my @arr2 = (1, 2, 3);
my $ref = \$arr2[1];
$$ref = 100;
is($arr2[1], 100, "reference to array element");

# Subroutine aliasing
sub modify { $_[0] = "changed" }
my @arr3 = ("original");
modify($arr3[0]);
is($arr3[0], "changed", "subroutine modifies array element");

# Chained access
my %deep;
++($deep{a}{b}[0] = 10);
is($deep{a}{b}[0], 11, "chained l-value");
```

### Regression Testing

Run full test suite after each phase:
```bash
prove Pl/t/
```

## Potential Issues

1. **Performance:** Boxing all array/hash elements adds overhead. May need optimization later.

2. **Backward compatibility:** Some code may expect raw values. The unboxing in `pl-aref`/`pl-gethash` should handle this.

3. **Nested structures:** `$arr[0][1]{key}` - each level needs proper box handling.

4. **Special variables:** `@ARGV`, `@_`, `%ENV` may need special handling.

5. **List operations:** `push`, `pop`, `shift`, `unshift`, `splice` need to work with boxes.

## Implementation Order

1. **Phase 1.1-1.2:** Array setf returns box, aref unboxes
2. **Phase 2.1-2.2:** Hash setf returns box, gethash unboxes
3. **Run tests** - many should still pass (backward compat)
4. **Phase 1.3, 2.3:** Add `-box` variants
5. **Phase 3:** Code generator changes for l-value contexts
6. **Run tests** - chop/chomp/++/-- on array/hash should work
7. **Phase 4:** Subroutine argument aliasing
8. **Phase 5:** Reference to elements (may work automatically)
9. **Phase 6:** Autovivification updates
10. **Full test suite + new tests**

## Files to Modify

1. `cl/pcl-runtime.lisp` - Core runtime changes
2. `Pl/ExprToCL.pm` - Code generation for l-value contexts
3. `Pl/t/transpile-test-01.t` - Add l-value tests

## Estimated Scope

- Runtime changes: ~200 lines modified/added
- Code generator: ~50-100 lines
- Tests: ~50-100 lines
- Risk: Medium-high (fundamental data model change)

---

## Phase 7: `$#array` as Lvalue (blocks array.t — 195 tests)

### Problem

`$#array` returns the last index of an array. In Perl it is also an lvalue:

```perl
$#ary = 5;      # Resize array to 6 elements (indices 0..5)
--$#ary;        # Shrink array by one
$#ary++;        # Grow array by one (new element is undef)
$x = --$#ary;   # Shrink and capture new last index
```

Currently `pl-array-last-index` returns a raw integer. When used with `--`/`++`/`=`, `pl-pre--` etc. call `box-set` on the integer, which crashes with `TYPE-ERROR: The value 4 is not of type PCL:PL-BOX`.

### Root Cause

The code generator (`ExprToCL.pm` line ~318) emits:
```lisp
;; $#arr
(pl-array-last-index @arr)
;; --$#arr  (wraps the above)
(pl-pre-- (pl-array-last-index @arr))
```

But `pl-pre--` expects a box (mutable container), not a plain integer.

### Solution: Code Generator Approach

Handle `$#array` in lvalue context in `ExprToCL.pm` by detecting `ArrayIndex` nodes inside prefix/postfix/assignment operators and emitting setter forms.

#### 7.1 Runtime: Add `pl-set-array-length`

**File:** `cl/pcl-runtime.lisp`

```lisp
(defun pl-set-array-length (arr new-last-index)
  "Set array length by setting $#array. Perl semantics:
   - Growing: extends with undef-boxed elements
   - Shrinking: truncates (adjusts fill-pointer)"
  (let* ((a (if (pl-box-p arr) (pl-box-value arr) arr))
         (new-len (1+ (truncate (to-number new-last-index))))
         (cur-len (length a)))
    (cond
      ((> new-len cur-len)
       ;; Grow: extend with undef boxes
       (dotimes (i (- new-len cur-len))
         (vector-push-extend (make-pl-box *pl-undef*) a)))
      ((< new-len cur-len)
       ;; Shrink: adjust fill-pointer
       (when (>= new-len 0)
         (setf (fill-pointer a) new-len))))
    new-last-index))
```

Export `pl-set-array-length` from the `:pcl` package.

#### 7.2 Code Generator: Detect `$#array` in lvalue context

**File:** `Pl/ExprToCL.pm`

In `gen_prefix_op` (for `++`/`--`) and `gen_postfix_op`, check if the operand is an `ArrayIndex` node (i.e. the child generates `(pl-array-last-index @arr)`). If so, emit the setter form instead.

**For `--$#arr`:**
```lisp
;; Instead of: (pl-pre-- (pl-array-last-index @arr))
;; Emit:
(pl-set-array-length @arr (1- (pl-array-last-index @arr)))
```

**For `$#arr--`:**
```lisp
;; Instead of: (pl-post-- (pl-array-last-index @arr))
;; Emit:
(let ((_tmp (pl-array-last-index @arr)))
  (pl-set-array-length @arr (1- _tmp))
  _tmp)
```

**For `++$#arr` and `$#arr++`:** Same pattern with `1+`.

**For `$#arr = N`:**
```lisp
;; Instead of: (pl-setf (pl-array-last-index @arr) N)
;; Emit:
(pl-set-array-length @arr N)
```

#### 7.3 Detection Strategy

The `PPI::Token::ArrayIndex` node appears in OpcodeTree as a leaf under the `++`/`--`/`=` operator. The code generator already has the node type info. The check is:

1. In `gen_prefix_op` / `gen_postfix_op`: if child node is `PPI::Token::ArrayIndex`, switch to setter form
2. In `gen_assignment` / `gen_assign_op`: if LHS is `PPI::Token::ArrayIndex`, switch to setter form

The `PPI::Token::ArrayIndex` is identifiable because `generate()` for it produces a string starting with `(pl-array-last-index`.

**Simpler approach:** Add an `lvalue_context` flag (already exists from Phase 3). When `ArrayIndex` is in lvalue context, wrap in a shim that emits the setter. Or: detect in the operator generators directly by inspecting the child node type in OpcodeTree.

#### 7.4 Tests

Add to `Pl/t/codegen-01.t`:
```perl
# $#array lvalue - pre-decrement
check('--$#ary', '(pl-set-array-length @ary (1- (pl-array-last-index @ary)))');

# $#array lvalue - assignment
check('$#ary = 5', '(pl-set-array-length @ary 5)');
```

Add to `Pl/t/transpile-test-01.t`:
```perl
# $#array lvalue runtime tests
test_cl('@ary = (1,2,3,4,5); --$#ary; is(scalar @ary, 4, "shrink")');
test_cl('@ary = (1,2,3); $#ary = 5; is(scalar @ary, 6, "grow")');
test_cl('@ary = (1,2,3); $#ary = 1; is($ary[0], 1, "truncate keeps early elements")');
```

### Files to Modify

1. `cl/pcl-runtime.lisp` — Add `pl-set-array-length`, export it
2. `Pl/ExprToCL.pm` — Detect `ArrayIndex` in lvalue context, emit setter forms
3. `Pl/t/codegen-01.t` — Codegen tests for `$#array` lvalue
4. `Pl/t/transpile-test-01.t` — Runtime tests for `$#array` resize

### Estimated Effort

- Runtime: ~20 lines
- Code generator: ~30-50 lines (detection + emission in 3-4 operator generators)
- Tests: ~10-15 lines
- Risk: Low (isolated change, clear semantics)
