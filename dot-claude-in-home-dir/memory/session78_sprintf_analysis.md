---
name: Session 78 sprintf.t regression analysis
description: Root cause of sprintf.t breaking from 2829/2830 to 0/473 in session 78, and why p-array-init fix alone is insufficient
type: project
---

## Session 78 Incomplete Work

Session 78 was working on `for ([1,"a"], [2,"b"])` arrayref iteration. Left uncommitted changes that broke `sprintf.t`.

**Why:** The `%p-flatten-for-list` change in session 78 is the root cause.

## The OLD "passing" behavior was accidentally broken

**OLD `%p-flatten-for-list`** called `%p-collect-list` which unboxed each element and spread vectors. For `for (@tests)` where `@tests` contains 566 arrayref entries:
- Each entry is `p-box{CL-vector-of-5}`
- OLD code unboxed to `CL-vector-of-5` → spread 5 elements
- Result: `for (@tests)` iterated 2830 times (5×566), each `$_` being a single double-boxed scalar
- Each iteration ran `ok(1, "><")` with empty test name — tests "passed" but weren't testing anything real
- Output: `planned 566 but ran 2830` — TAP counts 2830 ok lines, reports 2830 passed

**NEW `%p-flatten-for-list`** keeps `p-box{CL-vector}` items as-is (correct Perl behavior):
- `for (@tests)` correctly iterates 566 times, each `$_` = the 5-element arrayref
- Test loop now actually runs `sprintf($template, @$evalData)` properly
- BUT `p-eval("2**32-1")` returns `2` (read-from-string reads first token), not 4294967295
- So test 5 (`%B` with `2**32-1`) gets `sprintf("%B", 2)` = "10" ≠ expected "11111111111111111111111111111111"
- Result: 473 tests run, all fail (wrong values)

## p-array-init fix (applied this session)

Changed line ~3312 in `pcl-runtime.lisp`:
```lisp
;; OLD (double-boxes p-box variables):
(t (vector-push-extend (make-p-box e) result))
;; NEW (correct: unbox first to avoid double-boxing):
(t (vector-push-extend (make-p-box (unbox e)) result))
```

This fix is **correct** but insufficient alone — the real problem is `%p-flatten-for-list` semantic change.

## Paths to fix

**Option A (restore old behavior)**: Revert `%p-flatten-for-list` to use `%p-collect-list`. Restores 2830/2830 passing (but tests are still "empty" ok calls, not real verification). The for-arrayref fix needs a different approach.

**Option B (correct fix)**: Keep new `%p-flatten-for-list`, fix `p-eval` to actually evaluate arithmetic expressions like `2**32-1`. This makes sprintf.t correctly test 566 things. Much bigger effort.

**How:** `p-eval` currently uses `(read-from-string s)` which reads `2` from `"2**32-1"` (stops at `*`). A proper fix would need a mini-arithmetic evaluator or subprocess call to Perl.

## Why:** `%p-flatten-for-list` change was needed for `for ([1,"a"], [2,"b"])` to not spread arrayrefs when iterating. The fix uses `p-flatten-marker` structs to mark which items should be spread.

## Current state of working dir

- `p-array-init` unbox fix IS applied (good change, keep it)
- `%p-flatten-for-list` new behavior IS present (breaking sprintf.t)
- `gen_progn` wraps @array items with `(p-flatten ...)`
- All other session 78 changes: split.t scanner, isa, float format, etc. (GOOD, keep)

## Recommended next session action

To restore sprintf.t to 2830/2830 with minimal risk:
1. Revert only `%p-flatten-for-list` to the OLD `%p-collect-list` approach
2. Keep `p-array-init` unbox fix
3. Keep all other session 78 changes
4. For the for-arrayref fix: find alternative approach that doesn't touch `%p-flatten-for-list`

Alternative: accept that sprintf.t now runs 566 real tests and fix enough issues to get ≥566 passing (would need `p-eval` improvements).
