# PCL Perl Test Suite Improvement Plan

## Context

Current state: 1379 passing across 101 runnable test files (session 35).
Goal: Prioritize fixes by tests-gained / effort, covering both bugs in existing features
and missing runtime features.

Data source: Full sweep + targeted per-test diagnostics run this session.

---

## Priority Tier 0 — Quick wins, no semantic complexity (~5-30 min each)

### 0.1 Fix utf8::native_to_unicode / unicode_to_native naming
**Impact: +257 tests (translate.t 0 → ~257)**

The :utf8 package in `cl/pcl-runtime.lisp` defines `pl-native_to_uni` and
`pl-uni_to_native`, but the transpiler generates `UTF8::PL-NATIVE_TO_UNICODE`
and `UTF8::PL-UNICODE_TO_NATIVE` for `utf8::native_to_unicode()` /
`utf8::unicode_to_native()`.

Fix: add correct aliases to the :utf8 package:
```lisp
(defun pl-native_to_unicode (n) (unbox n))
(defun pl-unicode_to_native (n) (unbox n))
```
Also check: `pl-encode`/`pl-decode` — translate.t may need those too.
File: `cl/pcl-runtime.lisp`

### 0.2 Add pl-quotemeta runtime function
**Impact: +66 tests (quotemeta.t 0 → ~60+)**

quotemeta.t crashes with UNDEFINED-FUNCTION: `pl-quotemeta` doesn't exist.
The runtime has no `pl-quotemeta`. The transpiler maps `quotemeta($x)` to
`(pl-quotemeta $x)`.

Perl's quotemeta escapes all non-alphanumeric ASCII characters with `\`.
Characters with code > 127 are NOT escaped (Unicode letters, etc.).

```lisp
(defun pl-quotemeta (str)
  (let ((s (to-string str)) (result (make-adjustable-string 0)))
    (loop for c across s do
      (if (and (< (char-code c) 128)
               (not (alphanumericp c)))
          (progn (vector-push-extend #\\ result)
                 (vector-push-extend c result))
          (vector-push-extend c result)))
    (box result)))
```
File: `cl/pcl-runtime.lisp`. Also export from :pcl, add to `known_no_of_params` in `Pl/PExpr/Config.pm`.

Also fix: `\Q...\E` in string interpolation (`Pl/PExpr/StringInterpolation.pm`)
should NOT escape chars with code > 127. Currently it escapes all non-word chars.
Impact for lc.t: +5 (tests 33,39,45,51 — and test 7 `fc()` is separate).
**Total: +70 tests**

### 0.3 Add pl-continue and pl-break (given/when)
**Impact: switch.t +2 immediately (tests 1-2), unblocks further given/when work**

Tests 1 and 2 of switch.t fail with `PL-CONTINUE is undefined` and
`PL-BREAK is undefined`. These should throw "Can't continue outside..."
and "Can't break outside..." errors.

```lisp
(defun pl-continue () (error "Can't \"continue\" outside a when block"))
(defun pl-break () (error "Can't \"break\" outside a when block"))
```
When used correctly inside given/when they need to be handled by
the when/given macros via catch/throw. Start with error stubs.
File: `cl/pcl-runtime.lisp`

---

## Priority Tier 1 — Bug fixes, significant impact (~1-4 hours each)

### 1.1 Non-ASCII string literal encoding — ALREADY RESOLVED
**Status: Not needed — transpiler already outputs valid UTF-8**

Investigation (session 36): sort.t, hexfp.t, blocks.t, index.t transpiled output
is all valid UTF-8. The crashes are caused by OTHER bugs (transpiler structural
bugs, not encoding). See tasks #62, #63, #64.

Potential future use: A `pcl-str` macro/function could guarantee ASCII-only CL
output for strings with chars ≥ 128, avoiding any encoding ambiguity:
```lisp
;; pcl-str macro generates string from char codes (always ASCII safe)
(defmacro pcl-str (&rest codes)
  `(coerce (list ,@(mapcar (lambda (c) `(code-char ,c)) codes)) 'string))
;; Usage in generated code for "\xfe\x80Hello":
(pcl-str 254 128 72 101 108 108 111)
```
Implement if we discover encoding-related crashes in future.

File: `Pl/ExprToCL.pm` — string literal emission method.

### 1.2 Fix list assignment evaluation order (AASSIGN_COMMON)
**Impact: +~30 tests — list.t +17, aassign.t +5, array.t +6**

Perl guarantees: `($a,$b) = ($b,$a)` swaps. The entire RHS is evaluated
before any LHS assignment. Also: `@foo = @foo` must copy @foo first.
Also: `(undef,@foo) = @foo` must copy @foo before modifying it.

Current PCL behavior:
- `($a,$b) = ($b,$a)` gives `222-222` instead of `222-111` — $a assigned before $b read
- `@foo = @foo` gives wrong result — array overwritten while reading

Root cause: codegen for list assignment emits sequential assignments without
first capturing all RHS values.

Fix in `Pl/ExprToCL.pm` (list assignment codegen) and/or `cl/pcl-runtime.lisp`:
Generate code that captures all RHS values into a temporary list, then
assigns. Example:
```lisp
;; ($a,$b) = ($b,$a)
(let ((#:rhs (list $b $a)))
  (pl-scalar-= $a (nth 0 #:rhs))
  (pl-scalar-= $b (nth 1 #:rhs)))
```
For array assignment (`@foo = ...`), ensure the RHS is fully evaluated
into a fresh list before @foo is modified.

Relevant code: `Pl/ExprToCL.pm` — look for list/array assignment generation.

### 1.3 Fix split: empty pattern and capture groups
**Impact: split.t +20 tests (18-21 empty-pattern-limit, 32-54 captures, 58-60 /^/)**

Three sub-issues:

**a) Empty pattern `split('', $str, LIMIT)`** (tests 18, 20, 21)
- `split('', '1:2:3', -1)` should give `('1',':','2',':','3','')` — include trailing
- `split('', '1:2:3', 2)` should give `('1', ':2:3')` — limit respected
- Current: limit handling wrong with empty pattern

**b) Preserve separators when pattern has captures** (tests 32-54)
- `split /(-)/,  '1-10-20'` → `('1','-','10','-','20')`
- Current: drops the captured separator groups
- Fix: when pattern has capture groups, include them interleaved in result

**c) `/^/` in split** (test 60)
- `split /^/, "a\nb\n"` → split at start of each line (multiline mode)
- Currently not respecting /^/m semantics

File: `cl/pcl-runtime.lisp` — `pl-split` function.

---

## Priority Tier 2 — Feature additions, medium effort (~2-6 hours each)

### 2.1 List slices: `(LIST)[indices]`
**Impact: array.t +5 tests (27-32)**

`('a','b','c','d','e','f')[0..5]` → `('a','b','c','d','e','f')` (join → 'abcdef')
`('a','b','c','d','e','f')[0,2,4]` → `('a','c','e')`
`(@foo,@bar)[0..5]` → merge and slice

Currently returns only first element. List slices need codegen support.
The `[indices]` postfix on a list expression is a list slice.

Files:
- `Pl/PExpr.pm` or `Pl/ExprToCL.pm` — detect `LIST[indices]` and emit
  `(pl-list-slice (list ...) indices)`
- `cl/pcl-runtime.lisp` — implement `pl-list-slice`

### 2.2 Add fc() (fold-case)
**Impact: lc.t +1 (test 7)**

`fc($str)` — Unicode fold case. For ASCII: same as `lc`. For Unicode: use
SBCL's `char-downcase` which handles Unicode fold-case for many scripts.

```lisp
(defun pl-fc (str)
  (box (string-downcase (to-string str))))
```
File: `cl/pcl-runtime.lisp`. Add to `Pl/PExpr/Config.pm` `known_no_of_params`.

### 2.3 pos() function
**Impact: pos.t 0 → ~11 tests (pos.t currently 0 pass, 11 fail)**

`pos($str)` returns the current position in the string from the last `m//g` match.
Requires tracking per-string position after `m//g` operations.

Current PCL has no `pos()` support. The regex match machinery would need to
track position per string. This is medium complexity.

File: `cl/pcl-runtime.lisp` — add `*pl-pos-table*` hash table mapping string
identity to position, update `pl-regex-match` to set pos, implement `pl-pos`.

### 2.4 State variables
**Impact: state.t 0 → many tests**

`state $x = init` — like `my` but initialized only on first call.

In CL, this maps to a closure over a variable initialized once:
```lisp
;; state $count = 0;
(let ((state-$count (make-pl-box nil)))
  (unless (unbox state-$count)  ; initialized?
    (box-set state-$count 0))
  ...)
```
Or use a defvar in the sub's lexical closure.

Files: `Pl/Parser.pm` — detect `state` declaration, `Pl/ExprToCL.pm`,
`cl/pcl-runtime.lisp` — pl-state-init macro.

---

## Priority Tier 3 — Complex semantic changes (high effort, discuss first)

### 3.1 foreach as proper alias
**Impact: array.t +~40, list.t +5**

Perl's `foreach my $x (@array)` makes `$x` an alias to each element.
Modifying `$x` modifies the array. Currently PCL copies values.

The tests failing: array.t tests 30, 33-36 — loop body sees stale values.
These involve `@foo = ('XXX',@foo,'YYY')` (array self-assignment) and
similar array aliasing tests.

Note: Most foreach failures are actually AASSIGN_COMMON (fix 1.2), not the
alias behavior. The alias issue affects fewer tests but is fundamentally correct.
Defer — implement 1.2 first and re-measure.

### 3.2 given/when matching logic
**Impact: switch.t +~40 tests if fully fixed**

13 tests already pass. Failures are in the `when` matching (tests 4-9, 12, 19+):
- Numeric comparison `when (3)` not matching
- `given($x) when(undef)` not matching undef
- `when(++)` (smartmatch with increment)

The given/when smartmatch semantics are complex (deprecated in Perl 5.36+).
Implement basic cases: when(EXPR) does `$_ ~~ EXPR` smartmatch.
Current pl-when/pl-given/pl-given-when macros need debugging.

### 3.3 eval STRING
**Impact: cmpchain.t (if implemented); parts of die.t, etc.**

`eval "expression"` — compile and run Perl string at runtime.
Requires invoking the PCL transpiler at runtime. Complex but potentially
feasible via calling pl2cl from within the runtime.

Currently completely missing. cmpchain.t is entirely eval-based.

---

## Estimated Impact Summary

| Item | Tests gained | Effort | Type |
|------|-------------|--------|------|
| 0.1 utf8::native_to_unicode | +257 | 15 min | Missing alias |
| 0.2 pl-quotemeta + \Q fix | +70 | 1 hr | Bug + missing |
| 0.3 pl-continue/pl-break | +2 | 10 min | Missing |
| 1.1 Non-ASCII encoding | +100 | 2 hr | Bug |
| 1.2 List assign order | +30 | 2 hr | Bug |
| 1.3 Split fixes | +20 | 2 hr | Bug |
| 2.1 List slices | +5 | 1 hr | Missing |
| 2.2 fc() | +1 | 15 min | Missing |
| 2.3 pos() | +11 | 3 hr | Missing |
| 2.4 state vars | +? | 4 hr | Missing |
| 3.1 foreach alias | +40 | 4+ hr | Complex bug |
| 3.2 given/when | +40 | 6+ hr | Complex |
| 3.3 eval STRING | +? | 8+ hr | Complex |

**Recommended execution order:**
1. 0.1 → 0.2 → 0.3 (trivial, huge immediate gain)
2. 1.1 (encoding fix, unblocks many files)
3. 1.2 (list assignment, fixes multiple tests in multiple files)
4. 1.3 (split correctness)
5. 2.1 + 2.2 + 2.3 (features, then measure)
6. Discuss 3.x before starting

---

## Files to Modify

- `cl/pcl-runtime.lisp` — all runtime function additions/fixes
- `Pl/ExprToCL.pm` — string literal emission (1.1), list assignment codegen (1.2), list slice (2.1)
- `Pl/PExpr/Config.pm` — add `quotemeta`, `fc` to `known_no_of_params`
- `Pl/PExpr/StringInterpolation.pm` — fix \Q not escaping Unicode > 127 (0.2)
- `Pl/t/transpile-test-01.t` or `codegen-01.t` — regression tests for each fix

## Verification

After each fix:
1. `prove -j8 Pl/t/` — ensure PCL suite still passes (2383 tests)
2. `perl run-perl-test.pl perl-tests/AFFECTED.t` — verify targeted improvement
3. `perl sweep-perl-tests.pl` — full sweep to confirm no regressions
