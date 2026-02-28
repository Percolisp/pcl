# Perl op/ Test Suite - Categorized Failure Analysis

Generated: 2026-02-08
Baseline: PCL suite 2257 tests, all passing

## Test Run Summary

| Metric | Count |
|--------|-------|
| Total test files | 103 |
| Fully passing | 16 (append, arith, array, bool, cond, defined, defins, delete, dor, if, isa, join, kvaslice, loopctl, sleep, while) |
| Intentional skips | 2 (crypt, lfs) |
| With failures | 52 |
| 0/0 crash/parse | 30 |
| Timeout | 3 (heredoc, method, warn) |
| **Total passing tests** | **~900** |
| **Total failing tests** | **~380** |

## Failure Categories (sorted by total failure count)

### 1. string_escape — ~87 failures across 7 files

Missing string escape sequence processing for `\Q\E`, `\U`, `\L`, `\u`, `\l`, `\F`, `\x{}`, `\o{}`.

| File | Pass/Fail | Specific Issue |
|------|-----------|----------------|
| lc.t | 11/45 | `\Q\E`, `\u`, `\l`, `\U`, `\L`, `\F` case escapes |
| qq.t | 0/30 | `\x{}`, `\o{}` unicode/octal escapes in qq{} |
| oct.t | 72/7 | `\x20` hex escape in test strings |
| ord.t | 31/5 | `\x{HHHH}` high codepoint escapes |
| lex.t | 1/1 | Hash interpolation + %ENV boxing |
| bless.t(partial) | - | `\Q$var\E` in regex interpolation |
| chr.t(partial) | - | chr() for special/high codepoints |

**Root-cause fix:** Implement `\Q`, `\U`, `\L`, `\u`, `\l`, `\F` processing in string interpolation (StringInterpolation.pm or runtime). Add `\x{}`, `\o{}` to ExprToCL string conversion.

### 2. numeric_format — ~56 failures across 5 files

Number-to-string conversion produces wrong format (trailing decimal points, subnormal floats, Inf/NaN).

| File | Pass/Fail | Specific Issue |
|------|-----------|----------------|
| infnan.t | 12/27 | `"1.#INF"` vs `"Inf"`, sprintf for Inf/NaN |
| exp.t | 16/16 | `"0."` instead of `"0"`, `"1."` instead of `"1"` |
| arith2.t | 2/7 | Subnormal float: `"0.15e-305"` vs `"1.525e-306"` |
| num.t | 42/4 | Trailing decimal point + pack dependency |
| negate.t | 22/2 | `"-10.0"` instead of `"-10"` |

**Root-cause fix:** Fix `to-string`/`box-sv` to strip trailing `.0` and trailing decimal points from number formatting. Fix Inf/NaN string representation.

### 3. crash_type_error — ~35+ failures across 11 files

Runtime type mismatches, most commonly `:UNDEF` as bare symbol, vectors-as-hashes, and unboxed values.

| File | Pass/Fail | Specific Issue |
|------|-----------|----------------|
| list.t | 38/17 | `:UNDEF is not of type PCL:PL-BOX` in list assignment |
| split.t | 31/6 | `:UNDEF is not of type PCL:PL-BOX` from split results |
| or.t | 0/0(crash) | `$!` errno string not boxed as PL-BOX |
| grep.t | 3/4 | `vector is not HASH-TABLE` — map result mishandled |
| each_array.t | 0/0(crash) | `each()` on arrays not implemented |
| range.t | 21/7 | `"1e2" is not INTEGER` in range operator |
| time.t | 10/1 | gmtime(-1) crashes on negative epoch |
| repeat.t | 35/1 | List repeat in assignment context |
| aassign.t | 3/1 | BOX-SET on vector instead of PL-BOX |
| chop.t | 27/1 | chomp with numeric $/ |

**Root-cause fix:** Ensure `:UNDEF` is never returned as bare symbol — always box as `*pl-undef*` or `(make-pl-box nil)`. Fix `$!` to return boxed value.

### 4. wantarray_context — 16 failures across 2 files

wantarray returns wrong context in expression positions.

| File | Pass/Fail | Specific Issue |
|------|-----------|----------------|
| wantarray.t | 14/14 | Returns void ('V') instead of scalar ('S') in `||`, `&&`, `//`, `?:` |
| context.t | 6/2 | Wrong context in BEGIN blocks |

**Root-cause fix:** Set scalar context (not void) for wantarray in boolean/conditional expression positions.

### 5. missing_builtin — diverse, 18 files

Tests crash on undefined functions. No single fix resolves most of these.

| Function | Files Affected |
|----------|---------------|
| pack/unpack | concat, length, num, pack |
| tie/untie | chr, hash, negate |
| pos() | pos |
| prototype() | signatures |
| UTF8::native_to_unicode | chars, quotemeta, translate |
| Internals::SvREADONLY | undef, unshift |
| eq_array/eq_hash (test.pl) | hashassign, kvhslice |
| DynaLoader | chdir |
| exists &sub | exists_sub |
| caller() completeness | caller |

### 6. transpile_failure — 12 files

Generated CL has syntax errors (mostly unbalanced parentheses).

| File | Specific Issue |
|------|----------------|
| anonsub.t | Unbalanced parens (anonymous sub codegen) |
| concat2.t | Unbalanced parens |
| die_exit.t | Unbalanced parens |
| recurse.t | Unbalanced parens |
| splice.t | Unbalanced parens |
| blocks.t | `Package PL- does not exist` (empty package name) |
| for.t | PPI parse failure |
| hexfp.t | Sub `p0` not defined |
| index.t | Unmatched close paren |
| sort.t | `Comma not inside backquote` |
| substr.t | PPI parse failure |

**Root-cause fix:** Investigate paren-balancing bug in code generator. 5 files share "end of file" (unbalanced parens) error.

### 7. eval_string — ~18 failures across 4 files

| File | Pass/Fail | Specific Issue |
|------|-----------|----------------|
| die.t | 3/14 | eval/die $@ propagation broken |
| cmpchain.t | 0/0(crash) | Uses eval "string" for chained comparisons |
| state.t | 0/0(crash) | eval() argument count wrong |
| closure.t | 0/4 | Closures don't capture lexicals properly |

### 8. regex_advanced — ~16 failures across 4 files

| File | Pass/Fail | Specific Issue |
|------|-----------|----------------|
| study.t | 35/8 | (?{code}) regex code blocks |
| qr.t | 1/8 | qr// objects not first-class |
| reset.t | 16/5 | reset() regex state |
| bless.t(partial) | - | `\Q$var\E` regex interpolation |

### 9. tie_overload — 4 files

| File | Pass/Fail | Specific Issue |
|------|-----------|----------------|
| chr.t | 1/12 | tie not implemented |
| hash.t | 1/1 | tie not implemented |
| negate.t(partial) | - | tie blocks later tests |
| reverse.t | 10/3 | Tie::Array loading fails |

### 10. typeglob — 3 files

| File | Pass/Fail | Specific Issue |
|------|-----------|----------------|
| auto.t | 38/1 | *FOO unbound |
| ref.t | 0/0(crash) | *BAR typeglob reference |
| local.t | 0/0(crash) | local(*GLOB) invalid package |

### 11. Other categories

| Category | Files | Notes |
|----------|-------|-------|
| test_timeout | 3 | heredoc, method, warn |
| file_io | 2 | flip, readline |
| crash_other | 4 | closure, do, print, sprintf |
| sprintf_format | 1 | sprintf2 |
| local_dynamic | 1 | local |
| crash_unbound_variable | 3 | each, sub, caller |

---

## Implementation Plan

### Phase 1: numeric_format (trailing decimal point) — HIGHEST ROI
<!-- PLAN: Fix to-string/box-sv in pcl-runtime.lisp to strip trailing ".0" and
     trailing "." from float-to-string conversion. Perl's number stringification
     never produces trailing dots. Check: (format nil "~F" 0.0) → "0.0" should
     become "0". Also fix Inf/NaN representation. Expected impact: exp.t +16,
     infnan.t +10-15, num.t +2-3, negate.t +1 = ~30-35 tests -->

### Phase 2: crash_type_error (:UNDEF boxing)
<!-- PLAN: Audit all places in pcl-runtime.lisp that return :UNDEF as a bare symbol
     and ensure they return *pl-undef* (boxed). Key spots: pl-setf list assignment
     padding, split result padding, $! errno. Expected impact: list.t +10-15,
     split.t +3-5, or.t unblock = ~15-25 tests -->

### Phase 3: wantarray_context
<!-- PLAN: In pcl-runtime.lisp, ensure wantarray is set to scalar (not void) when
     called in expression positions like ||, &&, //, ?:. May need parser changes
     to annotate context. Expected impact: wantarray.t +10-14 = ~10-14 tests -->

### Phase 4: string_escape
<!-- PLAN: Add \x{}, \o{} to convert_perl_string in ExprToCL.pm. Add \Q, \U, \L,
     \u, \l, \F to string interpolation processing. Expected impact: qq.t +15-20,
     lc.t +20-30, oct.t +5, ord.t +3 = ~40-60 tests -->

### Phase 5: transpile_failure (paren balancing)
<!-- PLAN: Investigate common pattern causing unbalanced parens in anonsub, concat2,
     die_exit, recurse, splice. May be an edge case in statement or expression
     codegen. Expected impact: 5 files unblocked = potentially 50+ new tests -->
