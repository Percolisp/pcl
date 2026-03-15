# Perl op/ Test Suite — Categorized Failure Analysis

Last updated: 2026-03-14 (session 77)
Sweep total: **8451+ passing** (session 77 added isa.t: +14, split.t: +16) across 100 files (+2 skipped)

---

## Fully Passing (35 files — do not need investigation)

arith.t, arith2.t, assignwarn.t, blocks.t, bool.t, closure.t, cmpchain.t,
cond.t, defined.t, defins.t, die.t, dor.t, exp.t, grep.t, hashassign.t,
if.t, int.t, kvhslice.t, lc.t, loopctl.t, lop.t, my.t, not.t, num.t,
or.t, pow.t, push.t, qq.t, quotemeta.t, recurse.t, sleep.t, sub.t,
translate.t, unshift.t, while.t

---

## Skipped (hang — do not investigate)

- **bop.t**: hangs (large shift counts in SBCL). See `docs/todo-features.md`.
- **heredoc.t**: hangs (edge case in heredoc parsing). See `docs/todo-features.md`.

---

## Zero-Passing Files — Root Causes Known, Not Worth Fixing

| File | Root Cause | Notes |
|------|-----------|-------|
| **args.t** (0/4) | `@_` aliasing — deliberate not-supported | See `docs/not-supported.md` |
| **crypt.t** (0/0) | Self-skip: `plan 0 # Skip crypt unimplemented` | System has no crypt() |
| **die_exit.t** (0/17) | Tests subprocess exit codes — all use `fresh_perl_is` or `system()` | Cannot run in PCL |
| **flip.t** (0/3) | Flip-flop `..` in scalar context not implemented | See `docs/todo-features.md` |
| **hexfp.t** (0/0) | PPI can't parse hex floats (`0x1.8p+1`) | See `docs/not-supported.md` |
| **lfs.t** (0/0) | Self-skip: `plan 0 # Skip no 64-bit file offsets` | Platform skip |
| **print.t** (0/3) | All 3 tests use `fresh_perl_is` (spawn real Perl subprocess) | Cannot run in PCL |
| **hexfp.t** (0/0) | PPI parse error on hex float literals | See `docs/not-supported.md` |

---

## Zero-Passing Files — Root Causes Identified, Fixable

| File | Pass/Fail | Root Cause | Fix Complexity |
|------|-----------|-----------|----------------|
| **caller.t** | 0/? | UNBOUND-VARIABLE crash at startup — likely `$Pkg::var` forward decl issue | Medium |
| **chdir.t** | 0/? | Uses POSIX module (XS) — XSLoader not supported | Blocked (XS) |
| **concat2.t** | 0/3 | operator overloading (`use overload '""'`, `'.'`); test 3 = `local $~` magic var | Hard (overload) |
| **each.t** | 0/? | `XSLOADER::PL-LOAD` undefined — XSLoader crashes at load | Blocked (XS) |
| **grent.t** | 0/? | Undefined function — likely `getgrent`/`getpwent` POSIX API | Medium (add stub) |
| ~~**isa.t**~~ | **14/14 FULLY PASSING** | Fixed session 77 | Done |
| **method.t** | 0/? | SBCL INPUT-ERROR-IN-LOAD — parse error, non-ASCII chars in source likely | Medium |
| **pack.t** | 0/? | Undefined function in `pack`/`unpack` — possibly missing format chars | Medium |
| **sort.t** | 0/? | TYPE-ERROR from Tie::StdArray (Tie works now but SPLICE recursive dispatch?) | Medium |

---

## Partially-Passing Files — Root Causes Identified

### sprintf.t — 2829/2830 (1 failure)
- **Failing test**: `%53.0f` with precision 0 formats as `0.` instead of `0` (trailing dot)
- **Root cause**: `p-sprintf` for `%f` with precision 0 produces `0.` not `0`
- **Fix**: Strip trailing dot in `%f` formatting when precision=0
- **Complexity**: Easy — one-liner in `p-sprintf`'s float formatting

### warn.t — 6/11 (5 failures: tests 3, 6, 9, 10, 11)
- **Failing pattern**: `warn $ref; ok ref($warnings[0]) eq "ARRAY" && $warnings[0] == $wa`
- **Root cause**: Reference equality via `==` fails because `box-set` strips the ref-box wrapper.
  `$warnings[0]` becomes a raw CL-vector, `to-number(CL-vector)` = length (0),
  while `to-number($wa)` = object-address. See `docs/reference-equality.md`.
- **Fix**: 3-part fix: box-set preserve ref-boxes + pl-push-impl + box-nv. HIGH RISK.
- **Complexity**: Hard (box-set is central runtime, regression risk)

### do.t — 25/52 (27 failures)
- **Tests 9-10**: `do { 1 if $zok }` — bare-if implicit return (condition false → return condition value not nil)
- **Tests 12-51**: `return do { }` receiving caller context — all wantarray/context issues (deferred)
- **Tests 32+**: Various wantarray/context semantics
- **Fix**: Tests 9-10 fixable with bare-if implicit return (see `docs/todo-features.md`). Tests 12+ deferred.
- **Complexity**: Medium for tests 9-10 only

### aassign.t — ~91/177 (86 failures)
- **Tests 8, 10-11, 13-14, 16-17**: Lvalue aliasing, hash/array element aliasing
- **Tests 19-20**: Lvalue sub (not-supported, see `docs/not-supported.md`)
- **Tests 23-24**: Nested array elem swap
- **Tests 26**: `my (...) = @_` list assignment edge case
- **Tests 29-41**: NOSTEAL optimization (internal Perl list-assign optimization) — not applicable
- **Root cause**: Mix of unsupported features (lvalue sub, @_ aliasing) and list-assign edge cases
- **Complexity**: Hard overall; individual tests may be medium

### context.t — 5/8 (3 failures)
- **Test 2**: `$h{foo} = foo` — wantarray context in hash-subscript assignment (deferred)
- **Tests 7-8**: `$_ = sub { context(); BEGIN { } }->()` — `BEGIN {}` inside anon sub generates
  `(EVAL-WHEN ...)` as arg to `p-funcall-ref` instead of hoisting it. Crashes.
- **Fix**: Fix `BEGIN {}` inside anonymous sub body to hoist eval-when before the funcall
- **Complexity**: Medium (parser issue in how BEGIN is handled inside sub bodies)

### pos.t — 8/16 (8 failures)
- **Test 4**: `pos()` set inside `//g` loop — likely scope/state issue
- **Tests 9-11**: lvalue pos (pos as lvalue), pos refuses @arrays and %hashes
- **Tests 12-15**: pos on *glob, UTF-8 pos, defelems pos propagation
- **Root cause**: `p-pos` implementation incomplete; lvalue semantics not supported
- **Complexity**: Medium-Hard

### vec.t — 6/23 (17 failures after crash)
- **Root cause**: TYPE-ERROR — vec() implementation issue or string/integer type mismatch
- **Investigation needed**: Run and check specific error
- **Complexity**: Unknown

### ref.t — 3/7 then crashes
- **Tests 1,3,5,7**: `local(*foo) = *bar` — typeglob localization (not-supported)
- **Crash at test 8**: Continues failing after typeglob tests
- **Root cause**: `local(*GLOB)` documented not-supported
- **Complexity**: Blocked

### anonsub.t — 1/5 (4 failures)
- **Root cause**: Unknown — previously was parse/codegen issue
- **Investigation needed**: Run and check specific errors
- **Complexity**: Unknown

### hash.t — 1/5 (4 failures)
- **Root cause**: Likely tie-related (test uses tied hashes)
- **Investigation needed**: Verify

### lex.t — 1/1 (1 failure)
- **Root cause**: Hash interpolation or %ENV boxing issue
- **Investigation needed**: Small file, easy to investigate

### signatures.t — 4/8 (4 failures)
- **Root cause**: Subroutine signature edge cases
- **Investigation needed**: Run and check

### split.t — 115/132 (17 failures → 4 remaining after session 77)
- **Session 77 fixes** (16 tests): `perl-regex-to-ppcre` for `\x{HH}` escapes, scanner-based
  split with modifiers + capture groups, pattern unboxing, split `/^/` → `/^/m`, `local(undef, $a)`
  list assignment with skip markers, `local(...)` RHS in list context.
- **Remaining 4 failures**:
  - Test 32: `split` with subprocess (`fresh_perl_is`) — cannot run in PCL
  - Tests 58-59: `split('abc' =~ /b/, ...)` — wantarray context leaks into first arg, `/b/` returns
    vector instead of 1. Hard (wantarray context in function arg evaluation)
  - Test 73: `split(/$x/, ...)` — regex variable interpolation: `/$x/` compiled as literal `/$x/`
    string, not interpolated. Need to handle variables in compiled-regex context.
- **TODO next session**: Fix test 73 (regex variable interpolation); tests 58-59 are wantarray-related (deferred)

### concat.t — likely 22/24 or similar
- **Root cause**: Likely UTF-8 encoding edge case in `.=` operator
- **Investigation needed**: Check current pass count

### length.t — partial
- **Investigation needed**: Check current status

### list.t — 37/55 (18 failures)
- **Tests 30-38**: `do { if-elsif-else }` returning list (wantarray context)
- **Test 39**: `(1,2,3)` inside `||` — only returns last element
- **Test 8**: Chained list assignment
- **Tests 48-55**: List slice issues
- **Root cause**: Mix of wantarray/context (deferred) and list/slice codegen issues
- **Complexity**: Hard overall

---

## Files Not Yet Characterized (need investigation next session before working on them)

Run `perl run-perl-test.pl perl-tests/FILE.t 2>&1 | head -20` first:

- anonsub.t, hash.t, lex.t, signatures.t, vec.t, concat.t, length.t
- caller.t (unbound var crash — what var?), pack.t (what function?)
- grent.t (what function?), sort.t (after Tie fix — what TYPE-ERROR?)

---

## Investigation History — Session 77 (2026-03-14)

### What Was Fixed
- **split.t**: 99/132 → 115/132 (+16):
  - `perl-regex-to-ppcre`: converts `\x{HH}` → literal Unicode chars (cl-ppcre limitation)
  - Scanner-based split with modifier flags (`:multi-line-mode` etc.) + `:with-registers-p t` for capture groups
  - Unbox boxed `$pat` (qr// in variable) before passing to `p-split`
  - `split /^/` → `split /^/m` (Perl special case)
  - `local(undef, $a, ...)` — undef skip markers in `_find_symbols_and_undefs_in_list`
  - `local(...)` RHS evaluated in list context with `*wantarray* = t`
- **isa.t**: 0/14 → 14/14 (FULLY PASSING):
  - `isa` infix binary operator: `is_token_operator` extended for `PPI::Token::Word`; `handle_subcalls` skips `isa`; bareword RHS converted to string in Pratt parser
  - `p-isa` runtime: `finalize-inheritance` for unfinalized CLOS classes
  - `p-isa` custom method dispatch: checks `PL-ISA` in object's package
  - `p-method-call` UNIVERSAL fallbacks: `isa` → `p-isa`, `can` → `p-can` when not found in MRO

### Remaining for split.t
- Test 32: subprocess (unfixable)
- Tests 58-59: wantarray in `split(EXPR =~ /re/, ...)` context — deferred
- Test 73: `split(/$x/, ...)` — regex variable `/$x/` not interpolated. **TODO next session**:
  In `gen_leaf` for `PPI::Token::Regexp::Match`, when pattern contains `$var`, generate `(p-regex (format nil "~A" $var))` style interpolation instead of literal. Or: in `p-regex`, detect variable references in pattern and eval them.

### Regression test gap
- No transpile-test added for `isa` yet. Tried adding tests but hit `print $fh isa "Dog"` filehandle detection issue — when `$var isa "Foo"` appears directly after `print`, `$var` is misidentified as a filehandle. **TODO next session**: Add isa regression test using variable assignment form: `my $r = $obj isa "Dog"; print $r ? "yes" : "no";`

---

## Investigation History — What Was Checked Session 76 (2026-03-14)

Files I ran `perl run-perl-test.pl` on:
- `sprintf.t`: 2829/2830 (1 fail: `%53.0f` trailing dot)
- `warn.t`: 6/11 (reference equality bug, documented in `docs/reference-equality.md`)
- `do.t`: 25/52 (bare-if + wantarray)
- `aassign.t`: 91/177 (lvalue + aliasing + NOSTEAL)
- `context.t`: 5/8 (wantarray + BEGIN inside anon sub crash)
- `pos.t`: 8/16 (incomplete pos/lvalue implementation)
- `each.t`: XSLoader crash
- `isa.t`: `isa` infix operator crash
- `print.t`: all `fresh_perl_is`
- `die_exit.t`: subprocess exit code tests
- `concat2.t`: overloading + `local $~`
- `ref.t`: typeglob localization
- `lfs.t`, `crypt.t`: self-skip

---

## Fix Priority Queue

### High ROI, Doable Next Session

1. **split.t test 73**: `split(/$x/, ...)` — regex variable interpolation in `p-regex`/codegen
2. **isa regression test**: add to transpile-test-04.t using `my $r = $obj isa "Foo"; print $r ? ...`
3. **anonsub.t, hash.t, lex.t, signatures.t, vec.t**: still uncharacterized — run and check
4. **caller.t**: UNBOUND-VARIABLE at startup — investigate `$Pkg::var` forward decl issue

### Medium ROI, Multiple Sessions

4. **do.t tests 9-10**: bare-if implicit return (condition false → return condition value)
5. **context.t tests 7-8**: BEGIN inside anon sub generates wrong eval-when position
6. **warn.t tests 3,6,9-11**: reference equality (HIGH RISK to box-set)
7. **sort.t**: investigate TYPE-ERROR after Tie fix

### Low ROI or Blocked

- **concat2.t**: needs operator overloading
- **ref.t**: local(*glob) — documented not-supported
- **chdir.t**, **each.t**: XS/DynaLoader dependency
- **die_exit.t**, **print.t**: subprocess tests — cannot run in PCL
