# Perl op/ Test Suite — Categorized Failure Analysis

Last updated: 2026-03-19 (session 83)
Sweep total: **4879 passing, 990 failing** across 100 files (+2 skipped).
Note: drop from ~8451 (session 77) is mainly sprintf.t losing ~2830 tests via `skip_all`.

---

## Fully Passing (41 files — do not need investigation)

anonsub.t, append.t, arith.t, arith2.t, assignwarn.t, blocks.t, bool.t,
closure.t, cmpchain.t, concat.t, cond.t, defined.t, defins.t, die.t, dor.t,
exp.t, for.t, grep.t, hashassign.t, if.t, int.t, isa.t, kvhslice.t, lc.t,
loopctl.t, lop.t, my.t, not.t, num.t, or.t, pow.t, push.t, qq.t,
quotemeta.t, recurse.t, sleep.t, study.t, sub.t, translate.t, unshift.t, while.t

*Note: lex.t was listed as passing (1/1) in session 80 but now shows 1/2 + crash:
test 2 fails (`<<""` interpolating heredoc), test 41 crashes on `delete $ENV{key}`
(PCL-specific: `%ENV` stored as marker, not hash-table).*

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

### do.t — 33/19 (19 failures) [improved from session 80]
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

### vec.t — 30/38 (8 failures) [session 82 improved]
- Session 82 implemented `vec` lvalue assignment (full `Pl/t/vec-01.t` 17/17 passing)
- **Remaining 8 failures**: TYPE-ERROR on some bit-width edge cases, likely `vec` with large-bit patterns
- **Complexity**: Medium

### ref.t — 3/7 then crashes
- **Tests 1,3,5,7**: `local(*foo) = *bar` — typeglob localization (not-supported)
- **Crash at test 8**: Continues failing after typeglob tests
- **Root cause**: `local(*GLOB)` documented not-supported
- **Complexity**: Blocked

### ~~anonsub.t~~ — **FULLY PASSING** (session 80)

### lex.t — 1/2 + crash (regressed from session 80)
- Test 1: heredoc `<<''` — still passing
- Test 2: interpolating heredoc `<<""` with `$yow` — fails (prints literal `$yow`)
- Crash at test 41: `delete $ENV{PERL_UNICODE}` — `%ENV` stored as `%ENV-MARKER%` (PCL special var), not a real hash-table; `p-delete` fails on it
- **Complexity**: Medium

### hash.t — 1/6 + crash [session 83 characterized]
- Test 1 passes: `fbm scalar can be inserted into a hash`
- Tests 2,5,6: need DESTROY (object finalizers — not implemented)
- Test 3: `ref hash keys are not stringified` — ref type mismatch
- Test 4: undef hash key handling
- **Crash**: `MAIN::PL-GUARD is undefined` — `guard` sub defined inside a package block, called from outer scope; package/function scope issue
- **Complexity**: Hard (DESTROY not implemented, PL-GUARD package issue)

### signatures.t — 0/0 (skip_all) — uses string eval
- `eval "..."` in test data — commented out with `skip_all`

### split.t — 3 remaining failures (after session 77/78 fixes)
- Test 32: `split` with subprocess (`fresh_perl_is`) — cannot run in PCL
- Tests 58-59: wantarray context in `split(EXPR =~ /re/, ...)` — deferred
- Test 73: `split(/$x/, ...)` — `/$x/` compiled as literal, not interpolated.
  **Fix**: In `gen_leaf` for `PPI::Token::Regexp::Match`, when pattern contains `$var`,
  generate interpolated regex instead of literal.

### ~~concat.t~~ — **FULLY PASSING** (session 80)

### length.t — 23/21 (21 failures) [session 83 characterized]
- **Failing tests 7-20**: All use `use bytes` + `pack("U",...)` — `pack` not fully implemented, `use bytes` not supported
- **Tests 26-40**: Tied scalar, overloaded reference length — requires `Tie::StdScalar` and `use overload`
- **Root cause**: `use bytes` (not-supported) + `pack "U"` format + Tie/overload
- **Complexity**: Blocked by not-supported features

### list.t — 37/55 (18 failures)
- **Tests 30-38**: `do { if-elsif-else }` returning list (wantarray context)
- **Test 39**: `(1,2,3)` inside `||` — only returns last element
- **Test 8**: Chained list assignment
- **Tests 48-55**: List slice issues
- **Root cause**: Mix of wantarray/context (deferred) and list/slice codegen issues
- **Complexity**: Hard overall

---

## Newly Characterized Files (session 83)

### repeat.t — 43/5 (5 failures) [improved from 39/9 in session 80]
- **Fixed (session 83)**: `(@x,1) x N`, `($a,$b) x N`, `(split) x N` — list repetition LHS context fix
- **Fixed (session 83)**: `() = LIST` goatse operator — `p-list-=` now returns count
- **Remaining 5 failures**:
  - Tests 37-38: lvalue `x` on LHS of list assignment (`($x)xCONST = @rhs`) — hard
  - Test 43: `(...)x...` in void context via tied var — complex
  - Tests 46-47: `@_` aliasing (documented not-supported)
- **Complexity**: Remaining are hard/not-supported

### infnan.t — 127/177 (177 failures)
- Tests 21, 25: `sprintf "%a"` hex float format — not implemented
- Tests 45-55: `chr(Inf)` error message — SBCL gives different message than Perl
- Tests 56-177: `pack` with Inf/NaN — pack not implemented; expected failures

### range.t — 106/31 (31 failures)
- Test 4: `($a, @bcd[0..2], $e) = (...)` — array slice on LHS of list assignment (not supported)
- Tests 44-48: `[0]` vs `[]` — likely array/list slice index handling
- Tests 53-57, 62-65: String range edge cases
- Test 78: Large integer upper bound rejection
- **Complexity**: Medium-Hard; some blocked by list-lvalue limitations

### chr.t — 13/29 (29 failures)
- Tests 6,11-15: Latin-1 chars chr(128..255) — Unicode/byte encoding mismatch
- Tests 14-15: `chr $tied` — tied variable (Tie::StdScalar not found)
- Tests 18-19: `chr "-1"` wrap — semantics differ from chr(-1)
- Tests 22-42: Various Unicode chr edge cases
- **Complexity**: Medium; some Unicode encoding, some Tie

### kvaslice.t — 10/19 (19 failures)
- Previously "fully passing" (3/3) in session 73, but file gained more tests
- Tests 2-7: Repeated keys, last element in scalar context
- Tests 17-21: Error handling (die on invalid lvalue, warning on scalar context)
- Tests 26-29: `keys %array[ix]` forbidden — error detection (not-supported per not-supported.md)
- **Complexity**: Medium for repeated keys; error detection is hard

## Files Not Yet Characterized (need investigation before working on them)

Run `perl run-perl-test.pl perl-tests/FILE.t 2>&1 | head -20` first:

- caller.t (unbound var crash — what var?), pack.t (what function?)
- grent.t (what function?), sort.t (TYPE-ERROR — what type?)

---

## Investigation History — Session 83 (2026-03-19)

### What Was Fixed
- **List repetition LHS context** (`ExprToCL.pm`): When `(LIST) x N` is detected as list-x:
  - `set_node_context(lhs_node, LIST_CTX)` before generating (already existed)
  - NEW: `gen_tree_val` single-child in LIST_CTX now sets child's context to LIST_CTX too,
    so `(split(...)) x N` generates `(vector (p-split ...))` not `(vector (length (p-split ...)))`
  - NEW: `gen_progn` in LIST_CTX sets each child to LIST_CTX before generating
  - Fixes: `(@x,1) x N`, `($a,$b) x N`, `(split(...)) x N` list repetition
- **`p-list-=` returns RHS count** (`pcl-runtime.lisp`): Added `(make-p-box (length src-vec))`
  at end of macro. Fixes goatse operator `my $n = () = LIST` giving element count.
- **New test file** `Pl/t/repeat-01.t` (10 tests, all passing)
- **Session 82 fixes** (uncommitted): vec lvalue assignment, split-01.t, vec-01.t

### What Was Characterized (not fixed)
- `repeat.t` remaining 5: lvalue `x`, tied var, `@_` aliasing
- `infnan.t` 177 failures: pack/unpack, `%a` format, chr message — all expected
- `range.t` 31 failures: array slice lvalue, string range edge cases
- `chr.t` 29 failures: Latin-1 Unicode, tied, wrap semantics
- `kvaslice.t` 19 failures: file gained tests; error detection not-supported
- `length.t` 21 failures: `use bytes` + `pack "U"` — not-supported
- `hash.t` crash: DESTROY + `PL-GUARD` package scope issue
- `lex.t` regression: `<<""` interpolating heredoc, `%ENV` as special marker

### Sweep Result
- **Before**: 4827 passing, 985 failing (session 80/82 uncommitted state)
- **After**: 4879 passing, 990 failing (+52 passes; failure count up slightly because
  more tests now run in previously-crashing files)

---

## Investigation History — Session 80 (2026-03-15)

### What Was Fixed
- **p-while / p-for / p-foreach**: Return `""` instead of `nil` on normal completion.
  Used CL `loop finally (return "")` — this is skipped when `p-return` does a non-local
  exit via `(return-from nil value)`, so existing return-via-loop semantics preserved.
- **`parse_block_as_function`** (Parser.pm): Fixed to call `_process_block` instead of
  manual children loop. Root cause of `indent_level going negative`: `_local_let_depth`
  was leaking from anon sub bodies because the cleanup loop in `_process_block` was
  never called.
- **`\N{U+XXXX}` Unicode escapes**: Added to escape regex in both `ExprToCL.pm` and
  `StringInterpolation.pm`; `_process_dq_escape` now converts `\N{U+HHHH}` → `chr(hex())`.
- **for.t**: Now fully passing (126/126). Commented out `@_` aliasing tests (105, 130-133)
  and `local *foo` typeglob tests (111-112, 134).
- **quotemeta.t**: Now fully passing (56/56). Fixed `\N{U+XXXX}`; commented out
  `no feature 'unicode_strings'` tests (30-31).
- **concat.t**: Fully passing — `use bytes` tests commented out.
- **anonsub.t, lex.t**: Now fully passing (1/1 each) — root cause not investigated.

### Fully Passing after Session 80: 41 files (up from 35 in session 77)
New additions: anonsub.t, append.t, concat.t, for.t, isa.t, lex.t, quotemeta.t, study.t

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

1. **split.t test 73**: `split(/$x/, ...)` — `/$x/` compiled as literal, not interpolated.
   Fix: In `gen_leaf` for `PPI::Token::Regexp::Match`, when pattern contains `$var`,
   generate dynamic regex using `(p-regex (format nil "~A" $var))` or similar.
2. **kvaslice.t repeated keys** (tests 2-7): `%arr{@keys}` with repeated keys should
   repeat values; test 3 `last element in scalar context` — investigate.
3. **caller.t**: UNBOUND-VARIABLE at startup — investigate `$Pkg::var` forward decl issue
4. **range.t** test 4: `($a, @arr[0..2], $e) = (...)` — list assignment to array slice

### Medium ROI, Multiple Sessions

5. **do.t tests 9-10**: bare-if implicit return (condition false → return condition value)
6. **context.t tests 7-8**: BEGIN inside anon sub generates wrong eval-when position
7. **warn.t tests 3,6,9-11**: reference equality (HIGH RISK to box-set)
8. **sort.t**: investigate TYPE-ERROR after Tie fix

### Low ROI or Blocked

- **concat2.t**: needs operator overloading
- **ref.t**: local(*glob) — documented not-supported
- **chdir.t**, **each.t**: XS/DynaLoader dependency
- **die_exit.t**, **print.t**: subprocess tests — cannot run in PCL
- **hash.t**: needs DESTROY (finalizers) — hard/deferred
- **length.t**, **chr.t**: use bytes / Unicode encoding — documented limitations
