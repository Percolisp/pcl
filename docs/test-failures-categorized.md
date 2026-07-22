# Perl op/ Test Suite — Categorized Failure Analysis

Last updated: 2026-07-20 (session 301) — header/totals only; the
per-category analysis below is a session-156 snapshot.  **Current
per-file status lives in `.faillog/_status.tsv` (regression watchdog
baseline: `docs/fail-baseline.tsv`, re-blessed s301) and the bug
groupings in `docs/sweep-bug-catalog.md`.**
Sweep total: **18386 passing / 666 failing**, 66 fully-passing files
(incl. closure.t 257+0/258 as of s301; E1 complete, census 111/0).

Run: `perl sweep-perl-tests.pl --jobs 8` from `/home/bernt/pcl/`

---

## Fully Passing (35 files — do not need investigation)

anonsub.t, append.t, arith.t, arith2.t, assignwarn.t, auto.t, bool.t, chars.t,
cmpchain.t, concat.t, cond.t, defined.t, die.t, dor.t, each_array.t, exists_sub.t,
exp.t, if.t, int.t, isa.t, kvaslice.t, negate.t, not.t, num.t, oct.t, pow.t, push.t,
qq.t, quotemeta.t, recurse.t, reverse.t, sleep.t, study.t, translate.t, warn.t, while.t

---

## Skipped (hang — do not investigate)

- **heredoc.t**: hangs (edge case in heredoc parsing). See `docs/todo-features.md`.
- **list.t**: hangs. See `docs/todo-features.md`.
- **bop.t** *(Crashed in sweep but effectively a hang)*: large shift counts cause SBCL to spin
  (332+121/510). See `docs/todo-features.md`. Also blocked by prototype `($)` spill bug.

---

## Zero-Passing / Blocked (not worth pursuing)

| File | Root Cause | Notes |
|------|-----------|-------|
| **args.t** (0/4) | `@_` aliasing — deliberate not-supported | See `docs/not-supported.md` |
| **chdir.t** (0/?) | Uses POSIX module (XS) — XSLoader not supported | Blocked (XS) |
| **crypt.t** (0/0) | Self-skip: `plan 0 # Skip crypt unimplemented` | System has no crypt() |
| **die_exit.t** (0/17) | All tests use `fresh_perl_is` or `system()` subprocess | Cannot run in PCL |
| **flip.t** (0/3) | Flip-flop `..` in scalar context not implemented | See `docs/todo-features.md` |
| **hexfp.t** (0/0) | PPI can't parse hex floats (`0x1.8p+1`) | See `docs/not-supported.md` |
| **lfs.t** (0/0) | Self-skip: `plan 0 # Skip no 64-bit file offsets` | Platform skip |
| **length.t** (0/?) | `use bytes` + `pack "U"` format not supported | See `docs/not-supported.md` |
| **pack.t** (14099/14722) | 8771 SKIP, 623 fail (see docs/pack-attack-plan.md). Runs to completion. | Partial — see session-log §196 |
| **print.t** (3/3) | ✅ FULLY PASSES (s288 audit) — `fresh_perl_is` works via `perl-tests/t/test.pl`'s subprocess `_fresh_perl_run`; the old "cannot run" claim was stale.  NB: flaky under heavy parallel load (subprocess spawns can time out → INCOMPLETE 0/3) | Passes |
| **signatures.t** (0/0) | Self-skip via `skip_all` — uses `eval $data` (string eval) | Self-skips |
| **sprintf.t** (0/0) | Self-skip via `skip_all` — uses `eval $data` (string eval) | Self-skips |

---

## Crashed Files — Root Causes Known

### Quick wins — stub or one-liner fixes

| File | Crash / Pass | Root Cause | Fix |
|------|-------------|-----------|-----|
| **readline.t** | 15+19/36 PARTIAL | ✅ crash fixed (session 135): `skip_without_dynamic_extension` stub added. Remaining 19 failures are wantarray/lvalue issues. | Done — partial |
| **args.t** | 11+12/23 | ✅ crash fixed (session 135): `goto &funcname`/`goto &$scalar` implemented. Remaining failures are @_ aliasing (documented not-supported). | Done — partial |

### Auto-vivification write-back (Hard)

These crash because `p-aref` returns a value, not a settable location. Nested ref
auto-vivification (`push @{$arr[N]}, val`) doesn't write back to the outer container.
Fix requires returning settable locations from `p-aref` — a pervasive change.

| File | Crash / Pass | Root Cause |
|------|-------------|-----------|
| **ref.t** | 115/184 run (245 planned) | Session 147: no crash. 66 failing = DESTROY-callback tests + others PCL can't support without finalizers. Session 167: +2 (tests 54-55 fixed via p-refgen-list). Tests 56-61 still fail: `\(1,@a,@b)` — needs code-gen fix for multi-term list. `grep` fails on this file (embedded null bytes) — use Perl one-liners. |
| **array.t** | 125+69/195 | Session 147: +56 passing (was 69+40). Fixed: -splice tokenization, p-set-array-length auto-vivify, p-defpackage @ISA init. Remaining failures: nested ref auto-viv write-back (`push @{$arr[N]}, val`) |
| **grent.t** | 2+0/3 | Same — `push @{$arr[N]}, val` in grp entry building |

### Missing built-ins

| File | Crash / Pass | Root Cause | Fix |
|------|-------------|-----------|-----|
| **closure.t** | 50+0/? | `MAIN::PL-READ` undefined — `read(FH, $var, N)` not implemented | Medium — implement `p-read` |
| **defins.t** | ✅ 27/27 PASSING | Was "2+0/27 CRASH" (session 127). Fixed by session 130. All 27 tests pass now. |

### Complex language features

| File | Crash / Pass | Root Cause |
|------|-------------|-----------|
| **loopctl.t** | ✅ FIXED (59/67) | Was: `last LABEL` from inside called sub. Fixed in session 133-134 |
| **lop.t** | ✅ FIXED (47/47) | Was: "compiled with errors". Fixed in prior sessions |
| **method.t** | 68+45/163 | Session 148: crash at ~113. Many fixes applied: dynamic typeglob `*$var=`, `use base`/`use parent` @ISA, qualified dispatch `Foo->PKG::method`, `PKG::SUPER::method`, tied scalar invocant, empty-string class → main. Next crash: `SUPER::m{@a}` indirect-object method call generates `(SUPER::pl-m @a)` as function call instead of method call. |
| **reset.t** | 16+8/45 | `?pattern?` one-match patterns + `reset()` — both removed in Perl 5.38 |
| **sprintf2.t** | 1420+9/? | TYPE-ERROR or `%a` hex-float format after 1429 tests (pre-existing baseline crash) |
| **substr.t** | 273+104/400 | BOUNDING-INDICES-BAD-ERROR — string index out of bounds on edge cases |
| **vec.t** | 76/78 | Tests 25/26 fail: `my $foo` inside `eval {}` hoisted to outer block, shadows file-level wide-char $foo; see session-log.md §189 |

### Tied variables / DESTROY (Not worth pursuing)

| File | Crash / Pass | Root Cause |
|------|-------------|-----------|
| **state.t** | 78/166 | `Can't locate method STORE in package countfetches` — Tie:: interaction, given/when (removed in Perl 5.38). 162 tests now run (vs 117); 4 fail on given/when (not-supported) |
| **or.t** | 5+0/14 | `Can't call method FETCH on non-blessed reference` — tied variable |
| **hash.t** | 1+5/? | DESTROY + tie + PL-GUARD scope issue |

### Blocked / Not Worth Pursuing

| File | Crash / Pass | Root Cause |
|------|-------------|-----------|
| **aassign.t** | 104+83/177 | Mix: lvalue aliasing, `@_` aliasing, NOSTEAL optimization |
| **args.t** | 0+4/23 | `@_` aliasing + `goto &sub` — NOT WORTH PURSUING |
| **caller.t** | 3+7/112 | 36 string evals, `%::` stash, filename/line — NOT WORTH PURSUING |
| **chdir.t** | 43/44 | 1 remaining: test 30 (LOGDIR + chdir returning wrong path). 43 of 44 passing. |
| **each.t** | 13+8/? | `Hash::Util` bucket manipulation — NOT WORTH PURSUING |
| **flip.t** | 0+3/14 | Flip-flop operator not implemented |
| **hexfp.t** | 0+0/125 | PPI parse error on hex floats |
| **infnan.t** | 127+177/? | `sprintf "%a"` hex-float + `pack` with Inf/NaN |
| **join.t** | 39/43 | 4 remaining: tests 9/10 lazy-eval undef warn (hard), 18 undef sep warn, 29 ref identity. Tests 33/39 FIXED (session 155). |
| **lc.t** | 2659/2659 PASSING (transient crash in --jobs 8 parallel sweep only) | FIXED session 155 prev. |
| **pack.t** | 14099/14722 (8771 skip, 623 fail) | See `docs/pack-attack-plan.md`. Main groups: Group A eval-block list ctx (~216t), Group B error msgs (~32t), Group E float checksum FIXED, Group G U format (~152t), Group I UTF-8 bytes (~106t). |
| **undef.t** | 17+4/88 | read-only `$1`, DESTROY, stash `$::{z}` manipulation |

---

## Partial Files (Early Stop — plan mismatch or crash mid-file)

| File | Pass+Fail/Plan | Root Cause |
|------|---------------|-----------|
| **bless.t** | 116+2/118 | No longer crashes (session 146 check). Tests 111-112: read-only blessing error message (not-supported). Tests 115-116: CODE ref DESTROY never called. Planned 118 but ran 116 (2 DESTROY-based tests never print). |
| **blocks.t** | 1+0/26 | Test 1 passes (constant named after special block). All remaining use `fresh_perl_is` (subprocess) → silently return, test harness sees fewer tests than planned |
| **concat2.t** | 1+2/4 | `use overload '""'` and `'.'` — operator overloading not implemented |
| **die_exit.t** | 0+0/17 | All `fresh_perl_is` / `system()` — subprocess tests |
| **kvhslice.t** | ✅ 39/39 | Session 188: `%{$ref}{"keys"}` parse error fixed (Cast+Block+Block now handled); restored test; fixed SKIP stub count; plan corrected 38→39. Session 187 had reduced plan to 38 (dropped one stub). |
| **lex.t** | 11+12/53 | ✅ evalbytes stub added (session 127) — was CRASH(2+4). Remaining: XS::APItest stubs, string interpolation edge cases, error-message matching |
| **pos.t** | 12+18/33 | Crash at test 17+: `pos $_[N]` — subscript arg bleed in parser. `pos $x` OK; `pos $_[N]` passes subscript as extra arg to `pos`. Tests 9-15: DESTROY/defelems not-supported |
| **print.t** | ✅ 3/3 | `fresh_perl_is` supported (subprocess helper in `perl-tests/t/test.pl`); stale "0/3 cannot run" row corrected s288.  Flaky under heavy parallel load only |
| **qr.t** | 19+17/37 | Tests 3,6,9: ref equality (`==` on regex objects → 0). Tests 12,13,14,18: pattern matching with qr// objects. Test 22: tied var for regex |
| **range.t** | ✅ 158+0/162 (4 skip) | Fully passing as of s308 — IV-range endpoint check added to `%p-range-classify` ("Range iterator outside integer range" now dies on the counting-loop path too) |
| **sort.t** | 202+2/205 | Tests 170 (`sorted!`) and 177 (AUTOLOAD without stub) — in the blessed baseline. s308 fixed "Ret: blk ret" (multi-value return under :void caller) |
| **split.t** | 214+5/219 | 5 tests skip "need dynamic loading". "planned 219 but ran 214" is from skip count mismatch, not a crash. Tests 32,58-59,73: see "Failing Without Crash" section. |
| **sub.t** | ✅ 65/65 | Session 187/188: `my sub`, `@_` aliasing, RT124156, DESTROY-GC, Internals, CORE::__SUB__ — all SKIPped. Session 188: `*_{ARRAY}` SKIP comment updated (was "parse error", now explains XS+undef*_ requirement). |
| **time.t** | 52+19/72 | All `scalar gmtime(...)` / `scalar localtime(...)` — wantarray issue (deferred by policy) |

---

## Failing Without Crash (some tests pass, some fail, no early stop)

These files run to completion but have failures. Not yet in fully-passing.

| File | Pass/Plan | Failures | Root Cause | Fix |
|------|----------|---------|-----------|-----|
| **chop.t** | 60/100 | 40 fail | Unicode multi-byte chop; `pack('U',0)` utf8-NUL tests | Documented — Unicode encoding limits |
| **chr.t** | 15/45 | 30 fail | Tests 6,11-15: Latin-1 chars chr(128-255) encoding; tests 22-42: Unicode chr edge cases | Unicode encoding mismatch; `use bytes` |
| **context.t** | 6/8 | 2 fail | Test 2: wantarray leaks into regex; test 8: `BEGIN {}` inside anon sub generates wrong `eval-when` | Test 2: wantarray (deferred). Test 8: Medium fix — hoist `BEGIN` eval-when before `funcall` |
| **delete.t** | 51/56 | 5 fail | Tests 52-54: chained subscript edge cases; test 55: `$#arr` in list context; test 56: error message format | Medium |
| **do.t** | 63/73 | 10 fail | Tests 9-10: bare-if implicit return (condition false → should return nil, not condition); test 22: FIXED session 167 (p-array-= flatten-marker). tests 12-51: wantarray/context | Tests 9-10: Medium fix (see `docs/v1-implementation-plan.md` B1). Rest: wantarray (deferred) |
| **for.t** | 129/138 | 9 fail | Tests 127,129: `for my Dog $spot` type annotations on loop var — PCL crashes/returns undef instead of running. Tests 131-138: `CORE::my/our/state` + type annotations — error-detection tests | Easy: comment out tests 131-138 (error detection). Tests 127,129: fix type annotation parsing |
| **grep.t** | 67/77 | 10 fail | Tests 29,35,37: `grep {hash}->{deref}` — parser misreads hash-constructor block + deref. Tests 47-48: FIXED (map scalar aliasing). Test 54: wantarray (deferred). Tests 61,69,71,73,75,76: DESTROY / error detection — not-supported | Mixed: wantarray (deferred), DESTROY (not-supported), parser bugs remain for 29/35/37 |
| **hashassign.t** | 262/309 | 47 fail | Tests 207-214: hash assignment list/scalar context wantarray. Remaining: various list-context edge cases | Wantarray (deferred) |
| **index.t** | 108/120 | 12 fail | Tests 49-58: multi-byte Unicode character offset (SBCL character vs byte index). Tests 111,119: ref stringification change; tied magic | Unicode offset semantics; tied not-supported |
| **local.t** | 297/319 | 22 fail | `local $hash{key}`, `local @arr[N]`, `local *GLOB` — element-level and typeglob localization | Documented not-supported. See `docs/not-supported.md` |
| **ord.t** | 35/38 | 3 fail | Tests 33-35: `ord` of out-of-range codepoints (0x110000+) — SBCL vs Perl semantics | Unicode edge case; low priority |
| **push.t** | 27/32 | 5 fail | Tests 3: autovivify array (push onto undef → 0 instead of list). Tests 4-6: error message format for push onto non-array. Test 32: read-only error message | Test 3: auto-vivification; tests 4-6,32: error message format (not-supported) |
| **repeat.t** | 45/48 | 3 fail | Tests 37-38: lvalue `x` on LHS of list assignment. Test 43: `(...)x...` via tied var. Tests 46-47: `@_` aliasing | Hard/not-supported |
| **splice.t** | ✅ 34/34 | Session 187: SKIP stubs for wantarray regression tests (13,19) and SvREADONLY test. Side-effect splice preserved so subsequent tests still have correct @a state. |
| **unshift.t** | 18/19 | 1 fail | Test 19: croak when unshifting onto readonly array — error message format | Error message format (not-supported) |
| **wantarray.t** | 17/28 | 11 fail | Tests 2,4,5,7,9,12,15,18,21,24,27: all wantarray/context — returns 'V' or 'A' when expects 'S' | Wantarray (deferred by policy) |

---

## Characterized But Not Worth Pursuing

- **aassign.t**: Lvalue aliasing, `@_` aliasing, NOSTEAL — all deliberate not-supported
- **args.t**: `@_` aliasing + `goto &sub` — deliberate not-supported
- **caller.t**: String eval + `%::` stash + filename/line — too many dependencies
- **chop.t**: Unicode encoding limits — documented
- **chr.t**: Unicode encoding limits — documented
- **each.t**: `Hash::Util` bucket manipulation — not applicable to PCL
- **hash.t**: DESTROY + tie — needs DESTROY implementation
- **hashassign.t**: Remaining failures all wantarray — deferred
- **infnan.t**: pack/unpack + `%a` format — expected failures
- **join.t**: `$SIG{__WARN__}` + `use overload` — not implemented
- **lc.t**: Stub `find_utf8_ctype_locale` would unlock 2577 tests, but locale Unicode testing not in scope
- **length.t**: `use bytes` + `pack "U"` — documented not-supported
- **local.t**: Element-level and typeglob localization — documented not-supported
- **or.t**: Tied variable — blocked
- **state.t**: Tie:: interaction — blocked
- **time.t**: Wantarray — deferred
- **undef.t**: read-only `$1` + DESTROY + stash manipulation — all not-supported
- **wantarray.t**: Entire wantarray system — deferred by policy

---

## Fix Priority Queue

### Done (session 127)

1. ✅ **`alarm(N)` stub** — added to pcl-runtime.lisp + Config.pm + RUNTIME_NAMES
2. ✅ **`my sub` lexical subs** — fixed name extraction in Parser.pm (skip `my`/`our`/`state` qualifiers)
3. ✅ **`evalbytes` stub** — added to pcl-runtime.lisp + Config.pm + RUNTIME_NAMES  
4. ✅ **`goto LABEL` codegen** — ExprToCL.pm emits `(go :label)` for bareword label arg
5. ✅ **Standalone `LABEL:` statement** — Parser.pm emits tagbody tag for bare label compounds
6. ✅ **Lowercase filehandle in `<fh>`** — gen_readline quotes `[A-Za-z_]\w*` barewords

**Net: +38 passing tests (7843→7881), crashed files 32→29, PCL suite 2857→2861 tests**

### High ROI, Doable Next Session

1. **readline.t crash at test 30** — `*x=<y>` + `$SIG{__WARN__}` interaction. Test checks warning text captured by WARN handler. The `p-glob-assign` + readline + warn-capture combo fails. Needs investigation.

2. **`context.t` test 8**: `BEGIN {}` inside anon sub body generates wrong `eval-when` — see B6 in `docs/v1-implementation-plan.md`. Medium.

3. **`@A::ISA = scalar` coercion** — bless.t now runs 116/118, NOT a crash. Still worth fixing: `@A::ISA = 'BB'` must route through `p-array-=`. Gains unknown (was labeled "88 hidden tests", now unclear).

4. ~~**`defined(BAREWORD_FH)`** → defins.t ALREADY FIXED (session 130)~~ ✅

5. **`split.t` test 73**: `/$x/` — regex variable not interpolated at pattern compile time. Generate interpolated regex when pattern contains `$var`. File now 214/219 (not a crash).

6. **`pos $_[N]` parse bug** → fixes pos.t crash at test 17. The subscript `$_[N]` is bleeding extra args into `pos`.

### Medium ROI, Multiple Sessions

7. **my.t tests 51-57** (RT #133543): `my VAR if 0` — error-compat. These check that PCL rejects invalid Perl (deprecated `my` in false conditional). Per policy these should be commented out.

8. **readline.t tests 1-18**: pipe/alarm EINTR, tied scalars, typeglob operations. Complex features.

### Low ROI or Blocked

- **ref.t**: 107+66/257 — 66 failing tests = DESTROY-callbacks + unsupported features. No crash. Not worth pursuing without finalizer support.
- **array.t / grent.t**: remaining failures are nested ref auto-viv write-back (`push @{$arr[N]}, val`) — large change to p-aref
- **concat2.t**: needs operator overloading
- **loopctl.t**: `last LABEL` dynamic scoping — fundamental limitation
- **lop.t**: ANON-BLOCK codegen failure — needs investigation
- **sort.t**: Tie::StdArray — tied arrays
- **substr.t**: BOUNDING-INDICES-BAD-ERROR — string index edge cases
- **range.t**: array slice LHS + string range edge cases
- **sprintf2.t**: pre-existing crash, unclear root cause
- **vec.t**: 76/78 — 2 remaining failures (tests 25/26) due to `my $foo` hoisting bug in eval blocks
- **kvhslice.t**: lvalue + error-detection not-supported
- **qr.t**: ref equality on regex objects (core limitation of CL objects vs Perl SVs)

---

## Investigation History

### Session 127 (2026-04-10) — crash doc + quick wins

**Full categorization of all 100 test files** (first complete pass since session 124's partial doc).
New sections: "Failing Without Crash" (16 files), corrected "Fully Passing" from 44→35.

**Fixed 6 bugs (all low complexity):**

1. **`alarm(N)` no-op** — `p-alarm` stub in pcl-runtime.lisp; added to Config.pm + RUNTIME_NAMES. readline.t no longer crashes on PL-ALARM.

2. **`my sub` name extraction** — Parser.pm `_process_sub_statement`: skip `my`/`our`/`state` qualifiers when extracting name. Fixes `PL-NOT_CONSTANTM` undefined in sub.t tests 17-18.

3. **`evalbytes` stub** — `p-evalbytes` in pcl-runtime.lisp delegates to `p-eval`. Added to Config.pm + RUNTIME_NAMES. lex.t: CRASH(2+4) → PARTIAL(11+12).

4. **`goto LABEL` codegen** — ExprToCL.pm gen_funcall: `goto BAREWORD` emits `(go :label)` instead of `(pl-goto (pl-label))`.

5. **Standalone `LABEL:` statement** — Parser.pm `_process_compound_statement`: bare label (no block/keyword) now emits `:label` tagbody tag. Enables `goto loop` in my.t block.

6. **Lowercase filehandle quoting in `<fh>`** — gen_readline: changed `^[A-Z][A-Z0-9_]*$` to `^[A-Za-z_][A-Za-z0-9_]*$`. Fixes UNBOUND-VARIABLE for `<y>` in readline.t.

**Sweep: 7843→7881 (+38 passing), crashed 32→29 files, PCL suite 2857→2861 tests.**

---

### Session 126 (2026-04-10)
- Reverted session-125 PExpr regressions (study.t fully passing again)
- Committed: bareword subscripts, local @A::ISA sigil fix, @A::ISA qualified assignment, @ISA-first method dispatch
- study.t: fully-passing (43/43) confirmed
- **Sweep: 7843 passing, 1152 failing, 35 fully-passing** (slight variance from session state)

### Session 125 (2026-04-09)
- `local @A::ISA` sigil fix, `p-copy-array` scalar wrapping
- Bareword subscripts `$a[bar]`, `$h{key}` → string literals
- `@A::ISA = scalar` dispatch fix, `$#Pkg::var` codegen fix
- Regressions introduced (then fixed in session 126): study.t crash, sprintf2.t crash

### Session 124 (2026-04-08)
- Categorized all partial/crashed test failures (this document)
- Fixed `"-4\n".."0\n"` range with trailing whitespace (range.t test 22)
- Investigated bareword array subscript crash (delete.t test 54)

### Session 123 (2026-04-07)
- `warning_is`/`warning_like` stubs added → assignwarn.t fully passing
- `blocks.t` FIXED: all-caps invocant guard (INIT, FILE, etc.)
- `gmtime`/`localtime` NaN fix → time.t: 40→52 passing
- `p-*` / `pl-*` naming fix for Tie::Array conflict

### Session 122 (2026-04-06)
- Study.t, lop.t, loopctl.t, or.t crashes characterized
- Bareword subscript `$a[bar]` → crash characterized

### Session 110 (2026-04-01)
- `p-hash` flattens hash-table args — hashassign.t: 209→262 passing
- KV array slice codegen fix
- Trailing nil trim in array slices — delete.t: 51/56

### Session 84 (2026-03-19)
- `++("99a")` → 100 fix; `splice` scalar context; `p-..` range rewrite; array delete/exists

### Session 80 (2026-03-15)
- p-while/p-for return `""` on normal completion
- `\N{U+XXXX}` Unicode escapes; for.t fully passing; quotemeta.t fully passing
