# Session 30: Test Sweep Report (2026-02-15)

## PCL Suite Status
- **47 files, 2356 tests, all passing** (unchanged from session 29)

## Perl Test Suite Sweep Results

**103 test files | 1109 passing | 259 failing | 10 fully passing | 56 partially passing | 37 zero**

### Fully Passing Tests (10)
| Test | Pass/Total |
|------|-----------|
| qq.t | 30/30 **NEW!** |
| arith2.t | 9/9 **NEW!** (was 1/8) |
| dor.t | 13/13 |
| bool.t | 8/8 |
| cond.t | 4/4 |
| defined.t | 4/5 (4 pass, test 5 doesn't run) |
| isa.t | 4/14 (4 pass, 0 fail, rest don't run) |
| if.t | 2/2 |
| sleep.t | 4/4 |
| while.t | 4/4 |

### Major Improvements Since Last Sweep (Session 20)
| Test | Before | After | Delta | Notes |
|------|--------|-------|-------|-------|
| **arith.t** | 16 | 100 | **+84** | Huge jump! Crashes after 100 though |
| **lc.t** | 11 | 51 | **+40** | String case functions |
| **qq.t** | 0 | 30 | **+30** | Fully passing! |
| **infnan.t** | 19 | 37 | **+18** | Inf/NaN handling |
| **exp.t** | 16 | 31 | **+15** | Math functions |
| **sprintf2.t** | 2 | 12 | **+10** | sprintf improvements |
| **arith2.t** | 1 | 9 | **+8** | Fully passing! |
| **auto.t** | 0/crash | 38 | **+38** | Was total crash |
| **qr.t** | 0 | 7 | **+7** | Regex objects |
| **sub.t** | 2 | 7 | **+5** | Sub features |
| **bop.t** | 0/crash | 6 | **+6** | Bitwise ops |
| **local.t** | 0/crash | 4 | **+4** | local() now works partially |
| **array.t** | 20 | 41 | **+21** | Array operations |
| **negate.t** | 22 | 23 | **+1** | |

### REGRESSIONS (require ./test.pl ordering)
| Test | Before | After | Root Cause |
|------|--------|-------|------------|
| **oct.t** | 72/79 | **0/0** | `require './test.pl'` runs before `chdir 't'` |
| **context.t** | 6/8 | **0/0** | Same issue |
| **length.t** | 3/5 | **0/0** | Carp.pm transpilation UTF-8 crash (probably same root cause) |

**Root cause of regressions**: Sessions 28-29 added two-phase declaration reordering.
Phase 2 classifies `eval-when` containing `:load-toplevel` as compile-time and hoists
it before runtime code. Both `use` and `require` generate `eval-when :load-toplevel`,
but top-level `require` (not in BEGIN) should be runtime. Tests like `oct.t` have:
```perl
chdir 't' if -d 't';    # runtime
require './test.pl';      # gets hoisted before chdir → can't find test.pl!
```
Tests like `bool.t` work because they wrap both in `BEGIN { chdir; require; }`.

**Fix**: In `_is_compile_time_form`, don't classify bare `require` (not from `use`)
as compile-time. Or add an exception for `eval-when` wrapping `pl-require-file`.

### Near-Miss Tests (close to fully passing)
| Test | Pass | Fail | Total | Blocking Issue | Fix Difficulty |
|------|------|------|-------|---------------|---------------|
| chop.t | 28 | 0 | 28 | Crashes at exit (boxed test count in `1..N` plan) | Easy — unbox in plan |
| delete.t | 24 | 0 | 56 | Crash on `%REFHASH` (hash-of-hash autoviv) | Medium |
| recurse.t | 23 | 0 | 28 | Stack overflow on deep recursion at test 24 | Hard |
| loopctl.t | 39 | 0 | 67 | `last LABEL` from sub (dynamic scope) | Hard |
| num.t | 46 | 0 | 56 | Crash after 46 | Unknown |
| exp.t | 31 | 1 | 32 | 1 fail on precision | Prob not fixable |

### Interesting Observations

1. **chop.t** outputs `1..#S(PL-BOX :VALUE 148 ...)` — the test plan `$num_tests`
   is a PL-BOX that's not unboxed when printed in `1..$plan`. This is a bug in the
   test harness (test.pl) or how `plan` interpolates variables.

2. **repeat.t** (35/37) has STREAM-DECODING-ERROR at line 264 of generated CL.
   The Perl test has `"\xdd"` which generates raw byte 0xDD in the CL string literal.
   SBCL's UTF-8 reader chokes on this. **Fix**: Escape non-ASCII bytes in generated
   CL string literals as CL character references.

3. **not.t** (18/1) — crashes when trying to call `Scalar::Util::dualvar` even though
   the test has skip logic. The skip block's `pl-eval "use Scalar::Util"` call can't
   load the XS module, but the crash happens OUTSIDE the skip block at a second block
   that tries to call the function.

4. **array.t** improved from 20→41, but still has 40 failures from hash-in-list-assignment.

5. **auto.t** jumped from crash to 38/47! Declaration reordering fixed it.

6. **qq.t** is now fully passing (30/30)! String quoting works.

### Top Quick Wins for Next Session

1. **Fix require reordering regression** — Don't hoist bare `require` as compile-time.
   Would recover oct.t (72+), context.t (6+), possibly length.t (3+). (~81 tests)

2. **Fix non-ASCII byte encoding** — Escape bytes > 127 in generated CL strings.
   Would fix repeat.t crash (37→50 potentially), print.t, index.t, hexfp.t, and
   possibly others with `\xNN` escape sequences. (~30+ tests)

3. **Fix chop.t plan output** — Unbox the plan count. Would make chop.t appear clean
   (already 28/28 passing). Cosmetic but nice.

### Full Results Table

```
Test                 Pass  Fail  Total  Notes
─────────────────────────────────────────────
arith.t               100     0    183  CRASH after 100
pow.t                  75     2    268  CRASH
lc.t                   51     5   2716  CRASH
num.t                  46     0     56  CRASH
split.t                45    19    219  CRASH
array.t                41    40    195  CRASH (hash-in-list-assign)
loopctl.t              39     0     67  CRASH (last from sub)
auto.t                 38     1     47  CRASH
list.t                 38    17     73  CRASH
infnan.t               37     2     39  CRASH
repeat.t               35     2     50  CRASH (UTF-8 encoding)
study.t                35     8     43  exit(1)
ord.t                  33     3     38  exit(1)
chars.t                31     1     34  CRASH
exp.t                  31     1     32  exit(1)
qq.t                   30     0     30  FULLY PASSING!
chop.t                 28     0     28  CRASH (exit hook)
delete.t               24     0     56  CRASH (autoviv)
negate.t               23     1     48  CRASH (tie)
recurse.t              23     0     28  CRASH (stack overflow)
range.t                21     7    162  CRASH
not.t                  18     1     24  CRASH (Scalar::Util)
undef.t                18     6     88  CRASH
unshift.t              17     1     19  CRASH
reset.t                16     8     45  CRASH
concat.t               15     5    254  CRASH (pack)
wantarray.t            14    14     28  exit(1) (DO NOT WORK ON)
dor.t                  13     0     13  FULLY PASSING
lop.t                  13     0     47  CRASH (bareword)
sprintf2.t             12     0     12  CRASH
reverse.t              10     3     25  CRASH
time.t                 10     1     72  CRASH
arith2.t                9     0      9  FULLY PASSING!
chr.t                   9     4     45  CRASH
bool.t                  8     0      8  FULLY PASSING
qr.t                    7    14     37  CRASH
sub.t                   7     3     65  CRASH
bop.t                   6     0    510  CRASH
join.t                  6     0     43  CRASH (tie)
aassign.t               5    20     25  CRASH
int.t                   5     0     19  CRASH (use integer)
or.t                    5     0     14  CRASH
cond.t                  4     0      4  FULLY PASSING
defined.t               4     0      5  test 5 doesn't run
isa.t                   4     0     14  rest don't run
local.t                 4     2      6  CRASH
my.t                    4     0      4  CRASH (fwd decl)
sleep.t                 4     0      4  FULLY PASSING
while.t                 4     0      4  FULLY PASSING
append.t                3     0     13  CRASH
die.t                   3    14     26  CRASH
each.t                  3     1      4  CRASH
grep.t                  3     0     77  CRASH
hashassign.t            3     2    309  CRASH
args.t                  2     0      2  CRASH
bless.t                 2    10    118  CRASH (typeglob)
caller.t                2     5    112  CRASH
defins.t                2     0     27  CRASH (bareword FILE)
if.t                    2     0      2  FULLY PASSING
push.t                  2     0     10  CRASH (autoviv)
vec.t                   2     1     78  CRASH
assignwarn.t            1     0      1  CRASH
hash.t                  1     1      2  CRASH (tie)
kvaslice.t              1     0     38  CRASH
lex.t                   1     1     53  CRASH
switch.t                1     3    197  CRASH
[37 tests at 0 pass omitted]
─────────────────────────────────────────────
TOTAL                1109   259   8323
```

### Blocking Issue Categories

| Category | Tests Blocked | Impact (est. tests recoverable) |
|----------|--------------|--------------------------------|
| **require reordering** | oct, context, length + possibly others | ~81+ |
| **Non-ASCII byte encoding** | repeat, print, index, hexfp, length | ~30+ |
| **Hash-in-list-assign LHS** | array (41→195) | ~100+ |
| **Typeglobs `*FOO`** | auto, sort, bless, ref, local | ~50+ |
| **`tie` not implemented** | hash, join, negate, unshift | ~30+ |
| **`use integer` pragma** | int, bop (partially) | ~20+ |
| **`pack`/`unpack`** | concat, pack, append | ~20+ |
| **Deep recursion / stack** | recurse (23→28) | ~5 |
| **Closures / lexical capture** | closure (0→4) | ~4 |
| **`eval` string form** | die, cmpchain, state | ~30+ |
| **`Scalar::Util` XS** | not (18→24) | ~6 |
