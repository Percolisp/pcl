# bop.t Analysis (updated session 100, 2026-03-26)

510 tests. **207 pass** (up from 188 session 99). No longer crashes.

## Status by section (207/453 run, 57 tests 454-510 not run due to crash at 454)

```
Start End    Total   Section                                     Pass/Fail
---------------------------------------------------------------------------
1     6      6       Basic numeric &|^~                          6/6 (100%)
7     7      1       use integer ~0 sign check                   1/1 (100%) ✅ fixed session 100
8     12     5       Large UV shifts                             5/5 (100%)
13    14     2       use integer negative shifts                 2/2 (100%) ✅ fixed session 100
15    15     1       use integer huge negative shift             1/1 (100%)
16    20     5       Signed/unsigned cusp &|^                    5/5 (100%) ✅ fixed session 100
21    35     15      String bitwise short/long/const             15/15 (100%)
36    38     3       COW numeric |= &= ^=                        3/3 (100%)
39    47     9       Numeric double-magic                        9/9 (100%)
48    144    97      Tie double-magic (~100 tests)               73/97 (75%)
145   165    21      vec() + UTF-8 flag                          16/21 (76%)
166   175    10      use feature bitwise (numeric ops)           9/10 (90%)
176   201    26      use v5.27 bitwise repeat                    26/26 (100%)
202   214    13      ref/undef/glob bitwise                      12/13 (92%)
215   453    239     PVBM + object overload + $SIG{__WARN__}     25/239 (10%)
```

## Fixed in session 100
- **use integer bitwise semantics** — `~0`, `&`, `|`, `^`, `<<`, `>>` under `use integer` now return signed 64-bit results.
  - Added `p-to-s64`, `p-<<-int`, `p->>-int` to runtime; extended `use_integer` block in ExprToCL.pm
  - Tests 7, 13-15, 16-20 now all pass (was 1/8)

## Fixed in session 99
- **String bitwise** (tests 21-35) — `p-bit-and`/`p-bit-or`/`p-bit-xor` detect non-numeric string operands, do char-by-char ops. `p-to-s64`, `p-<<-int`, `p->>-int` added to runtime.
- **`new CLASS ARGS` indirect object syntax** — pre-pass in `handle_subcalls` before right-to-left loop.

## Remaining failures by category

### Tie double-magic (tests 48-144, 24 failures)
FETCH called twice when should be called once. PCL's tie has a double-eval problem.
`fetches($x)` returns 2 when expected 1. Requires tie semantics to be fixed.

### vec write fetch (tests 149, 154)
vec lvalue triggers 1 FETCH (should be 2 for read-modify-write). Also tie-related.

### UTF-8 flag (tests 145-165, 5 failures)
`utf8::is_utf8()`, `utf8::upgrade()`, `~` on UTF-8 strings. Skip per user request.

### `~.` string-force complement (tests 172, 186)
`~.` is `use feature "bitwise"` string-force NOT. PPI tokenizes `~.22` as `~` then `.22` (float) — wrong.
Fix requires detecting `~.` pattern at parse time.

### `$warn` counting (tests 202, 215)
`local $SIG{__WARN__} = sub { $warn++ }` — counts warnings from bitwise ops on refs.
PCL doesn't implement `$SIG{__WARN__}` call protocol. `$warn` stays 0 instead of expected 10.

### Ref stringification for bitwise (tests 216-453, ~199 failures)
`\1 | "x"` should stringify the ref ("SCALAR(0x...)") and do string bitwise.
PCL's `p-backslash(1) = make-p-box(1)` → box-set strips to 1 → stringifies as "1" not "SCALAR(0x...)".
**Additional blocker**: expected values in tests are computed via `eval qq/chr($co $op $so)/`.
PCL's `p-eval` can't evaluate these Perl expressions → expected values are wrong strings.
Fixing would require both `p-backslash` double-boxing AND full `p-eval` (string eval).

### Crash at test 454
`new version ~$_` triggers `p-method-call("version", ...)` but the `version` module isn't loaded.
Tests 455-510 (Config/XS/fresh_perl_is/string eval) are out of scope anyway.

## Priority fixes for next session
1. **~. operator** (tests 172, 186) — detect `~` + `.N` token in PExpr.pm when `use feature "bitwise"` active
2. **Tie FETCH double-call** (tests 48-144, 24 failures) — investigate why FETCH is called twice for read ops
3. **Broader improvements** — consider bop.t-unrelated targets: sort.t remaining 18, B1 bare-if return
