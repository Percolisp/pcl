# Perl Test Suite Status

Tests copied from: `~/perl5/perlbrew/build/perl-5.40.3/perl-5.40.3/t/`

Last updated: 2026-02-23 (Session 45)

## How to Run

```bash
# Run full sweep (from perl-tests/)
perl ../sweep-perl-tests.pl --jobs 8 --timeout 60

# Run single test
perl ../run-perl-test.pl TESTNAME.t

# Copy a new test from Perl source
cp ~/perl5/perlbrew/build/perl-5.40.3/perl-5.40.3/t/op/TESTNAME.t perl-tests/
```

---

## Current Totals

**2889 passing / 440 failing across 101 files (28 fully passing)**
*(Baseline at session 45 start was 1996/412; +893 from BEGIN ordering fix)*

Skipped (known hang): bop.t, heredoc.t

---

## Fully Passing (28 files)

append, args, arith, arith2, assignwarn, blocks, bool, cmpchain, cond,
defined, defins, delete, dor, exp, if, int, isa, loopctl, lop, negate,
num, or, pow, qq, sleep, translate, unshift, while

---

## Zero Passing (35 files)

| Test File | Error | Root Cause |
|-----------|-------|------------|
| anonsub.t | INPUT-ERROR-IN-LOAD | `__DATA__` section / `*DATA` typeglob |
| chdir.t | 0 output | Unknown |
| closure.t | SIMPLE-ERROR crash | Lexical closures (defvar = global, no capture) |
| concat2.t | Crash | `use overload` module loading failure |
| crypt.t | 0 output | Unknown |
| die_exit.t | INPUT-ERROR-IN-LOAD | Parse/codegen error |
| do.t | 0 output | Unknown |
| each.t | INPUT-ERROR-IN-LOAD | `use Hash::Util` fails |
| each_array.t | TYPE-ERROR | `each()` on array not implemented |
| exists_sub.t | UNDEFINED-FUNCTION | Missing runtime functions |
| flip.t | 0/3 fail | `$.` line number not impl; needs harness |
| for.t | 0 output | PPI parse failure at line 767 (exotic `for` syntax) |
| grent.t | UNDEFINED-FUNCTION | `setgrent`/`getgrent` OS-specific |
| hash.t | Crash after 1 | `tie` not implemented |
| hexfp.t | INPUT-ERROR-IN-LOAD | Hex float parsing |
| join.t | UNDEFINED-FUNCTION | `$T` unbound, `tie` not implemented |
| kvaslice.t | SIMPLE-PROGRAM-ERROR | `eval` not named-unary (Bug 1) |
| kvhslice.t | SIMPLE-PROGRAM-ERROR | `eval` not named-unary (Bug 1) |
| lfs.t | 0 skip | No 64-bit file offsets (skips itself) |
| method.t | Timeout (>120s) | Transpilation too slow |
| pack.t | Fatal error | `which_perl()` missing |
| pos.t | SIMPLE-PROGRAM-ERROR | `pos()` semantics wrong |
| print.t | 0 output | Unknown (tempfile was missing, now added) |
| push.t | SIMPLE-TYPE-ERROR | `+($a,$b)` list idiom → wrong type |
| readline.t | UNBOUND-VARIABLE | Bare filehandles (`F`, `IS`) |
| recurse.t | Crash/stack overflow | SBCL control stack limit |
| ref.t | INPUT-ERROR-IN-LOAD | Parse/codegen error |
| signatures.t | UNDEFINED-FUNCTION | `prototype()` built-in not implemented |
| sort.t | Crash/stack overflow | SBCL control stack limit + typeglobs |
| splice.t | TYPE-ERROR | Parse/codegen failure |
| sprintf.t | SIMPLE-ERROR | Requires version.pm |
| state.t | SIMPLE-PROGRAM-ERROR | `eval` not named-unary (Bug 1) |
| substr.t | TRANSPILE_FAIL | PPI parse failure (non-ASCII typeglob) |
| vec.t | TYPE-ERROR | `vec` as lvalue not supported |
| warn.t | UNDEFINED-FUNCTION | `$SIG{__WARN__}` not implemented |

---

## Partial Success

| Test File | Pass/Total | Blocking Issue |
|-----------|------------|----------------|
| lc.t | 76/88 | Unicode locale functions |
| oct.t | 77/79 | 2 tests need real string `eval` |
| list.t | ~37/55 | `do{if/elsif/else}` list return, `||` list context |
| split.t | ~96/132 | Non-ASCII in source → INPUT-ERROR-IN-LOAD |
| array.t | ~56/105 | SIMPLE-TYPE-ERROR in pl-list-= with complex LHS |
| local.t | ~64/105 | `local` hash-slice, complex localization |
| switch.t | ~15/59 | UNBOUND-VARIABLE (given/when scoping) |
| study.t | ~34/43 | `(?{})` regex code blocks not supported |
| chars.t | ~31/32 | Unicode edge case |
| ord.t | ~31/36 | Codepoints > 0x10FFFF |
| not.t | ~18/24 | `Scalar::Util::dualvar` |
| undef.t | ~18/24 | UNDEFINED-FUNCTION |
| aassign.t | ~5/25 | Aliasing ($$scalar), lvalue subs |
| sub.t | ~2/65 | `utf8::encode`, constant sub edge cases |
| die.t | ~3/17 | `$SIG{__DIE__}`, eval/die semantics |
| context.t | ~6/8 | Sub call counting, wantarray |
| my.t | ~8/? | Forward decl scanner too aggressive |
| wantarray.t | ~14/28 | Boxed wantarray values |
| reverse.t | ~10/? | Timeout risk (Tie::Array) |
| concat.t | ~15/20 | `use bytes` pragma |
| chr.t | ~1/9 | `use bytes` pragma |
| time.t | ~10/11 | Crash in gmtime hash test |
| bless.t | ~2/12 | `*TEST` typeglob unbound |
| reset.t | ~16/21 | `??` regex not implemented |
| lex.t | ~1/53 | TYPE-ERROR crash early |
| arith.t | ~16/183 | Float literals `1e999`, early crash |
| exp.t | ~16/32 | Float precision, quadmath |
| grep.t | ~3/7 | Crash at test 4 |
| auto.t | ~?/? | Typeglobs not implemented |
| qr.t | ~0/37 | `object_ok` shadowing |
| length.t | ~3/5 | `BYTES::$^H` unbound |
| die.t | ~4/26 | eval/die semantics |
| sprintf2.t | ~2/8 | UNDEFINED-FUNCTION |

*(Counts marked `~` are from previous sessions and may have changed)*

---

## Known Bugs (priority order)

### Bug 1: `eval` not a named unary — ~243+ tests blocked

`eval` is in `known_no_of_params` as `[0, 1, -2]` but NOT in `named_unary`.
Without named-unary treatment, `eval` consumes all remaining args instead of
stopping at the first comma.

```perl
# state.t line 15:
ok eval 'CORE::state $x = 1;', 'CORE::state outside of feature.pm scope';
# Generates: (PL-EVAL "..." "CORE::state...") — ok() is swallowed!
# Should be: (PL-OK (PL-EVAL "...") "CORE::state...")
```

**Fix:** Add `'eval' => 1` to `named_unary` in `Pl/PExpr/Config.pm`.
**Affects:** kvaslice.t, kvhslice.t, state.t (and likely others)

---

### Bug 2: BEGIN block ordering — FIXED (Session 45)

Phase 2 reordering was classifying BEGIN block `eval-when` forms as runtime,
so `let`-with-`pl-sub` blocks were moved BEFORE the BEGIN's `require './test.pl'`.
Fixed in `_is_compile_time_form` in `Parser.pm`: all `eval-when` forms are now
compile-time (the `pl-require-file` check only applies to `:load-toplevel` forms).
**Gained ~893 tests** (1996 → 2889).

---

### Bug 3: PPI parse fails on exotic syntax — ~490 tests blocked

- **for.t** (775 lines): fails at line 767: `for ${*$f} (5,11,33) {`
  PPI error: "Illegal state in 'for' compound statement"
- **substr.t** (921 lines): fails at line 772: `substr $t, 0, 0, *ワルド;`
  PPI error on non-ASCII typeglob identifier

Both are at the end of large files. PPI returns `undef` for the whole file.

**Workaround:** Binary-search for the first failing line in `Parser.pm`'s
`_build_ppi_doc`, strip from there, parse the rest. Would recover ~100 (for.t)
+ ~390 (substr.t) tests.

---

### Bug 4: Missing/broken special variables

- `*DATA` — `__DATA__` section → anonsub.t crashes entirely
- `$^A` — format/write accumulator → index.t loses 17 tests at test 49
- Bare filehandles (`F`, `IS`) → crashes readline.t, reset.t

---

### Bug 5: Missing functions

| Function | Files blocked | Tests |
|----------|--------------|-------|
| `pl-tie` / `pl-untie` | join.t, hash.t | 43+ |
| `pl-prototype` | signatures.t | unknown |
| `pl-setgrent` etc. | grent.t | OS-specific |

---

### Bug 6: `+($a, $b)` list idiom mishandled — push.t

`return +($first, $second)` transpiles to `(pl-+ (progn $first $second))`.
`pl-+` numifies the arrayref → 0. Then `pl-cast-@ 0` = 0 → type error.
`+($a, $b)` (Perl list-context idiom) should be `(progn $a $b)`, not `pl-+`.

---

### Bug 7: sprintf positional args — 47+ failures in sprintf2.t

`%1$s`, `%2$s`, `%NNN$s` positional format specifiers not implemented.

---

### Bug 8: Stack overflow — recurse.t, sort.t

Both hit SBCL's control stack limit. Need investigation of specific recursion patterns.

---

## Priority Summary

| Bug | Fix difficulty | Est. tests gained |
|-----|---------------|------------------|
| eval → named_unary | Easy (Config.pm) | ~243 |
| PPI parse fallback | Medium (Parser.pm) | ~490 |
| sprintf positional args | Medium (pcl-runtime.lisp) | 47 |
| `+()` list idiom | Medium (PExpr.pm) | ~unknown |
| pl-tie stub | Hard | 43+ |
| Missing special vars | Easy-Medium | ~30 |
| BEGIN block ordering | **DONE** (Session 45) | +893 |

---

## Fixed Bugs (historical)

| # | Bug | Fixed |
|---|-----|-------|
| 1 | test.pl stub shadowing | Early session |
| 2 | Undeclared variables crash | Mostly fixed (defvar scan) |
| 3 | `q{}` quoting | Fixed |
| 4 | Transpiler stderr corruption | Session 17 |
| 5 | `$#array` lvalue | Session 18 |
| 6 | `-bareword` parser issue | Session 19 |
| 7 | Hash in list assignment (RHS) | Session 20 |
| 8 | Array/hash ref boxing | Session 20 |
| 9 | Array/hash on LHS of list assignment | Session 42 |
| 10 | `use vars` support | Session 42 |
| 11 | Chain comparison (N-term) | Session 41 |
| 12 | Fake glob in `_fix_ppi_glob_after_block` | Session 41 |
| 13 | `\&Func` coderef | Session 42 |
| 14 | `use integer` pragma | Session 43 |
| 15 | `local $x` save/restore | Session 43 |
| 16 | `my(@arr, %hash)` params crash | Session 43 |
| 17 | Non-ASCII string negation (`-"Ā"`) | Session 44 |
| 18 | Wide chars in oct/hex | Session 44 |
| 19 | `-N**exp` precedence | Session 44 |
| 20 | `pl-array-= scalar` case | Session 44 |
| 21 | BEGIN block ordering (phase 2) | Session 45 |
