# Perl Test Suite Status (as of Session 32, 2026-02-15)

## Overview
- **103 test files** from Perl's `t/op/` directory
- **~1262 passing** across all files (up from ~1192 in session 31, +70)
- **10 fully passing**
- Run with: `cd /home/bernt/pcl && perl run-perl-test.pl perl-tests/TESTNAME.t`

## Session 32 Fixes (low-hanging fruit)
- **`use integer` pragma skip**: int.t 5->18, bop.t unblocked (but hangs)
- **`pl-plan` unbox**: chop.t 28->40, switch.t 1->13 (plan count was boxed)
- **`$/` reference handling**: chop.t gains tests (chomp with `$/ = \3`)
- **`pl-pack`/`pl-unpack` added**: num.t 46->56, concat.t 15->26, sprintf2.t 2->12, append.t 3->5
- **`utf8`/`POSIX` stub packages**: for future use

## Session 31 Fixes
- **require reordering fixed**: oct.t 0->77, context.t 0->6
- **UTF-8 encoding fixed**: repeat.t 35->39

## Fully Passing Tests (10)
qq.t (30), arith2.t (9), dor.t (13), bool.t (8), cond.t (4),
defined.t (4), isa.t (4), if.t (2), sleep.t (4), while.t (4)

## Top Performers (20+ passing)
arith.t (100), oct.t (77), pow.t (75), num.t (56), lc.t (51),
split.t (45), array.t (41), chop.t (40), loopctl.t (39), repeat.t (39),
auto.t (38), list.t (38), infnan.t (37), study.t (35), ord.t (33),
chars.t (31), exp.t (31), qq.t (30), concat.t (26), delete.t (24),
negate.t (23), recurse.t (23), range.t (21)

## Major Blocking Issue Categories (sorted by impact)
| Category | Files affected | Est. tests recoverable |
|----------|---------------|----------------------|
| Hash-in-list-assign LHS | array.t etc | ~100+ |
| `tie` not implemented | join,hash,negate,chr,or | ~30+ |
| TYPE_ERROR (box/type mismatch) | 11 | ~30+ |
| UNDEF_FUNC (missing functions) | 11 | ~30+ |
| TRANSPILE_FAIL (parser limits) | 5 | ~15+ |
| Codegen unbalanced parens | index,hexfp | ~15+ |
| `use integer` semantics | bop.t (hangs) | ~100+ if implemented |

## Notes
- "UNBOUND: THREAD" in sweep reports is a **false category** — sweep regex
  picks up "THREAD" from SBCL's backtrace header, not an actual variable.
  Actual crash reasons vary per file.
- bop.t hangs (510 tests, probably infinite loop related to `use integer` being no-op)
- `use integer` pragma needs discussion before implementing (SESSION_STATE.md note)

## Full results table
See `pcl/memory/session-31-report.md` for session 31 sweep.
