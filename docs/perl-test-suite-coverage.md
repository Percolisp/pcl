# Perl Test Suite Coverage Analysis

Surveyed: `t/op/` (227 files) and `t/base/` (9 files) from Perl 5.40.3.
Our coverage: `perl-tests/` has ~60 of those files, covering all the major operator/control-flow areas well.

## High-value gaps — feature implemented, no test file

These are the most actionable: PCL already supports the feature, we just haven't
pulled the authoritative test file in. Each one is likely to surface real bugs.

| File | Lines | Feature |
|------|-------|---------|
| `tr.t` | 1214 | `tr///` — implemented, zero coverage |
| `eval.t` | 770 | `eval "string"` — implemented (session 104) |
| `magic.t` | 983 | `$_`, `$!`, `$,`, `$.`, `$\` and other magic vars |
| `postfixderef.t` | 383 | `$ref->@*`, `$ref->%*` postfix deref |
| `lex.t` | 400 | lexical variable scoping edge cases |
| `lex_assign.t` | 417 | lexical variable assignment edge cases |
| `rs.t` | 300 | `$/` record separator, `local $/` tricks |
| `repeat.t` | 276 | `x` string/list repeat operator |
| `filetest.t` | 395 | `-e`, `-f`, `-d`, `-r` etc. |
| `numconvert.t` | 283 | string↔number coercion edge cases |
| `override.t` | 179 | overriding built-ins with `CORE::` |
| `rand.t` | 140 | `rand` / `srand` |
| `yadayada.t` | 94 | `...` (yada yada) operator |

**Priority pick**: `tr.t` (1214 lines, fully in scope) and `eval.t` (770 lines, feature complete).

## New Perl 5.34–5.36 features — not yet implemented

These files test syntax PCL has not implemented. Worth noting but lower priority
than the gap files above.

| File | Lines | Feature |
|------|-------|---------|
| `try.t` | 336 | `try { } catch ($e) { }` (5.34+) |
| `for-many.t` | 501 | `for my ($x, $y) (LIST)` multi-var for (5.36+) |
| `defer.t` | 287 | `defer { }` block (5.36+) |
| `catch.t` | 56 | `catch` keyword details |

## Files we have that are NOT in Perl's t/op or t/base

These are PCL-specific tests or tests we wrote ourselves:

- `arith.t` — basic arithmetic (we wrote this; Perl uses `opbasic/` instead)
- `concat.t` — string concatenation (Perl folds this into other test files)
- `errno_test.t` — errno handling (our own)
- `min_local.t` — minimal local test (our own)
- `qq.t` — quoting operators (our own)

## Clearly out of scope — do not pull

Threading, taint mode, tie system, SV internals, stash/symbol-table internals,
removed features (smartmatch/given/when), process control, signal dispatch,
lvalue subs.

Specific files: `threads.t`, `threads-dirh.t`, `lock.t`, `taint.t`, `utftaint.t`,
`tie.t`, `tiearray.t`, `tiehash.t`, `tiehandle.t`, `studytied.t`,
`stash.t`, `gv.t`, `symbolcache.t`, `svflags.t`, `upgrade.t`, `avhv.t`,
`smartmatch.t`, `switch.t`, `lvref.t`, `sub_lval.t`, `fork.t`, `exec.t`,
`waitpid.t`, `sigdispatch.t`, `sysio.t`, `dbm.t`, `evalbytes.t`.

## Recommended workflow for adding a new file

1. Copy `t/op/foo.t` → `perl-tests/foo.t`
2. Run `perl sweep-perl-tests.pl --jobs 1 perl-tests/foo.t` to see baseline failures
3. Inspect `/tmp/foo.lisp` (via `./clt foo`) for patterns
4. Write focused `Pl/t/foo-01.t` for the failing cases
5. Fix, verify sweep improves, commit
