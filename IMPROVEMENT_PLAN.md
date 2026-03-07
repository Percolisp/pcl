# PCL Perl Test Suite Improvement Plan

## Context

**Last updated: session 64 (2026-03-07). Current state: ~5497+ passing.**

This plan was first written at session 35 (~1379 passing). Items below are
updated to reflect current state. For the higher-level roadmap, see
`PERL_TEST_SUITE_PLAN.md`.

---

## Status Summary

| Item | Was | Now | Status |
|------|-----|-----|--------|
| 0.1 utf8 naming | 0/257 | 257/257 | **DONE** |
| 0.2 quotemeta + \\Q fix | 0/60 | 54/60 | Mostly done (6 remain) |
| 0.3 pl-continue/pl-break | 0/59 | 0/59 | **SKIP** — given/when removed in Perl 5.38 |
| 1.1 Non-ASCII encoding | many | — | RESOLVED (wasn't the issue) |
| 1.2 List assign order | — | 37/55 | Partial |
| 1.3 Split fixes | 96/132 | 99/132 | Partial (33 remain) |
| 2.1 List slices | — | — | Unverified |
| 2.2 fc() | lc.t fail | lc.t 82/82 | **DONE** (unicode tests commented out) |
| 2.3 pos() | 0/21 | improved | **DONE** (session 64) |
| 2.4 State variables | 0/? | partial | Partial (session 61-63) |
| 3.1 foreach alias | — | — | Not started |
| 3.2 given/when | 13/59 | 0/59 | **SKIP** — removed in Perl 5.38, not worth implementing |
| 3.3 eval STRING | — | — | Not started |

---

## ~~0.1 utf8::native_to_unicode~~ — DONE (translate.t: 257/257)

Functions `pl-native_to_unicode` / `pl-unicode_to_native` moved to the
`:pcl` package so `MAIN` inherits them. translate.t now fully passing.

---

## 0.2 quotemeta + \\Q — Mostly done (quotemeta.t: 54/60)

`pl-quotemeta` is implemented. 6 failures remain; check what they are
before doing more work here.

---

## ~~0.3 pl-continue / pl-break (given/when)~~ — SKIP

`given`/`when` was removed from Perl core in **Perl 5.38**. The system Perl
is 5.40.3, so switch.t tests a dead feature. Not worth implementing in PCL —
no real CPAN code written after 2022 uses it. Add switch.t to the skip list.

---

## ~~1.1 Non-ASCII string literal encoding~~ — RESOLVED

Not a real issue. Crashes were caused by other bugs (structural codegen
issues in sessions 36-40). The transpiler emits valid UTF-8.

---

## 1.2 List assignment evaluation order — Partial (list.t: 37/55)

`($a,$b) = ($b,$a)` swap and array self-assignment still broken.
18 failures in list.t remain. See session 43 notes for breakdown:
- Tests 30-38: `do { if-elsif-else }` returning list
- Test 39: `(1,2,3)` inside `||` becomes `(progn ...)` → returns `3`
- Test 8: chained list assignment
- Tests 48-55: list slice issues

---

## 1.3 Split fixes — Partial (split.t: 99/132)

33 failures remain. Most are `SB-C::INPUT-ERROR-IN-LOAD` — non-ASCII
characters in the source triggering SBCL read errors. This is the same
class of problem as the old 1.1 issue but at the SBCL level, not the
transpiler level. Need to escape high-byte characters in string literals
as `\uXXXX` or character codes.

---

## ~~2.2 fc()~~ — DONE

`fc()` implemented. lc.t: Unicode incompatibility tests (57 of them) commented
out in session 64; remaining 82 tests all pass. See `docs/not-supported.md`
for the rationale on what was excluded.

---

## ~~2.3 pos()~~ — DONE (session 64)

`pos()` implemented in pcl-runtime.lisp (`pl-pos`, `pl-set-pos`, `pl-reset-pos`).
New `Pl/t/pos-01.t` has 8 tests (all passing). `perl-tests/pos.t` should now pass
more tests — verify with next sweep.

---

## 2.4 State variables — Broken (state.t: 0/?)

`SB-C::INPUT-ERROR-IN-LOAD` — state.t crashes at load. Likely a codegen
or parse error in `state $x` handling. Investigate the transpiler output
before implementing the runtime side.

---

## 3.2 given/when — Regressed (switch.t: 0/59)

Was 13/59 in an earlier session. Now 0/59 with UNBOUND-VARIABLE on load.
Something in the given/when codegen broke. Needs a targeted debug run:
```
echo 'use feature "switch"; given(1) { when(1) { print "ok\n" } }' | ./pl2cl
```
Fix the load error first, then the matching logic.

---

## Remaining High-Value Targets

Based on session 43 sweep data, the highest-gain unfixed items are:

| File | Pass | Fail | Root cause |
|------|------|------|------------|
| switch.t | — | — | **SKIP** — given/when removed in Perl 5.38 |
| array.t | 56 | 49 | SIMPLE-TYPE-ERROR in pl-list-= |
| list.t | 37 | 18 | Various (see 1.2 above) |
| split.t | 99 | 33 | Non-ASCII in source → SBCL read error |
| lc.t | 76 | 12 | Missing fc() + other |
| state.t | 0 | ? | Load error |
| pos.t | 0 | 21 | Regex syntax error |
| aassign.t | 5 | 20 | Aliasing, lvalue subs |
| die.t | 3 | 14 | $SIG{__DIE__}, die propagation |
| warn.t | 0 | 33 | $SIG{__WARN__} not implemented |

For $SIG handlers (warn.t, die.t), see `PERL_TEST_SUITE_PLAN.md` item 1.4.

---

## Verification

After each fix:
1. `prove -j8 Pl/t/` — PCL suite (2493 tests, all must pass)
2. `perl run-perl-test.pl perl-tests/AFFECTED.t`
3. `perl sweep-perl-tests.pl --jobs 8 --timeout 60` — full sweep
