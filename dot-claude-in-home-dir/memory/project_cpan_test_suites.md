---
name: project_cpan_test_suites
description: "Running CPAN modules' own t/ test suites through PCL — Test::More enabler + survey + cluster catalog"
metadata: 
  node_type: memory
  type: project
  originSessionId: 00d2b825-a53f-4eed-ae62-e7f107f08cbe
---

**Session 243 (2026-06-09/10): made CPAN modules' OWN test suites runnable through PCL, unmodified.**

## The enabler (commits 2d942d1, bf98e6b)
CPAN `.t` files `use Test::More`, which on this perl is the **Test2 stack** — depends on XS
internals (`Test2::API::Instance::PL-IPC`) PCL can't run, so every suite (and perl-tests/parent.t)
crashed at load. PCL already had the TAP subs (`pl-ok`/`pl-is`/… in `cl/pcl-test.lisp`) + harness;
the missing wire was routing `use Test::More` to them.
- `*p-pcl-provided-modules*` (Test::More/Test::Simple/Test2::Bundle::More) in pcl-runtime.lisp:
  `p-use` skips loading the real .pm; the already-loaded subs resolve. **No .t file is edited.**
- Added missing subs: `is_deeply` (recursive deep-equal over boxes/arefs/hrefs), `use_ok`,
  `require_ok`, `isa_ok`, `can_ok`, `explain` — exported from :pcl in pcl-test.lisp.
- **LOAD-ON-DEMAND** (user insisted: "can't have the test suite loaded for no reason"):
  `use Test::More` → `p-ensure-test-lib` loads `cl/pcl-test.lisp` ONCE (guarded on `(fboundp 'pl-ok)`),
  using `*pcl-runtime-directory*`. `runpcl` NO LONGER preloads pcl-test.lisp; runt/sweep still do (the
  `require './test.pl'` perl-tests need it). So a non-test program never loads the test layer, and a
  .t is self-contained (works under standalone `pcl` too). **STILL TODO: `subtest` (nested plans).**

## Survey (unmodified suites via runpcl)
List::Util 38 files: 4 PASS / 8 PART / 26 fail; Role::Tiny 23: 1/0/22; Try::Tiny ~13: 2/2/7.
The suites RUN now and many assertions pass (List::Util 117 ok). Reds concentrate in 3 root clusters.

## Cluster catalog (triage by shared root cause, NOT file-by-file)
- **A — shim export gaps** (the "not exported" crashes): real modules export subs our lib/ shims
  lacked. **DONE as DYING STUBS** (commit cc29fa9, per user "just add die statements first"):
  Scalar::Util += refaddr, unweaken; List::Util += maxstr, minstr, reductions, sample,
  zip_{longest,shortest}, mesh_{longest,shortest}. Because `use` runs the real Exporter, a missing
  export hard-dies the WHOLE file; stubs make import succeed, only a CALL dies. weak.t: crash→13 ok.
  Real impls are the obvious follow-up (maxstr/minstr trivial).
- **B — `PPI::Structure::Condition` codegen gap** (the "unknown type" crashes): **FIXED** (cc29fa9).
  `return X if (A) || (B)` — when a postfix conditional's condition STARTS with a parenthesised group
  + operator, PPI mislabels the leading `(A)` as Structure::Condition (its `if (...)` bracket) not
  Structure::List; `PExpr::parse()` only knew List. Now parse() treats Condition == List (both just a
  paren expr). Math::BigInt: 12 transpile errors → 0. Regression test Pl/t/misc-fixes-02.t.
- **C — Role::Tiny / module-path** (the "Can't locate" crashes), NOT yet done: (1) `with 'MyRole'`
  not installing role methods into the consumer (`Can't locate object method "bar"`) — core feature;
  (2) `My::Example` → `My//Example.pm` double-slash path bug loading t/lib helpers.

## Deeper blockers found (beyond the clusters)
- **Math::BigInt**: after the Condition fix it transpiles clean but dies at runtime
  `Can't find class ~A for SUPER:: call` — SUPER:: resolution in its class hierarchy. Blocks lln.t/
  sum.t/max.t/product.t (all `use Math::BigInt`). Separate deep issue.
- **Stale module cache gotcha**: `~/.pcl-cache` caches transpiled modules; `runpcl`'s
  `*pcl-skip-cache* t` does NOT clear the per-module cache. After a codegen fix, `rm -f ~/.pcl-cache/*`
  before re-testing module-loading files, or you'll see the OLD (broken) transpilation.

## Survey harness
`/tmp/cpan_survey.pl` (per-file ok/notok/status across a module's t/). Module suites on disk:
`~/.cpan/build/Role-Tiny-2.002004-0/t`, `~/.cpan/build/Scalar-List-Utils-1.70-0/t`,
`~/.cpanm/work/*/Try-Tiny-0.32/t`. See [[project_cpan_module_survey]], [[project_difftest_fuzzer]].
