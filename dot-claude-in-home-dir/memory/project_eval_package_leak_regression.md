---
name: project_eval_package_leak_regression
description: signatures.t 790->525 regression — package-switch inside eval STRING leaked *pcl-current-package*; FIXED. Plus remaining smaller fully-passing regressions to bisect.
metadata: 
  node_type: memory
  type: project
  originSessionId: c63f4e9a-1ff0-4fda-81e1-d208f9c34086
---

**FIXED (2026-06-28).** `eval "package Foo; ..."` leaked `*pcl-current-package*`
into the caller's dynamic scope, so a LATER `eval "bar()"` resolved bar() in Foo
→ `"The function |Foo|::pl-bar is undefined"`. Cost **~265 tests in
perl-tests/signatures.t (790->525)**. Root cause: commit `6e64eb2` made `p-eval`
*seed* the eval package from `*pcl-current-package*` but didn't *rebind* it; a
`package` stmt inside the eval setf's the global (see `p-defpackage`).
Fix: add `(*pcl-current-package* *pcl-current-package*)` to the `let` that already
rebinds `*package*` in `p-eval` (`cl/pcl-runtime.lisp` ~line 6752) — mirrors the
module-load rebind in `p-require`. Guard: new case in `Pl/t/eval-named-sub-01.t`.
Sweep total 17824->18089 (>= the 18088 from Jun 18). **Original 6e64eb2 goal
(`__PACKAGE__` in eval = caller pkg) still works** — pkg-name is captured before
the let, so it still seeds transpile.

**Bisect method that worked:** `git worktree add` + `git bisect run` a script that
runs `sweep-perl-tests.pl --jobs 1 perl-tests/signatures.t` and greps `pass=N`,
good if N>=700. Clean, no stash. (Confirmed good baseline = `31b2774`, Jun 19;
user's reference sweep `foo4` was Jun 18 00:24 = 18088 pass / 66 files.)

**STILL OPEN — smaller fully-passing regressions (66->62 over Jun18..Jun28), NOT
yet bisected.** These reproduce SOLO (not load-flakiness):
- `push.t` 31->30 (early-stop after test 30)
- `sort.t` 204->200 (now 200/2)
- `unshift.t` 18/19->early-stop at 18
- `chdir.t` 25->2 (POSIX; crashes early)
- `sub.t` 64->59 (fail 0->5)
- `flip.t` 13->11, `state.t` 149->141, `qr.t`/`substr.t` -1 each, `blocks.t` lost 1
Gains over same window: `infnan.t` +15 (now fully passing), `concat.t` +1 (now
full), `scalar.t` +12, `eval.t` +7, `postfixderef.t` +5, `caller.t` +3, `sprintf.t` +2.
Next: bisect push/sort/unshift/sub the same way (good=31b2774).

**Concurrency datapoint:** ran full sweep (`--jobs 8`) AND `prove -j8 Pl/t/`
simultaneously (16 SBCL procs) — no interference, sweep per-file numbers matched
solo re-runs exactly. Machine has headroom.
