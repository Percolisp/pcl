---
name: project_parent_substr_regression_bisect
description: "RESOLVED s248 except parent.t: substr.t #378 root-caused to box-set stale-class bug (FIXED 748f435); parent.t #7/8 = pre-existing error-text gap (open fix target); defins.t flake = junk file in perl-tests/t polluting glob"
metadata: 
  node_type: memory
  type: project
  originSessionId: 08bda7ff-813c-4e18-95c0-fbe1dc80928b
---

# parent.t / substr.t sweep-diff "regressions" — triage outcome (s248, 2026-06-12)

- **substr.t #378 + #383 — FIXED (commit `748f435`)**. Bisect → `2bc25da`,
  but the real bug was `box-set` never clearing a stale `:CLASS` when a
  plain value overwrote a box that held a blessed ref. Both tests had only
  ever "passed" via stacked accidents (old p-aref char-indexing + stale
  class firing an overload). Rules now: clear class when old value was the
  reference itself (vector/hash/fn/box); KEEP when old value was a plain
  scalar (blessed scalar referent keeps class — qr.t `$$e='Fake!'` stays
  Stew); magic-cell setters receive blessed boxes un-unboxed. Bonus fix:
  concat2.t (fully passing 68→69). Test misc-fixes-02.t #27.

- **parent.t #7/#8 — pre-existing, OPEN fix target.** Eval'd `use parent
  'Nonexistent'` dies `Package NONEXISTENT does not exist` instead of
  perl's `Can't locate Nonexistent.pm in @INC (...)`. Failing since ≤ s232;
  baseline had no rows because parent.t CRASHED at bless time (95bfc9e).
  Matters because `$@ =~ /^Can't locate/` is the standard CPAN idiom for
  detecting optional modules. Likely layer: p-require/eval package
  resolution, NOT lib/parent.pm.

- **defins.t #16 flake**: leftover `perl-tests/t/&=FILE` junk (io-test
  debris from a sweep run) pollutes `glob('*')`. Delete it and defins.t is
  27/27. If it reappears, find the io test that creates it and clean up.

- **qr.t baseline is stale**: single-file runs show 15 fails vs 10 baseline
  rows (#6/11/12/14/16 extra — $$d='Bad' through Regexp ref, SV-identity
  family), identical on old/new runtime; full-sweep runs match baseline.
  Environment/order-dependent, not a code regression.

Related: [[project_moo_progress]]
