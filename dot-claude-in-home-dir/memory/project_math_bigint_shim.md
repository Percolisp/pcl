---
name: project_math_bigint_shim
description: "pack.t timeout root cause = Math::BigInt::Calc precision-probe infinite loop under PCL bignums; shim in progress, still blocked on _base_len"
metadata: 
  node_type: memory
  type: project
  originSessionId: 46ee55e1-bc6e-4c11-a431-e6b88a55aa13
---

**pack.t TIMEOUT root cause (s252, definitive):** pack.t's SKIP block does
`eval q{ use Math::BigInt }`, which loads `Math::BigInt::Calc`. Calc's `BEGIN`
(line ~220) auto-detects the platform base length with an **empty-condition
`for(;;)` precision-probe loop** that `last`s only when `"9"x$e * "9"x$e` loses
native float/64-bit precision (perl rolls over ~e=9-16). **PCL uses
arbitrary-precision bignums → product always exact → loop never breaks → HANG.**
Confirmed: 6-line repro, perl rolls over, PCL doesn't through e=41. Transpile is
fast (~1.4s/module); only the BEGIN *execution* hangs.

Diagnosis method that worked: env-gated `LOAD-START`/`LOAD-END` trace in
`p-load-module-cached` (reverted) → showed Lib loads fine, Calc's own body hangs
after Lib returns → only top-level exec code is the BEGIN probe loop.

**User chose: write a Math::BigInt shim** (over fail-fast/document-only). Also
asked: **document WHY carefully — in code AND a docs list** (done:
`lib/Math/BigInt/Calc.pm` header + new §7 table in `docs/shipped-modules.md`).

**Shipped this session (clean, committable):**
- `lib/Math/BigInt/Calc.pm` = verbatim copy of real Calc, BEGIN probe loops
  replaced by hardcoded `MAX_EXP_F=MAX_EXP_I=9`, `__PACKAGE__->_base_len(9,1)`.
  **Kills the hang.** Math::BigInt + Lib already transpile/load fine unchanged.
- **FIXED a real general bug** `Pl/ExprToCL.pm` ~line 1885: `__PACKAGE__->method`
  emitted `(p-method-call (p-resolve-invocant "__PACKAGE__") …)` → dispatched on
  a class literally named "__PACKAGE__". Now compile-time-resolves the invocant
  to the current package (mirrors `print __PACKAGE__`). Regression test added
  Pl/t/file-line-01.t (Test 21b). file-line-01.t passes 30/30. Common idiom →
  helps many modules.

**STILL BLOCKED (next session):** `Math::BigInt::Calc->_base_len(9,1)` croaks
**"The base length must be a positive integer"** (its guard
`defined($bl) && $bl==int($bl) && $bl>0`). Symptoms: `_new`/`_str` work alone but
give WRONG result (`_new("12345")` → str 18422165010) because BASE_LEN never got
set → so the croak fires inside _base_len and the `my $BASE_LEN` etc. stay unset.
Passing literal `9` (not the `my $MAX_EXP_*` lexicals) did NOT help → the arg/box
or `==int()` check is misbehaving for a method-call arg in a module BEGIN under
PCL. The ternary-list-assign itself is fine standalone. **Investigate: how
`_base_len(9,1)` receives/compares its first arg when called from a module
BEGIN block via string-eval `require`.** Then verify `pack('w*', BigInt)` and
re-run pack.t (should go from TIMEOUT → mostly passing).

Cache gotcha: `rm -rf ~/.pcl-cache` between runs (runpcl's `*pcl-skip-cache*`
still read stale `~/.pcl-cache/*.lisp` in backtraces). See [[project_cpan_test_suites]].
