---
name: project_power_op_float_divergence
description: "Deferred — Perl ** always returns a float (NV); PCL returns exact bignum. Number-formatting divergence found by fuzzer, parked by user."
metadata: 
  node_type: memory
  type: project
  originSessionId: 8d97b038-770e-4180-91da-3d81fe2a7491
---

**STACKED / DEFERRED (user parked it 2026-06-26, "put that number formatting on the stack").**

The op-fuzzer (`tools/difftest-ops.pl`) found: Perl's `**` always yields a
floating-point NV (C `pow()`), so large powers print via `%.15g`:

- `2**50`  → perl `1.12589990684262e+15`, PCL `1125899906842624` (exact)
- `2**53`  → perl `9.00719925474099e+15`, PCL `9007199254740992`
- `2**53+1`→ perl `9.00719925474099e+15` (precision lost), PCL `9007199254740993`
- `2**81`  → perl `2.41785163922926e+24`, PCL `2417851639229258349412352`

Agree for exponents up to ~2**49 (≤15 sig digits). Divergence is **display**
(IV vs NV `%.15g`) below 2**53 and **value** (precision loss) above it.

**Why PCL diverges (deliberate, load-bearing):** `p-** ` (cl/pcl-runtime.lisp ~1735)
returns an exact bignum when base/exp are non-neg integers within ~1000 bits.
The comment cites pack needing `2**64` exact. Confirmed dependents that rely on
exact `2**N`: `cl/pack-impl.pl` (lines 297 `2**($nbytes*8)`, 1187 `2**$checksum_width`)
and `lib/Math/BigInt/Calc.pm` (mask init `2**$AND_BITS` etc.).

**Path to a faithful fix (if revisited):** make `**` always return double-float
(matching Perl), and give the internal callers an explicit integer-power helper
(pack-impl + BigInt::Calc are OUR transpiled code in `cl/`/`lib/`, editable).
Risk: must not break pack.t / bigint. Alternatively document as a deliberate
divergence like the `split` LHS-arity one in `docs/not-supported.md`.
