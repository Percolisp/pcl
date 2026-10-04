---
name: project_bigint_multiply_and_array_holes
description: "BigInt multiply fix (s252): compound-assign-on-element bug FIXED (shipped); multi-chunk multiply STILL blocked by sparse-array-holes — holes stored as raw nil vanish in %p-flatten-list (bmul rounding-args path), and converting them to undef collides with Exporter hash-export. Real fix = distinct hole marker at (setf p-aref), touches exists/delete."
metadata:
  node_type: memory
  type: project
  originSessionId: 46ee55e1-bc6e-4c11-a431-e6b88a55aa13
---

# Math::BigInt multiply: one bug FIXED, one DEFERRED (session 252)

After the case-sensitivity fix made Math::BigInt LOAD, multiply was wrong.
Two distinct general bugs were behind it. Investigation chain (white-box, the
right approach): `$x*2` wrong → `_mul` DIRECT correct → `bmul` wrong → `round`
DIRECT correct → step-by-step bmul replication → `round(@r)` got `a=2 nargs=0`
(the multiplicand `$y`=2 landed in the accuracy slot) → `@r=(undef,undef,undef,$y)`
arrived with leading holes DROPPED.

## BUG 1 — compound-assign on a container element (FIXED, SHIPPED)
`$xv->[0] *= $yv->[0]` (and `$h{k} .= …`, `$a[i] /= …`, every `OP=` except
`+=`/`-=`) used `(box-set ,place …)` in the compound-assign macros. `box-set`
silently NO-OPs on a non-box, and `(p-aref-deref …)`/`(p-gethash …)` return raw
VALUES, not boxes — so the store was lost (`100*3` → stayed `100`). `+=`/`-=`
already special-cased accessor places.
**Fix** (`cl/pcl-runtime.lisp`): new `%p-accessor-place-p` + `%p-store-back`
macro (setf for accessor places, box-set for boxes — mirrors p-incf); rewrote
`p-*= p-/= p-%= p-**= p-.= p-str-x= p-bit-and/or/xor= p-<<= p->>=
p-str-bit-and/or/xor=` to use it. Compile-time dispatch → ZERO runtime cost; the
box-scalar path expands byte-identically to before. Tests in misc-fixes-02.t.
This fixed **single-chunk** BigInt multiply (`999999999² = 999999998000000001`)
and is broadly useful. `_mul` direct is now fully correct (incl. multi-chunk).

## BUG 2 — array holes vanish when flattened (ATTEMPTED, REVERTED, DEFERRED)
`_mul` is correct, but `bmul` does `my @r; $r[3]=$y; … $x->round(@r)` where the
overload-arg array has HOLES at 0..2. Holes are stored as **raw `nil`** inside
the adjustable vector (`(setf p-aref)` `cl/pcl-runtime.lisp` ~4757,
`vector-push-extend nil` — comment "so exists returns false"). `%p-flatten-list`
(the p-list-= RHS flattener, ~3285) DROPS raw nil (`((null item) nil)`), so the
3 leading holes vanish → `round` sees `(2)` not `(undef,undef,undef,2)` → `$y`
becomes accuracy=2 → multi-chunk product rounded to 2 sig figs
(`12345678901234567890*2` → `25000000000000000000`). NOTE `p-flatten-args`
(call-args path) KEEPS raw nil (reads as undef downstream) — that's the
inconsistency.

**Attempt (REVERTED):** make `%p-flatten-list` convert nil→`*p-undef*` for
elements of an ADJUSTABLE vector (real @array), keep dropping standalone nil.
Fixed multiply (`24691357802469135780` ✓) and passed empty-list/delete-mid
sanity — **but the gate caught a regression**: `use-require-01` test 37
(`%Config`). Minimal repro: `package M; use Exporter 'import'; our @EXPORT =
qw(%Stuff); our %Stuff=(...)` → `use M` dies `"" is not exported by the M
module`. Exporter::Heavy's **hash-variable export** path has an internal raw nil
that MUST drop; my change turned it into undef→`""` → treated as an export
symbol. Plain Exporter (sub exports via @ISA or `use Exporter 'import'`) is
UNAFFECTED — only `@EXPORT = qw(%var)` (a `%`/sigil var export) breaks.

**Why it can't be fixed in the flattener:** a hole-nil (→ should be undef) and
Exporter's internal nil (→ should drop) are the SAME raw `nil`, indistinguishable
at `%p-flatten-list`. They must be separated at the SOURCE.

## The REAL fix (next time) — distinct hole marker
Store holes as `*p-undef*` (= `:undef`, NOT a p-box) instead of raw `nil` at the
autoviv-extend sites: `(setf p-aref)` ~4757 and `p-aref-box` ~4780 (and audit
`p-array-set`/splice/`delete`). Then:
- `exists $a[hole]` = `(p-box-p (aref a i))` → `:undef` is not a box → still
  false ✓ (`p-exists-array` ~5727).
- read `$a[hole]` → `p-aref-unbox-elem` already maps `:undef`→undef ✓.
- flatten keeps `*p-undef*` (real value) → holes flatten to undef ✓ (fixes bmul);
  Exporter's raw nil still drops ✓.
RISK: many sites assume "raw nil in array = hole"; `delete` stores nil; audit
all `(aref a i)`/`(null …)`/`(p-box-p …)` element checks. This is the documented
**sparse-array (holes)** limitation (`docs/not-supported.md`) — broad/risky,
needs its own focused session + full gate. Until then: single-chunk BigInt
multiply works; multi-chunk (and `pack('w', bignum)`) stay limited.

See [[project_math_bigint_shim]], [[project_case_sensitivity_general_fix]].
