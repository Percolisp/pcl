# The `raw-numeric` verdict — use-proven eager numeric coercion

Design note (s286, discussed with user).  Status: **designed, not implemented**;
scheduled after the counting-loop lowering (`docs/bench-exec-investigation.md`
fix menu).  Companion to the existing box/raw split in `docs/ir-spec.md` §2.2.

## Problem

The VarAnnotator's raw-slot verdict is **write-provenance-driven**: a `my`
scalar goes raw only when every write's RHS is operator-coerced (literals,
arith/string operator results — values provably plain).  A bare element read
(`my $n = $ENV{N}`, `my $q = $h{k}`) is not coerced — the element value may be
a reference (which is a p-box in PCL's value model) — so the variable stays
boxed even when every subsequent *use* is numeric.  Cost in hot code: box
indirection + generic dispatch per use (measured: cfor @2M 0.017s literal-bound
vs 0.073s env-N-boxed-bound).

## Design: a third verdict, `raw-numeric`

If (a) a **flag** guarantees no references flow into the slot, and (b) every
*use* of the variable is a **numeric operation**, then non-coerced writes are
wrapped in an eager numeric coercion — the compile-time equivalent of the user
writing `+ 0`:

```lisp
(setf $n (%pcl-to-number (p-gethash %ENV "N")))   ; slot = genuine host number
```

The slot then holds a real host number for its whole extent, so R1's inline
fixnum/double fast paths fire per use with no box, no nv-cache check, and no
per-iteration string re-parse.

This is **Perl's own optimization moved to compile time**: Perl caches the
numeric conversion in the SV (the NOK flag) after first numification; here the
compiler proves the cache can be pre-filled at the write.

## Soundness — each condition kills a different hazard

| value class | hazard under eager `+0` | killed by |
|---|---|---|
| plain number/string | none — numification of an immutable plain value is pure and stable, so once-at-write ≡ at-each-use | — |
| dualvar (`$!`-family) | none **given (b)**: every use would have picked the numeric side anyway | use-proof |
| undef | value-safe (0 everywhere a numeric use would see 0); see warning caveat | use-proof (`defined` disqualifies) |
| reference | `$q == $r` with `$r` a second live handle: frozen-at-write address vs live address can disagree after a GC move where Perl says equal; `0+` overload would run early | **the flag** — no use-analysis can see this |

The flag's semantics: "no reference values reach raw-numeric slots."  It can
default ON when the closed world allows it — the transpiled corpus contains no
`use overload` and no string `eval` in scope (an eval'd string can introduce
anything) — with a manual override for programs the heuristic can't clear.

## The use-set — and the boolean-context trap

**Licensing (numeric) uses:** `+ - * / % **`, `++/--`, `== != < <= > >= <=>`,
bitwise ops, range endpoints (`A..$n`), array index (`$a[$q]`), repeat count
(`LIST x $q`).

**Disqualifying uses** (any one → verdict stays boxed/raw as today):

- string uses: interpolation `"$q"`, `.`, `.=`, `eq ne lt gt cmp`, `length`,
  hash-key `$h{$q}`, `print`/`join`/any list-op arg;
- **boolean context** — `if ($q)` / `while ($q)` / `&&` / `||` / `?:`
  condition position.  This is the classic trap: Perl truthiness is defined on
  the *string* form, so `"0.0"`, `"00"`, `"0E0"`, `" "` are **true**, but
  their numified `0` is false.  A boolean test is NOT a numeric use;
- `defined($q)` — eager coercion turns undef into a defined `0`;
- any call argument (unknown callee), `\$q`, `local`, tie/glob contact,
  closure capture (the existing box vetoes all still apply first).

## Known residual divergence: warning fidelity (values all match)

Perl warns "isn't numeric" / "Use of uninitialized value" **per conversion**:
undef used numerically twice warns twice.  Eager coercion warns **once, at the
write** — and warns even when the numeric use sits in a branch that never runs.
Warning *count and timing* diverge; every produced value is identical.  A few
sweep tests match warning text/count (tr.t-style) — if one trips, register the
divergence, don't weaken the verdict.

## Implementation sketch

1. `Pl/VarAnnotator.pm` already walks every use of each lexical (that's how
   the `\$q`/capture vetoes work).  Add a per-variable use-classification
   against the table above; verdict `raw-numeric` when all writes are
   {coerced ∪ wrappable} and all uses are numeric and the flag allows.
2. Codegen: raw slot as today; wrap each non-coerced write RHS in
   `%pcl-to-number` (exists; the `+ 0` path).  Coerced writes (already
   numeric-op results) stay bare.
3. Flag plumbing: corpus scan (no `use overload`, no string-eval) sets the
   default; `PCL_ASSUME_PLAIN=0/1` (name TBD) overrides.
4. Guards: Pl/t transpile tests for the `"0.0"`-truthiness disqualifier, the
   `defined` disqualifier, and the `$ENV{N}` bench shape going raw-numeric;
   `tools/difftest-ops.pl` fuzz pass over the numeric-use axis.

## Relation to the alternatives considered (s286 discussion)

- **`%ENV`/`%INC` whitelist** (provenance-based): sound and tiny, but subsumed
  by raw-numeric for the bench shapes — `getenv` returns a *string*, and a raw
  string bound re-numifies per iteration (the box at least has an nv-cache);
  raw-numeric stores the number.  Skip the whitelist unless a string-typed
  need appears.
- **Pass-through relaxation** (raw slot may hold a ref-box; ops' generic
  branches absorb it): needs no flag and no use-proof, degrades per-value not
  per-variable — but relaxes the normative ir-spec §2.2 invariant ("a raw slot
  never holds a box or a reference") and so requires a consumer audit + fuzz.
  Keep as a separate, later option for *string*-used element-seeded scalars.
- **Counting-loop lowering** (`for (A..B)` → endpoints-once, no vector): still
  first — it kills the dominant range-materialization tax AND coerces hot loop
  bounds once as a side effect, independent of any verdict change.
