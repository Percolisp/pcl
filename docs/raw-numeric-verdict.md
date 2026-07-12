# The `raw-numeric` and `raw-string` verdicts — use-proven eager coercion

Design note (s286, discussed with user).  Status: **designed, not implemented**;
scheduled after the counting-loop lowering (`docs/bench-exec-investigation.md`
fix menu).  Companion to the existing box/raw split in `docs/ir-spec.md` §2.2.

Two symmetric verdicts: if every use of a lexical is provably a *numeric*
operation, coerce non-plain writes eagerly with the `+ 0` equivalent
(`%pcl-to-number`) and keep the slot a host number (**raw-numeric**); if every
use is a *string* operation, coerce with the `. ""` equivalent (`to-string`)
and keep the slot a host string (**raw-string**).  Both are Perl's own SV
conversion caches (NOK/POK) pre-filled at compile time.

**Key fact that makes both sound for references** (checked s286,
`cl/pcl-runtime.lisp:1206`): PCL ref identity — `==` on refs, refaddr, and the
`0x…` in `ARRAY(0x…)` stringification — is **not a memory address**; it is a
monotonic counter ID from a weak eq-table, stable for the object's lifetime
and never reused.  Nothing ever parses the string back to reach the data (and
couldn't — the layout is different anyway); real code only uses these values
as *identity tokens* (`$seen{$ref}++`, `"$a" eq "$b"`, `$r1 == $r2`).  Frozen
at the write or computed at each use, the token is byte-identical.  Therefore
**neither verdict needs a "no references" guarantee** — the only per-use
machinery an eager freeze skips is `use overload` (`""`/`0+` are code that
must run per use), so the gate is only:

> **Flag / closed-world condition:** no `use overload` in the transpiled
> corpus and no string `eval` in scope (an eval'd string could introduce
> one); manual override flag for programs the scan can't clear.

## Scope boundary — this verdict vs the SHIPPED counting-loop raw var (s287 Q&A)

Do not conflate the two regimes (user question, answered s287):

- **`p-foreach-range-raw` (shipped, s286b) needs NO flag, no scan, no
  strict check.**  Its raw slot is sound by *construction + veto*:
  (a) the values bound to the loop var come only from the range, and range
  elements are fresh plain scalars — perl numifies/stringifies the
  endpoints once at range construction, so even `(1..$overloaded_obj)`
  yields plain integers; no ref, overloaded object, or dualvar can ever BE
  a range element, regardless of what a runtime `eval` loads later;
  (b) every body write must be arith-shaped or the verdict is refused;
  (c) any `eval` word in the region sets `eval-in-region` and the loop
  falls back to the boxed `p-foreach-range` — the eval hole is closed by
  REFUSING the optimization, not by trusting an assumption.  (Verified:
  `for my $i (1..3) { eval q[$i+1] }` emits the boxed variant.)
- **THIS verdict (raw-numeric/raw-string, unimplemented) is the opposite
  regime**: the initial value comes from an arbitrary expression
  (`$ENV{N}`, `$h{k}`, `$s += $_` accumulators) and CAN be a ref, an
  overloaded object, or a dualvar.  That is why it needs the no-overload
  corpus scan (the "flag" above) — and because a string `eval` can load
  `use overload` AFTER the scan ran, the scan alone is unsound: the
  strict checked coercion (§below) at every raw write is the non-optional
  backstop that turns the remaining hole into a loud die instead of
  silent corruption.

Rule of thumb for implementers: a raw slot is either **provenance-pure**
(all reaching values proven plain by construction, evals vetoed — no
runtime check needed) or **scan-licensed** (arbitrary provenance — scan +
strict checked write, and the check is never weakened).  Nothing in
between ships.

Related: perl's own endpoints-once semantics make the counting loop
observably identical to perl even when the end variable is assigned inside
the body (verified vs perl 5.40; see `docs/ir-spec.md` §6.2, including the
one divergence: range elements are read-only in perl, writable in PCL).

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

| value class | hazard under eager coercion | killed by |
|---|---|---|
| plain number/string | none — conversion of an immutable plain value is pure and stable, so once-at-write ≡ at-each-use | — |
| dualvar (`$!`-family) | none given the use-proof: every use would have picked the same (numeric resp. string) side anyway | use-proof |
| undef | value-safe (`0` resp. `""` everywhere the licensed uses would see them); see warning caveat | use-proof (`defined` disqualifies) |
| reference | identity is a **stable counter ID** (see above), so frozen ≡ live for `==`, `eq`, hash-keying; only `use overload` (per-use code) can observe the freeze | **the flag** (no-overload corpus scan) |
| blessed ref (no overload) | `Class=HASH(0x…)` bakes the class name in at the write; a later cross-handle `bless` diverges — but re-bless visibility through scalar copies is already documented not-supported (`docs/not-supported.md` §scalar copy), and `ref($x)` uses disqualify | footnote, accepted |

## The use-sets — and the boolean-context asymmetry

**raw-numeric licensing uses:** `+ - * / % **`, `++/--`, `== != < <= > >= <=>`,
bitwise ops, range endpoints (`A..$n`), array index (`$a[$q]`), repeat count
(`LIST x $q`).

**raw-string licensing uses:** interpolation `"$q"`, `.`/`.=`,
`eq ne lt gt le ge cmp`, `length`, rvalue `substr`/`index`, `lc uc lcfirst
ucfirst`, hash-key `$h{$q}`, regex match/subst *target* (`$q =~ …` reading),
`split` target, `print`/`say`/`join`/sprintf-`%s` argument — **and boolean
context** (see below).

**Boolean context (`if ($q)` / `while` / `&& || !` / `?:` condition) is the
asymmetric case — the classic trap:**

- For **raw-numeric** it DISQUALIFIES: Perl truthiness is defined on the
  *string* form, so `"0.0"`, `"00"`, `"0E0"`, `" "` are **true**, but their
  numified `0` is false.  A boolean test is NOT a numeric use.
- For **raw-string** it is LICENSED: truthiness of the string form is
  truthiness of the value, for every value class — `"0.0"` stays true,
  `0`→`"0"` stays false, undef→`""` stays false, any ref→`"ARRAY(…)"` stays
  true.

**Disqualifying uses for both** (any one → verdict stays boxed/raw as today):

- the *other* verdict's coercions (a numeric op disqualifies raw-string and
  vice versa — mixed-use variables stay as they are);
- `defined($q)` — eager coercion turns undef into a defined `0`/`""`;
- `ref($q)`, any dereference (`$$q`, `$q->…`), `bless`;
- any call argument (unknown callee), `\$q`, `local`, tie/glob contact,
  closure capture (the existing box vetoes all still apply first).

## Checked coercion — die loud when the assumption is violated (user, s286)

The write wrapper is the perfect checkpoint: writes are rare, uses are hot, so
a defensive check at the write costs ~nothing while the uses stay
unconditionally fast.  The wrappers are therefore **strict**:
`%pcl-to-number-strict` / `%pcl-to-string-strict` inspect the incoming value
and **die** (hard, with the variable name and coercion kind in the message)
instead of silently discarding information when:

1. **an overload-capable blessed ref arrives** — a class with `""`/`0+`
   overload.  This catches at runtime the one hole in the corpus scan: a
   string `eval` that loaded `use overload` after transpile time.  Plain and
   blessed-but-unoverloaded refs pass (stable-ID argument, table above).
2. **a genuine dualvar arrives** — detectable as: p-box with `sv-ok` ∧
   `nv-ok` ∧ `nv ≠ (to-number sv)` (ordinary cache-warm boxes have consistent
   caches; share this predicate with `Scalar::Util::isdual`, one definition).
   Strictly, the use-proof makes dualvars *safe* to coerce (every licensed
   use would pick the same side) — but eager coercion irreversibly destroys
   the other side, so if the use-classifier ever has a bug, the failure would
   be silent data corruption.  Dying is the honest mode: same philosophy as
   the pipeline's "gates die hard" invariant.

If the die ever fires on real code, that is the signal to either fix the
classifier bug it exposed or re-box that variable — never to weaken the check.

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
   against the tables above; verdict `raw-numeric` (all uses numeric) or
   `raw-string` (all uses string/boolean) when all writes are
   {coerced ∪ wrappable} and the flag allows.
2. Codegen: raw slot as today; wrap each non-coerced write RHS in
   `%pcl-to-number` resp. `to-string` (both exist; the `+ 0` / `. ""` paths).
   Coerced writes (already operator results of the right family) stay bare.
3. Flag plumbing: corpus scan (no `use overload`, no string-eval) sets the
   default; `PCL_ASSUME_PLAIN=0/1` (name TBD) overrides.
4. Guards: Pl/t transpile tests for the `"0.0"`-truthiness disqualifier (bool
   blocks raw-numeric, licenses raw-string), the `defined` disqualifier, the
   `$ENV{N}` bench shape going raw-numeric, and a `$seen{$ref}` string-identity
   round-trip under raw-string; `tools/difftest-ops.pl` fuzz over both axes.
5. Synergy: a `raw-string` accumulator whose writes are `.=`-shaped is exactly
   the slot the W15.8 append fix wants to make an adjustable fill-pointer
   string — implement the verdict first, the append transform rides on it.

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
  Mostly subsumed now that raw-string covers the string-used case; keep only
  for genuinely mixed-use element-seeded scalars if they ever show up hot.
- **Counting-loop lowering** (`for (A..B)` → endpoints-once, no vector): still
  first — it kills the dominant range-materialization tax AND coerces hot loop
  bounds once as a side effect, independent of any verdict change.
