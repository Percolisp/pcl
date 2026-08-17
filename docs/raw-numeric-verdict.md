# The `raw-numeric` and `raw-string` verdicts — use-proven eager coercion

Design note (s286, discussed with user).  Status: **step 1 + A-num SHIPPED
(s302); B-num/B-str freeze verdicts SHIPPED (s303, task #62) — implementation
notes below.**  Companion to the box/raw split in `docs/ir-spec.md` §2.2.

## Implementation notes (s303, as shipped)

- **Use classifier**: `Pl/VarAnnotator.pm` threads a use-class (`num`/`str`/
  `bool`/opaque-default) through `_tw_walk`; whitelist tables `%USE_NUM_OP`,
  `%USE_STR_OP`, `%USE_FN` (builtin arg positions), `%USE_BOOL_THROUGH_OP`
  (&&/|| pass bool through ONLY in bool context).  Reads hidden inside
  interpolatable quote-like LEAF tokens (regex patterns, backticks, heredocs)
  are scanned textually → `str`, except deref forms (`"$q->[0]"`) → opaque.
  Conditions (`if`/`while`/modifiers) walk with a `bool` root.
- **Verdict**: fires only when the var's ONLY blocking reasons are
  `write-shape`/`write-incdec-root`, it is not a sub param, and no
  parse-fallback text mentions it.  `PCL_OPT=-raw-numeric` disables (the
  optimization registry, Pl::Passes, s411; `PCL_NO_RAW_VERDICT=1` is its alias);
  `PCL_B_DEBUG=1` dumps per-var verdict/uses.  A `use overload` in the FILE
  disables both verdicts (`Parser2::{_overload_in_file}`); cross-file
  overloaded arrivals are the strict coercers' job.
- **Emission**: `Parser2::_wrap_freeze` wraps EVERY native write (decl init
  + root `$x = RHS;`) uniformly — a proven-arith RHS just pays one typecheck
  at the rare write.  Compound/incdec writes go through the existing `-raw`
  twins (a str-family compound like `.=` cannot reach a B-num slot: the
  compound's own read of `$x` classifies as that family's use and blocks the
  other verdict).
- **Runtime**: `%pcl-to-number-strict`/`%pcl-to-string-strict` first apply
  `%pcl-scalar-collapse` — box-set's scalar-assignment aggregate rule (raw
  adjustable vector → count, raw hash-table → key count) — so
  `my $n = @a = split ...` keeps its count semantics under the freeze.
- **Additional type-sensitive exclusions found while shipping** (beyond the
  s302 corrections): unary minus (`-"abc"` is string negation → opaque) and
  `//` (a DEFINED-ness test — frozen undef is a defined `0`/`""` → opaque).
- Guards: `Pl/t/raw-verdict-01.t` (verdict matrix + runtime fidelity incl.
  "0.0"-truthiness, ref stable-ID round-trip, aggregate collapse); regime
  pins updated in `parser2-01.t`/`parser2-02.t`.
- Measured (bench shape from §Problem, 2M-iteration cfor with `$ENV{N}`
  bound): boxed 0.128s → frozen 0.027s (4.7×); perl 0.088s — PCL moves from
  ~1.5× slower than perl to ~3× faster on this shape.

## S1 — the str-buffer append slot (shipped s303, same session)

The §Implementation-sketch item 5 synergy, built directly on the
use-classifier: a raw slot (plain-unboxable or B-str) qualifies as a
**str-buffer** when

- its writes are ONLY plain roots and `.=` compounds (`write_ops` ⊆ {.=};
  `x=`/bitwise/incdec block it),
- every use is a TRANSIENT stringify/boolean read — hash-key uses get their
  own class `strkey` (licenses B-str, but the table RETAINS the key object,
  so it must block in-place mutation), and
- it is not a foreach range var (the loop macro binds the var itself; there
  is no buffer init to append into).

Emission: plain writes wrap in `(%pcl-str-buffer V)` (fresh adjustable
fill-pointer string — REPLACING on assignment means stale aliases cannot
exist), `.=` lowers to `(%pcl-str-append $s V)` (extend + `replace`, O(1)
amortized; self-append `$s .= $s` is safe — source region [0,n) never
overlaps destination [n,2n)).  The escape analysis is the same whitelist
that licenses B-str: returns, call args, box stores, container stores are
all opaque → a buffer object can never leave the generated code, so host
CL calling PCL output only ever sees ordinary simple strings (consumer
contract note in `ir-spec.md` §2.2).

Measured (1M × 8-char appends): pre-S1 this shape was the one bench loss
(~1050× slower than perl at s302); with the buffer 0.052s vs perl 0.028s —
**1.9× slower**, a ~500× improvement.  Guards: S1 block in
`Pl/t/raw-verdict-01.t` (verdict matrix + self-append/alias runtime).

## Shipped so far (s302) — the provenance-pure extensions

Neither of these needs the flag/scan or the strict coercers — they extend the
write-provenance regime (all stored values operator-coerced raw), so they ship
first:

1. **Step 1 — coercing compound assigns as raw writes.**  A statement-root
   `$x OP= RHS` with a coercing op (`+= -= *= /= %= **= x= .= <<= >>= &= |=
   ^= &.= |.= ^.=`) no longer boxes `$x`; Parser2 lowers it through the op's
   **`-raw` macro twin** (`p-incf-raw` …), defined by `%define-compound-pair`
   from the SAME new-value builder as the boxed macro (cannot drift).  `||=
   &&= //=` store the RHS unchanged (may be a ref) → still box.  Seam/
   modifier/embedded positions still box (they lower through box-set).

2. **A-num — root `$x++;`/`$x--;` statements on numeric-write-family slots.**
   The write-shape oracle now returns the stored value's FAMILY ('num' =
   numeric-op result/number literal/num-family compound; 'str' = `.`-family/
   quote literal/interpolation/str-family compound).  A root-statement
   incdec is allowed on a raw slot iff EVERY other write is num-family —
   then the value can never be a non-numeric string, perl's ++ on it is
   plain numeric, and `p-incf-raw`/`p-decf-raw` match perl exactly.  Tail
   position: postfix emits `(prog1 $x (p-incf-raw $x))` (old value).  The
   `&= |= ^=` bit-compounds count as 'str' (perl dispatches `& | ^` to
   STRING bitwise when both operands are strings — not provably numeric).
   This regime **replaced the s286b C-for ++-step carve-out** and fixed its
   latent bug: a string-seeded counter (`for (my $i = "aa"; $i ne "ad";
   $i++)`) was numified (0) and the loop HUNG; the family gate keeps it
   boxed so magical string increment runs.

   Known residual (accepted, also listed under `++/--` licensing below): a
   var whose writes are all numeric can still magically-increment in perl
   ONLY via values this regime proves impossible, so A-num itself has no
   magical-increment divergence.  The scan-licensed B-num regime below DOES
   carry one (frozen `"5a"` increments numerically, perl magically) — noted
   for when it is implemented.

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

> **Corrections for the implementer (s302, found while shipping A-num):** two
> entries above are TYPE-SENSITIVE in perl and must NOT license raw-numeric —
> freezing flips a runtime dispatch:
> - `& | ^` dispatch to **string bitwise** when both operands are strings
>   (`"12" & "3"`); a frozen number turns that into numeric AND.  Only
>   `<< >>` are safe (always numeric).
> - **range endpoints**: `$a .. $b` runs perl's **magical string range**
>   (`"aa".."ad"`) when the endpoints are non-numeric strings; freezing
>   selects the numeric range.
> Both must classify as opaque (disqualifying).  Same class of trap, already
> accepted-and-documented for `++/--`: a frozen non-numeric string increments
> numerically where perl would do the magical increment (B-num only — the
> shipped A-num regime proves such strings impossible instead).

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
