
# Stable booleans (perl 5.36+) in PCL -- design (s516, Fable, 2026-10-12; task #3242)

**Status: BUILT (s517a, task #3242); §4 carries the measurements.**  Written so that #3242 can be an Opus brief
member instead of a parked question.  The measurements below are static reads
of the runtime; the two dynamic measurements the design needs are named in §6
and belong to the agent that builds it.

## 1. What perl does (5.36 and later; the oracle is 5.40.3)

Perl has two immortal boolean SVs, `PL_sv_yes` and `PL_sv_no`: dualvars whose
IV is 1 / 0 and whose PV is the shared static string `"1"` / `""`.  Since 5.36
an SV "is a boolean" (`SvIsBOOL`) when its PV buffer IS that shared static
string -- so the property is carried by the STRING BUFFER's identity, and:

- it SURVIVES every copy that shares the buffer: `my $x = !!1`, a sub's
  return value, storing into an array or hash element, `local`, list
  assignment, `wantarray` returns;
- it is LOST by anything that makes a new value: `+ 0`, `. ""`, `.=`, `++`,
  `sprintf`, `substr`, interpolation (`"$b"` is a plain `"1"`);
- `!!EXPR` re-booleanises anything.

Producers (5.36 perldelta, to be CONFIRMED one by one with `builtin::is_bool`
in the agent's probe -- the list is the probe's input, not its output): `!`,
`not`, every comparison and equality operator (`== != < > <= >=`, `eq ne lt gt
le ge`), `defined`, `exists`, `isa`, `builtin::true` / `false`,
`builtin::is_bool` itself.  NOT producers: `&&` / `||` / `//` (they return an
operand), `<=>` / `cmp` (-1 / 0 / 1), `grep` in scalar context (a count),
`delete`, `-X` filetests (probe: perl 5.40 may or may not).

Consumer that matters today: `JSON::PP` 4.x.  `use constant CORE_BOOL =>
defined &builtin::is_bool;` -- TRUE under PCL, because PCL registers
`builtin::is_bool` in `*p-declared-subs*` -- and then `builtin::is_bool($v)`
decides `true` / `false` in the encoder (PP.pm line 488) and `core_bools` in
the decoder.  Under PCL `is_bool` always answers false, so `encode_json([!!1,
!!0])` writes `[1,""]` where perl writes `[true,false]`: JSON-PP's
core_bools.t 9 rows, 003_types.t 2, 118_boolean_values.t 4 (the board's #3242
rows).

## 2. What PCL does today (measured on main 5de67310)

- A Perl boolean is produced by SIX runtime sites: `p-bool` (inline; `(if x 1
  "")`), called from the two comparison macro families `%def-overloaded-cmp` /
  `%def-overloaded-str-cmp` and from `p-exists`, `p-exists-array`,
  `p-sub-exists`; plus `p-!` (`(if (p-true-p a) "" 1)`, also `not`) and
  `p-defined` (`(if (%pcl-definedp val) 1 "")`).  `p-isa` is boolean-valued by
  #1737's rule.
- TRUE is the fixnum 1 -- an IMMEDIATE, `eq` to every other 1, so no identity
  can distinguish `!!1` from `1`.  FALSE is a literal `""` -- already one
  shared object per emitting site (a CL string literal in the runtime source).
- `builtin::is_bool` is registered and always returns false ("the safe
  answer", runtime ~line 30895); `created_as_number` / `created_as_string` are
  deliberately undefined so consumers take their flag-free paths.
- The normative text: ir-spec ~line 1757 "Comparison operators return Perl
  booleans: `1` (number) for true, `""` (empty string) for false"; coercion
  table row "host boolean true | 1"; not-supported.md "Interned boolean
  constants (`!0` / `!1` identity)".
- Truthiness fast path `p-true-p`: a number is false iff zero; a string is
  false iff `""` or `"0"` (two `string=` on the string arm); everything else
  takes `%p-true-p-slow`.  `.=` on a box copies the current string into a fresh
  adjustable buffer (`%pcl-str-buffer`) before appending, so a SHARED string
  object held in a box is never mutated in place -- the same protection string
  LITERALS from generated code already rely on.

## 3. The design: two interned strings, `+p-yes+` and `+p-no+`

Perl's own model, transposed: the boolean IS the shared string buffer.

- `+p-yes+` = one `(simple-array character (1))` holding `"1"`, `+p-no+` =
  one `(simple-array character (0))`, both `defconstant`/`defglobal`-interned
  in the runtime and EXPORTED.  Every producer in §2 returns them (ONE helper:
  `p-bool` returns `(if x +p-yes+ +p-no+)`; `p-!` and `p-defined` call it --
  rule 11, no second copy of the pair).
- `builtin::is_bool(v)` = `(or (eq v +p-yes+) (eq v +p-no+))` after unboxing
  one level (a box holding the object IS a boolean scalar); `builtin::true` /
  `false` return the objects.
- Copies keep identity for free: `box-set` stores the object; array / hash
  elements hold raw values (boxed aggregates: elements RAW by default), so an
  element holds the object itself; `.=` copies first (§2).  Arithmetic makes a
  new value by construction.  `!!$x` goes through `p-!` twice.
- Numification `"1"` -> 1 and `""` -> 0 already work through the string arms;
  stringification is the identity.  `print !!1` still prints `1`; `(1==1)+0`
  is 1; `sort { $a <=> $b }` on booleans compares 1 / 0 as before.

What CHANGES observably besides is_bool: nothing a program can see without
`is_bool` or JSON::PP, EXCEPT speed (§4) and `ref`-free identity probes
(`\!!1 == \!!1` is false in PCL today and stays false: a reference is to the
BOX, and each `\` of an rvalue makes a temp box -- perl's `\!0` identity is
the not-supported section's OTHER half, which stays not-supported; the
read-only die `for (!0) { $_ = 43 }` stays absent too, since the write lands
on the loop's alias box, not on the interned object).

## 4. The cost, and the two fast arms that remove it

TRUE stops being a fixnum, so two hot paths see a STRING where they saw a
number:

1. **Truthiness of a stored boolean** (`my $ok = $a == $b; ... if ($ok)`) --
   `p-true-p`'s string arm (two `string=`) instead of `zerop`.  Fast arm: test
   `(eq val +p-yes+)` -> T and `(eq val +p-no+)` -> NIL FIRST, before `numberp`;
   two pointer compares, and the common `(if (p-== ...))` shape never stores,
   because the emitter already tests the comparison's HOST boolean inline
   (ir-spec: "BOOLEAN context is the truth of that same answer").  MEASURE
   that the emitter does so for the stored shape too.
2. **A boolean used as a number** (`$n += ($i % 2 == 0)`, `($x > 0) * 2`) --
   the string arm of `%to-number-raw` (a parse) instead of the fixnum.  Fast
   arm: `(eq v +p-yes+)` -> 1, `(eq v +p-no+)` -> 0 at the top of the string
   arm (or before the typecase).  The raw-numeric Kind-A verdict: find how a
   comparison RESULT is classified when it is stored into a raw-numeric slot
   today (`""` is not a number either, so the verdict must already coerce or
   decline there); confirm with `PCL_OPT=none` A/B that the emission is
   identical and the run is identical.

**MEASURED (s517a, the build).**  The raw-numeric verdict does not classify a
comparison result as a number: `my $n = 0; $n += ($i % 3 == 0)` emits
`(p-incf-raw $n (p-== (p-% $i 3) 0))` -- the slot `$n` is raw, the DELTA is
coerced by `p-incf-raw`'s `(to-number d)`, which reaches `%to-number-raw`'s
new `+p-yes+` / `+p-no+` arms; the stored shape `my $ok = $i % 2 == 0; ... if
($ok)` emits a raw `:scalar` slot holding the boolean and `(p-if $ok …)`, i.e.
`p-true-p`'s new arm.  The emission of both is identical under `PCL_OPT=none`
apart from the slot kinds, and both runs print the same (333333 / 500000).
The `p-true-p` arms sit at the head of the STRING arm, not before `numberp`
(a number pays nothing; a boolean pays one type test and one `eq`).  The two
bench rows `boolsum` / `boolif` are in tools/bench-exec.pl.

Everything else is identity-neutral: `join`, `print`, `sprintf("%s")` take
the string; `sprintf("%d")` numifies through arm 2.

## 5. Where the words go

- `docs/ir-spec.md`: the comparison-operators statement (~1757) and the
  coercion table row become "the interned Perl booleans `+p-yes+` / `+p-no+`,
  whose string value is `"1"` / `""` and whose number value is 1 / 0; identity
  is the boolean flag (`builtin::is_bool`); any new value drops it".
- `docs/not-supported.md` "Interned boolean constants": the IDENTITY half is
  implemented; the REFERENCE-identity and read-only halves stay, re-titled.
- CHANGELOG; `docs/shipped-modules.md` if `lib/builtin.pm` changes.

## 6. The bar for the brief that builds it (one member, runtime + guards)

- Perl-compared guard rows (a new `Pl/t/stable-bool-01.t`): `is_bool` over
  EVERY producer in §1 and over the non-producers (`&&`, `<=>`, `grep`,
  `delete`, a filetest -- as perl 5.40 answers); copy survival (scalar, return,
  element, hash value, `local`, list assign); loss (`+0`, `. ""`, `.=`, `++`,
  `sprintf`, `"$b"`); `!!` re-booleanises; `print`/`join`/`sprintf` text
  unchanged; `JSON::PP::encode_json([!!1, !!0, 1, "", 0])` = `[true,false,1,"",0]`
  with the CORE JSON::PP (perl 5.40 ships 4.16) and with the dist's 4.18 under
  `PERL5LIB`; `decode_json('[true,false]')` with `core_bools`.
- The two dynamic measurements: `tools/bench-exec.pl` base / tree / base, the
  control band, plus a NEW row `boolsum` (the counting idiom `$n += ($i % 3 ==
  0)` over 1e6) and a `boolif` row (store-then-test) -- the fast arms of §4
  must hold both within the band; a cost above it is FLAGGED and the fast arms
  are the first thing to re-check.  `PCL_OPT=none` runs identically.
- The board: JSON-PP's 15 #3242 rows leave the blessed file by edit; nothing
  else moves (a row that moves by a `1` -> `true` in some OTHER dist is read,
  not blessed over).  Sweep + companion (op/not.t, op/defined.t, op/exists_sub.t,
  cmp/*) as the cl/ row of the WHAT-TO-RUN table says; everyday.
- Not in scope: `created_as_number` / `created_as_string` (SV flags PCL does
  not track -- a separate, larger question), `\!0` reference identity, the
  read-only die.

## 7. Alternatives considered and rejected

- **A struct type with two singletons.**  Every `typecase` in the runtime that
  dispatches on a value's kind (to-number, to-string, truthiness, `ref`,
  sprintf, join, print, sort, the box-set arms, pack) would need a new arm:
  hundreds of sites, and the raw-slot optimizer would meet a kind it has never
  seen.  The interned-string design reuses the string arms everywhere and adds
  two `eq` tests in two places.
- **A flag on the box (IOK/POK-style).**  PCL's value representation IS the
  raw object: a boolean stored into a raw array element, a raw-numeric slot or
  a hash value has no box to carry a flag, and `my $x = $y` would have to copy
  it.  The property must live on the object, which is exactly what the shared
  string gives -- and what perl itself does.
- **Leave `is_bool` false and special-case JSON::PP in a shim.**  Rule 9a: a
  module's behaviour never goes into the runtime, and a shim that lies about
  `is_bool` for one consumer fixes 15 rows while every other 5.36+ idiom stays
  wrong.
