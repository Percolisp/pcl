# The runtime, reviewed against perl's own implementation (s525r, Fable fork, 2026-10-24)

The USER's question B of the parser review: "Should we do a deep review of the
runtime, too?" -- answered YES in `docs/parser-review-s525.md` §9, and this is
that review, written for later presentation.  Same method as the parser review:
measure first, perl 5.40.3's own source as the reference
(`~/perl5/perlbrew/build/perl-5.40.3/perl-5.40.3/`), inventories with line
refs, options sized with the number that proves each.  Measured on main
`6eb33b12` (code `ef5df65a`); no code was changed; one task filed (#3760).

Two words a reader needs.  A **box** (`p-box`, `cl/pcl-runtime.lisp:2883`) is
PCL's Perl scalar: a mutable cell holding the value plus cached numeric and
string views, a blessing class and a "made by `\`" flag -- the analogue of
perl's SV.  A **raw slot** is a `my` scalar the compiler has proven only ever
holds a plain number or string, emitted as a bare host variable with no box at
all (`docs/ir-spec.md` §2.2); every operator accepts either form and unboxes
internally.  The **generation stamp** (`*pcl-cache-generation*`, today
`v2-8284`) is the string every emission-changing commit bumps so that cached
module translations built by an older compiler are never reused.

## 0. The answer in ten lines

1. **The runtime is sane as an artifact AND, unlike the parser, mostly sane as
   a structure.**  Each concern has ONE mechanism: one coercion path (190
   `to-number` and 213 `to-string` call sites feed one function each), one
   value reader (`unbox`) that performs tie FETCH and magic get, one tie entry
   macro family (`%p-when-tied`, 27 uses), duplication at 0.85 % of the file
   (347 deletable lines of 40,771, the s411-s413 dedup held).
2. **Its growth is perl's surface, not accretion.**  +36,292 / -9,852 lines in
   729 commits over 90 days (net +26,440; 1,929 new top-level definitions;
   7,492 of the added lines are comments) -- the twelve largest commits are
   tie on aggregates, %SIG, filehandle layers, boxed aggregates, match
   variables, system databases, the fasl module cache, overload, the cache
   manifest, Unicode tables, zlib, File::Glob: one perl mechanism each.
3. **Every divergence the probes found is the REPRESENTATION, never an op.**
   25 one-liners: 20 identical to perl; the 5 that differ all trace to what the
   box does not remember -- the IV/UV/NV range faces (RULED unbounded, #1513 /
   #1515 / #3692) and the "has this scalar been used as a number" flag
   (#1040, and the new #3760: a negative shift operand is a UV in perl).
4. **The structural debt is one thing, in three faces:** the box carries
   perl's caches (`nv-ok` ≈ pNOK) but not perl's flags, so (a) numeric range
   has no 64-bit boundary (ruled), (b) a raw string has no history and a
   string-vs-number decision guesses with `looks-like-number`, (c) every
   operator asks "box or raw?" by hand -- 341 `p-box-p`, 294 `stringp`, 186
   `vectorp`, 116 `hash-table-p` tests and only 5 `typecase`s.
5. **A second lowering layer lives here:** 12 macros dispatch on the CODE
   SHAPE of their place argument (`p-setf` has 16 `(eq (car place) ...)` arms);
   the compiler emits a generic store and the runtime macro re-derives what the
   compiler knew.
6. **Rule 10 and rule 12 are mostly held:** 41 of 1,849 functions exceed 80
   lines, 7 exceed 200 (`p-list-=` 446, `p-method-call` 346, `p-setf` 340,
   `sprintf-one` 305); 66 `(t nil|0|"")` default arms remain, and the five
   sampled are legitimate empty answers.
7. **The four s509 speed levers (#2770-#2773) are all done**; what is left is
   the s509 "not cheap" list and the box-or-raw tax of point 4c.
8. **Recommendation:** no restructuring program.  One Opus filler session
   closes the string-vs-number guess exactly (perl's flag is already in the
   box; a raw string IS the flag), folds the six flipflop variants, re-audits
   the 66 default arms; the box-or-raw entry dispatch becomes ONE typed entry
   per op family in a perf-slot round with a bench bar; the IV/UV/NV faces
   stay parked by the USER's ruling; the shape macros are documented as the
   IR's assembler now and shrink when the parser's lowering is rebuilt.
9. The parser needs a new structure; the runtime needs a few named repairs.
10. Nothing in the runtime blocks the parser program, and the parser program
    changes nothing here.

## 1. Is the value model one mechanism?

### 1a. The representation, against perl's SV

| perl SV (`sv.h`) | PCL box (`p-box`, :2883) | verdict |
|---|---|---|
| `IOK`/`NOK`/`POK` -- which views are VALID (sv.h:414-416) | `nv-ok`, `sv-ok` -- cached views valid (`nv-ok` is three-valued: NIL / T derived / `:DUAL` a true dualvar, #1230) | same idea; PCL has no separate integer-vs-float validity because CL integers and doubles are distinct host types |
| `pIOK`/`pNOK`/`pPOK` -- PRIVATE validity: "this scalar has been used as a number" (sv.h:419-421) | `nv-ok` plays this role for a box; **a raw slot has no flag at all** | the gap behind #1040 / #3760 (see 1c) |
| `IVisUV`, IV vs NV, overflow to NV (sv.h:516; `pp_hot.c` `PERL_PRESERVE_IVUV` :1827-1914) | CL integers are unbounded; doubles are doubles | RULED: `docs/not-supported.md` "Integers are unbounded" (USER 2026-09-07, #1513; faces #1515, #3692) |
| `ROK` + the referent | `is-ref` + a box whose value is the referent | same |
| blessing: on the REFERENT's stash | `class` on the referent box (#1489f9d3) | same since s325 |
| `UTF8` flag: bytes vs characters per SV (sv.h:493) | strings are always character strings; octets are a FILEHANDLE property (layers, #1115) | a design difference, documented; `utf8::valid` answers 1 (#3613) |
| `GMG`/`RMG` magic flags, a magic chain per SV (`mg.c`) | the box's VALUE is a `p-tie-proxy` or a `p-magic-cell` (kinds seen: `:strbuf :memfh :tielem :lvalue :defelem :signal`) | same shape: the cell sits where perl's chain hangs |

So the box is perl's SV with its caches and without its flags.  Perl keeps
the flags because its ops branch on them (`SvNIOKp(left) || SvNIOKp(right)` at
`pp.c:2527` picks the numeric bitwise op; `SvIV_please_nomg` at `pp_hot.c:1914`
keeps `+` integer-exact); PCL branches on the host TYPE of the value instead.
Where the type says the same thing as the flag, the ops agree with perl (20 of
25 probes); where it does not -- a string that looks numeric, an integer past
64 bits -- they diverge, and those are exactly the five.

### 1b. Coercion: one path, as rule 11 asks

| question | the one function (line, length) | call sites | inline re-implementations |
|---|---|---|---|
| numify | `to-number` :6505 (17) → `%to-number-raw` :6475 → `parse-perl-number` :4488 (143) | 190 | 24 `parse-integer` sites: 8 in `p-oct`, the rest in hex / version / regex / escape / services parsing -- all legitimate (they parse perl SYNTAX, not scalars) |
| stringify | `to-string` :6819 (32) → `stringify-value` :5387 (90) → `%p-fixnum-string` :5238 | 213 | 61 `(format nil "~a/~d/~f/~e/~g")` sites: `sprintf-format-float-e/-f`, the six flipflop variants, diagnostics -- legitimate |
| truthiness | `p-true-p` :5734 (11) → `%p-true-p-slow` :5637 (72) | 49 | none found |
| numeric-looking string | `looks-like-number` :5756 (67) | 4 | none |
| double coercion | 16 `(coerce … 'double-float)` sites: division, `sqrt`/`log`, epoch seconds, `sprintf` | -- | legitimate arithmetic |

Verdict: coercion is ONE mechanism.  The s386/s411 reviews' duplication
findings for the compiler do not recur here.  The ir-spec §3.1-§3.3 tables
describe what the code does (checked against `parse-perl-number` and
`stringify-value`).

### 1c. What the box does not remember -- the measured consequence

`%p-bitwise-kind-of` (:13338) decides string-vs-numeric for `& | ^ ~`:

```lisp
((stringp val) (if (looks-like-number val) :number :string))   ; perl: SvNIOKp
```

perl asks a FLAG ("was this scalar ever numified"); PCL asks the TEXT.  The
probe that separates the two models:

```
my $x = "12"; my $y = $x + 0;  print $x | "3", " ", "12" | "3", " ", 12 | "3";
perl: 15 32 15        # $x gained pIOK by being added; the literal "12" never did
PCL : 15 15 15        # #1040: a numeric-LOOKING string takes the numeric op
```

The information is NOT missing from PCL's representation: a box that has been
numified has `nv-ok` set (that IS pNOK), and a raw string value has, by
construction, never been numified (a raw slot that is used as a number holds a
number).  So the exact rule is "a box with `nv-ok` → `:number`; a raw string →
`:string`" -- no `looks-like-number`, no cost.  This is option R1 in §10.

### 1d. The box-or-raw tax -- how often the runtime asks a value's type

| test | sites |
|---|---|
| `(p-box-p` | 341 |
| `(stringp` | 294 |
| `(vectorp` | 186 |
| `(hash-table-p` | 116 |
| `(numberp` | 57 |
| `(functionp` | 56 |
| `(unbox` | 261, of which 41 are the inline idiom `(if (p-box-p x) (unbox x) x)` |
| `(typecase` / `(etypecase` | 5 / 2 |

About a thousand hand-written representation tests in `cond` arms, and seven
type dispatches.  Perl pays the same question as `SvTYPE(sv)` + a flag test,
once per `pp_` function, in one `switch`.  This is the runtime's one real
STRUCTURAL finding: two representations (box, raw) meet every operator, and
the meeting is written out per site instead of once per op family.  It is
also the ONE place where the s509 review's remaining run-time gap plausibly
lives (§7).

## 2. Per-op divergence census (25 probes, perl 5.40.3 the oracle)

Probe files: the fork's scratchpad `probes/*.pl`, results in `probes/results.tsv`.
Every probe was run under `perl` and `./runpcl` and compared byte for byte.

| probe | ops covered | result |
|---|---|---|
| `pow` | `**` on ints, negatives, fractions, `0**0` | SAME |
| `modulo` | `%` with negative, float and bignum operands | SAME |
| `str-incr` | magical `++` on `az Zz a9 9z zz Az9 a Zz9` | SAME |
| `repeat` | `x` with negative, float, string counts; list repeat | SAME |
| `range-str` | `"aa".."ad"`, `"09".."11"`, `"x".."ab"`, an empty range | SAME |
| `num-strings` | `"0x10" "1e3" " 12 " "inf" "nan" "0 but true" "3rd" ".5" "1_000" "+3" "-"` + 0 | SAME |
| `float-print` | `1e15 1e16 0.1+0.2 1/3 1e21 1.0 -0.0 1e-5 1e100 2.5e-7` | SAME |
| `num-eq` | `"10"==10.0`, `"1.0" eq 1.0`, `"abc"==0`, `"10"<"9"`, `"10" lt "9"` | SAME |
| `sort-mixed` | `<=>` over ints, strings, floats, `"1e1"`, `"0x3"` | SAME |
| `sprintf-1`, `sprintf-2` | `%d %s %.2f %g %e %x %o %b %5.1f %-5s %05d %.0f` incl. banker's `%.0f` of 0.5/1.5 and `"12abc"` | SAME |
| `bit-string` | string `| & ^` on non-numeric strings | SAME |
| `nan` | NaN compares, `9**9**9`, `-9**9**9`, Inf/Inf | SAME |
| `incr-strnum` | `++` on `"1.5" "0x1" "1e2" "-3" " 7"` | SAME |
| `concat-num` | `.` with floats, `1e21`, `-0.0` | SAME |
| `hex-oct` | `hex`/`oct` incl. `0b`, `0o`, `1_f`, 64-bit `ff..ff` | SAME |
| `length-num`, `div`, `numify-ws`, `big-mod` | lengths of printed numbers; `1/7`, `1e300*1e10`, `2**1023*2`, `2**-1074`; whitespace numification; truthiness of `"0.0" "00" "0E0"`; `2**63 % 10`, `1e19 % 7` | SAME |
| `cmp-iv-nv` | `18446744073709551615 == 18446744073709551616`, `9007199254740993 == 9007199254740992.0` | **DIFF** -- **#3692** (filed s524): PCL compares exactly where perl converts the IV to NV |
| `iv-overflow` | `-9223372036854775808 - 1`, `18446744073709551615 + 1` | **DIFF** -- the RULED unbounded model (#1513): perl promotes to NV, PCL stays exact |
| `int-abs`, `int-arith` | `int(1e20)`, `4294967296 * 4294967296` | **DIFF** -- the same model (#1515: `int()` of an NV past 2^53; a product past 2^64) |
| `bitops` | `-1 >> 1`, `"12" \| "3"` | **DIFF** -- two faces: `-1 >> 1` = **#3760 (FILED by this review)**: perl reads a shift's left operand as a UV (`pp.c:2164-2208`, `SvUV_nomg`), so `-1 >> 1` is 9223372036854775807 and `-1 << 1` is 18446744073709551614, and an NV above UV_MAX saturates (`2**64 >> 1` = 9223372036854775807); PCL shifts the signed integer, which is perl's `use integer` behaviour.  `"12" \| "3"` = **#1040** (pending): perl 32 (string op, both POK-only), PCL 15 (the `looks-like-number` guess of §1c) |

What the census says: the `p-*` implementations of perl's `pp_*` semantics are
right wherever the representation can express the answer -- string increment,
ranges, repetition, `%`'s two regimes, `**`'s NV rule, `%g` printing, NaN,
whitespace numification, `"0 but true"` all agree.  The five divergences are
one question (what a scalar remembers about itself), and three of the five
are ruled.  #3760 is the cheap one: a two's-complement mapping of a negative
shift operand, one `minusp` test, no 64-bit model needed.

## 3. Dispatch sprawl: the ten largest functions

| function | lines | `cond` | `case` | `typecase` | `if` | `when` | what it dispatches on | perl's shape |
|---|---|---|---|---|---|---|---|---|
| `p-list-=` (MACRO) | 446 | 6 | 0 | 0 | 18 | 8 | the CODE SHAPE of the LHS: scalar-only fast path (≤ 8 scalars), skip markers, arrays/hashes, nested lvalues, `(…) x N` repetition, static vs dynamic offsets | `pp_aassign` (`pp_hot.c`): ONE run-time loop; the common-vars analysis is a compile-time flag (`OPpASSIGN_COMMON_*`) |
| `p-method-call` | 346 | 7 | 0 | 0 | 10 | 37 | invocant kind (tie proxy, typeglob, class string, object), own-method cache, MRO walk, `SUPER::`, `AUTOLOAD`, `can`, fully-qualified names | `pp_method_named` + `gv_fetchmeth_pvn` + the method cache: one resolver, one cache |
| `p-setf` (MACRO) | 340 | 6 | 0 | 0 | 7 | 4 | the CODE SHAPE of the place: sigil (`$ @ %`), `p-gethash`, `p-aref`, deref chains, slices, `local`, aliases -- **16 `(eq (car place) …)` arms + 2 sigil arms** | perl has no analogue: the OP tree already names the store op (`sassign`, `aassign`, `helem`, `aelem` …) |
| `sprintf-one` | 305 | 11 | 5 | 0 | 26 | 5 | the conversion letter (14 arms), flags, width, precision, sign | `sv_vcatpvfn_flags` (`sv.c` ~13,000-13,900): one `switch`, comparable |
| `p-array-fill` | 235 | 3 | 0 | 0 | 4 | 4 | tied source/target, flattening of nested vectors, holes, boxing per element | `pp_aassign`'s array branch + `av_store` |
| `p-sprintf` | 227 | 3 | 0 | 0 | 13 | 16 | the format string scan, vectors flag, argument consumption | the same `sv_vcatpvfn_flags` |
| `%p-clock-resolution` | 210 | 0 | 1 | 0 | 8 | 3 | one `case` over clock ids with a long platform table in its docstring | `pp_sys.c` `Time::HiRes` glue |
| `box-set` | 186 | 2 | 0 | 1 | 8 | 12 | a number/simple-string FAST PATH, then tie STORE, magic set, box-in-box (reference) preservation, class copy, aggregate scalar-context collapse | `sv_setsv_flags` (`sv.c`): ~400 lines, the same concerns |
| `%p-open-impl` | 162 | 1 | 0 | 0 | 4 | 7 | mode string, 2- vs 3-argument open, in-memory handles, dup, pipes | `pp_sys.c` `pp_open` + `do_openn` (`doio.c`) |
| `%p-hash-user-count` | 145 | 0 | 0 | 0 | 5 | 6 | tied hash → SCALAR; the internal class key; the `%ENV`/`%INC` markers | `HvUSEDKEYS` |

Three readings.  First, **three of the ten are MACROS that re-derive the
compiler's knowledge**: `p-setf`, `p-list-=` and (in the same family)
`p-.=`, `p-pre++`/`p-post++`/`--`, `p-scalar-=`, `p-viv-root`,
`p-viv-ensure-arrayref`, `p-sort-cmp`, `%define-compound-pair` -- **12 macros
inspect the code shape of their argument** (`(eq (car place) …)`, `(symbolp
place)`, `symbol-name place`).  `Pl/ExprToCL.pm` emits `(p-setf PLACE V)` for
every store and the runtime macro decides, at CL macro-expansion time, which of
16 store forms it meant.  This is a second lowering layer in the runtime file,
and it is where the parser review's "ONE lowering" endgame (§8 there) has a
runtime half: either the compiler names the store op (as perl's op tree does)
and the macros collapse to one shape each, or the macros are declared the IR's
assembler and `docs/ir-spec.md` says so.  Both are legitimate; today neither
is written down.

Second, the genuinely long RUN-TIME functions (`p-method-call`, `sprintf-one`,
`box-set`) have perl counterparts of the same kind and comparable length;
`p-method-call`'s 37 `when`s against `gv_fetchmeth`'s one resolver and one
cache is the only one where perl is visibly simpler -- its tie-proxy and
typeglob invocant arms and the three name-resolution paths could sit behind
`%pcl-invocant-class` instead of in the caller.

Third, `%p-hash-user-count` at 145 lines for "the key count minus the class
key" is the size smell: the tied path, the `:__class__` key and the
`%ENV`/`%INC` marker hashes each have an arm where perl has `HvUSEDKEYS`.
A rule-10 split candidate, not a design flaw.

## 4. Magic and tie, against `mg.c`

perl: one magic CHAIN per SV, read by `mg_get` at every `SvGETMAGIC` site and
written by `mg_set` at every `SvSETMAGIC` -- several hundred macro sites across
`pp*.c`.  PCL: the box's VALUE is the magic (`p-tie-proxy` for a tied scalar,
`p-magic-cell` with a kind and getter/setter for `:strbuf :memfh :tielem
:lvalue :defelem :signal`), read by ONE function:

```lisp
(defun unbox (val)          ; :~5,9xx -- the ONE reader
  ... ((p-tie-proxy-p v) (unbox (%p-tie-fetch val v)))
      ((p-magic-cell-p v) (funcall (p-magic-cell-getter v))) ...)
```

with `%p-read-operand` beside it so an operator that classified a value does
not FETCH it twice (#1813), and `box-set` as the one writer that routes STORE
and the setter.  For aggregates the entry test is the macro family
`%p-when-tied` / `-kind` / `-hash` / `-array` (:1112-1135; 27 + uses), whose
cost was measured free (`docs/tie-aggregates.md` §2).  Hand tests remain where
the generic reader cannot be used: 19 `p-tie-proxy-p` and 25 `p-magic-cell-p`
sites -- the writers and the places that must NOT fetch (`local`, `defined`,
`exists`, the dualvar copiers).  Verdict: the same shape as perl's, with fewer
sites because reads go through one function.  Nothing to restructure.

## 5. Duplication

`tools/dup-census.pl cl/pcl-runtime.lisp` (5-line shingles): **49 runs, all
SHAPE-only, 0 exact; 347 deletable lines = 0.85 % of the file.**  Families
worth a fold, by deletable lines: the six flipflop operators (`p-flipflop`,
`-3`, `-num`, `-num-3`, `-dyn`, `-dyn-3`: ~45 lines across clusters 2/5/6/7/9/10
-- one operator with two flags), `%p-local-array-elem-init-plain` vs
`%p-local-array-elem-plain` (14), `p-//=` / `p-and-assign` / `p-or-assign`
(10), `p-pre++`/`p-pre--` + `p-post++`/`p-post--` (17),
`%def-overloaded-cmp` vs `%def-overloaded-str-cmp` (21), `p-gmtime` /
`p-localtime` (8), `p-keys` / `p-values` (11).  Cross-checked by reading:
the flipflop six are the same state machine with the `...` and numeric-line
variants spelled out; the local-array-elem pair differs by one `init` step;
the overloaded-cmp pair differs by the fallback comparator.  Rule 11 holds;
the s411-s413 dedup (six families then) did its job.

## 6. Growth: what the +36k lines of 90 days are

`git log --since=90.days --numstat -- cl/pcl-runtime.lisp`:

| | |
|---|---|
| commits | 729 |
| added / removed / net | +36,292 / -9,852 / +26,440 |
| by month | Jul (from the 26th) +897, Aug +7,999, **Sep +19,487**, Oct +7,909 |
| new top-level definitions (`+(def…` lines) | 1,929 |
| added COMMENT lines | 7,492 (21 % of the added lines) |

The twelve largest commits (added lines): tie on arrays and hashes 554 (#155),
`%SIG` store seam and boot dispositions 543 (#2107), filehandles carry octets
unless a layer says otherwise 458 (#1115), boxed aggregates phase 4 440,
derived match variables materialise on read 426, telldir/host/service
databases 409 (#2093), the module cache stores compiled fasls 406 (#1188),
overload's bitwise/nomethod prerequisites 321 (#972), the cache dependency
manifest 311 (#1261), Unicode properties from perl's tables 309 (#2060), the
zlib shim 302 (#3161), File::Glob without XS 290 (#488).  A 40-commit even
sample reads the same way: each is one perl mechanism (`readdir` order,
`&name;` sharing `@_`, `can` returning undef, an operator reading its operand
once, `$0` writable, `qr` as an object, element args aliasing through `@_`).

Perl's own runtime for comparison: `sv.c` 17,581 + `pp.c` 7,944 + `pp_hot.c`
6,918 + `pp_ctl.c` 6,928 + `pp_sys.c` 6,208 + `mg.c` 4,034 + `hv.c` 4,198 +
`pp_pack.c` 3,184 + `numeric.c` 1,998 + `pp_sort.c` 1,486 + `doop.c` 1,298 +
`av.c` 1,265 = 65,768 lines, 204 `pp_` functions behind 422 ops.  PCL's
40,771 lines cover the same surface plus the I/O layer, `sprintf`, the regex
glue and the module cache.  The file is growing because the surface is large
and was being covered one mechanism per batch -- September's +19.5k is the
boxed-aggregates, tie, filehandle-layer and match-variable months -- not
because the same thing is being written twice (§5).  The 21 % comment share is
the project's WHY-discipline and is also why the file reads long.

## 7. Run time

`docs/speed-review-s509.md` profiled thirteen slow bench rows and filed four
levers; **all four are DONE** (#2770 typed string copy, #2771 `print`'s list
path, #2772 scalar `grep`, #2773 `unshift` boxing -- status `completed`).  Its
§3 "not cheap" list stands: `fhread` (a reader redesign), the regex rows (the
ONE-engine ruling), objects' per-object EQUAL hash (#1890), `f(@big)`'s `@_`
aliasing, `pack`.  Not re-profiled here (the brief allowed one `sb-sprof`
run; s509's profiles are a week old and nothing in §1-§6 contradicts them).
What §1d adds: the per-site box-or-raw test is a cost perl does not pay in
this form (its flag test is one bit, in one place per op), and the raw-slot
verdicts remove it exactly where the compiler can prove the shape -- so the
remaining gap on boxed-heavy rows (objects, hashes of boxes) is at least
partly the ~1,000 hand dispatches, which is why R3 in §10 carries a bench bar
rather than a readability one.

## 8. Rules 10 and 12

| rule | measured |
|---|---|
| 10: no function over ~80 lines | **41 of 1,849** over 80; 7 over 200 (`p-list-=` 446, `p-method-call` 346, `p-setf` 340, `sprintf-one` 305, `p-array-fill` 235, `p-sprintf` 227, `%p-clock-resolution` 210); three of the seven are macros |
| 12: a missing case dies, never `(t nil)` | **66 `(t|otherwise nil|0|"")` arms in 59 functions** (grep `^\s*\((t\|otherwise)\s+(nil\|0\|"")\)`); #152's audit (s337c) found 38 sites and left "no value-producing swallow"; the five sampled here (`sprintf-one`'s sign and radix prefixes) are all legitimate empty answers over OPEN sets -- a re-audit of the other 61 is a filler, not a finding |

## 9. What is right and must survive

One coercion path (§1b).  One reader with tie and magic inside it (§4).  The
raw-slot design and its measured verdicts (`docs/boxed-aggregates-design-s455.md`).
Duplication at 0.85 %.  The rule-12 culture (`%p-bitwise-operand-kind`'s
docstring is the archetype: "a state that is none of the three DIES naming
itself -- the arm this replaced answered :NUMBER for every unlisted state").
`docs/ir-spec.md` as the normative manual, matching the code where checked.
The layer rule (shims in `lib/`, not here).  The harness: `tools/bench-exec.pl`,
the everyday battery, the sweep -- all runtime-independent.  And the comments:
a fifth of what was added explains why, which is how a 40k-line file stays
reviewable.

## 10. Verdict, options, recommendation

**Verdict.**  The parser review's verdict was "sane as an artifact, not as a
structure".  The runtime's is "sane as an artifact and, with three named
exceptions, as a structure".  The exceptions: (i) the box-or-raw dispatch is
written per site (~1,000 tests, 5 `typecase`s); (ii) the box lacks perl's
flags, which is RULED for the range faces and a cheap exact fix for the
string-vs-number face; (iii) twelve shape-dispatching macros are a second,
undocumented lowering layer.  None of these is growing the way the parser's
passes grew; the growth here is coverage.

**Options, each with the number that proves it.**

| option | what | size | bar |
|---|---|---|---|
| **R1 -- remember numification exactly** | `%p-bitwise-kind-of` and any sibling that asks `looks-like-number` of a VALUE (not of perl syntax) answer from the representation: a box with `nv-ok` is `:number`, a raw string is `:string`; closes #1040; #3760's negative-shift face lands beside it (`(ldb (byte 64 0) n)` when `use integer` is off) | 1 filler session | perl probes `"12"\|"3"`=32, `$x+0; $x\|"3"`=15, `-1>>1`; bop.t rows out of `fail-baseline` by edit; bench unchanged |
| **R2 -- the IV/UV/NV faces** | a 64-bit model at the operators (#1513's design) | parked by the USER's ruling (2026-09-07) | re-raise only if the cause census shows CPAN rows behind it |
| **R3 -- ONE typed entry per op family** | a macro per family (`define-scalar-op`, `define-container-op`) that performs the box/raw/tie/magic/undef `typecase` ONCE and hands the body a plain value; the ~1,000 hand tests shrink to the families' entries; byte-identical behaviour | 4-6 sessions in the perf slot, one family per session, hand-replaced A/B first | bench rows no slower (the s509 bars), gate + sweep +0, the test count `(p-box-p` 341 → under 100 |
| **R4 -- the shape macros** | now: `docs/ir-spec.md` names the 12 macros as the IR's assembler and lists their arms; later: when the parser program rebuilds lowering (parser review §8), the compiler names the store op and `p-setf` shrinks to one arm | docs now; the rest rides on the parser program | `p-setf` 16 arms → 1 at the end |
| **R5 -- rule-10 splits** | `p-method-call` → resolve + cache + call; `sprintf-one` by conversion class; `p-list-=` by regime; `%p-hash-user-count`; the flipflop six → one; the local-array-elem pair → one | fillers, 2-3 sessions total | corpus-diff IDENTICAL (macros) / bench unchanged (functions); 7 functions over 200 → 0 |
| R6 -- the 66 default arms | re-audit, one line each, DIE where a value flows on | half a session | the count with a verdict per arm |

**Recommendation.**  No restructuring program for the runtime; the USER's
"look at the runtime after" is answered by three named repairs and one
measured refactor.  Order: **R1 + R6 + the flipflop fold in one Opus filler
session** (small, closes a silent-wrong family, bench-neutral); **R3 as the
next perf-slot item** when the perf rotation comes round (it is the one
structural item and the one with a plausible speed return, so it belongs
where a bench bar is routine); **R4's documentation half now**, its code half
with the parser program; **R5 as fillers**; **R2 stays parked**.  None of
this competes with the parser program for a slot, and none of it changes the
emission the parser program will be measured against.

Reading list for whoever executes: `docs/ir-spec.md` §2.2, §2.2b, §3.1-§3.6;
`docs/boxed-aggregates-design-s455.md`; `docs/tie-aggregates.md` §2 and §7;
`docs/speed-review-s509.md` §3-§4; `docs/not-supported.md` "Integers are
unbounded"; perl's `sv.h:414-516`, `pp.c:2106-2210` and `:2527`,
`pp_hot.c:1816-1920`, `sv.c` `sv_setsv_flags`, `mg.c` `mg_get`/`mg_set`.
