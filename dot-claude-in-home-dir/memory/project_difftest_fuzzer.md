---
name: project_difftest_fuzzer
description: "Differential fuzzer (tools/difftest-ops.pl) — proactive PCL-vs-perl coverage; how to run, extend, and what it found"
metadata: 
  node_type: memory
  type: project
  originSessionId: eeab055f-63ef-4cda-95e9-edd8799a488c
---

**WRITEUP: `docs/difftest-fuzzer.md`** (session 242) — how-to-run/how-it-works/axis
table/bug ledger. **Axes 9–14 added s242; axes 15–21 added s244.** Now **21 axes,
931 snippets**.

**Axes 15–21 (session 244, commits `9fad8d2` + `bee3791`):** 15 closures/lexical
capture, 16 local/dynamic scope, 17 sort variants, 18 regex features, 19 autoviv/
nested data, 20 numeric stringification & sprintf precision, 21 short-circuit/
defined-or side-effect ordering. **2 NEW BUGS FOUND+FIXED:** (a) **closure-captured
`my @a`/`my %h` never populated** (axis 15) — captured aggregate renamed to a
let-bound lexical, init went through `p-my-=` (box-set = no-op on a non-box array/
hash) → whole-aggregate reads saw empty; fix = shared `p-array-fill`/`p-hash-fill`
(fill the adjustable lexical in place, no proclaim-special) + LIST-ctx RHS. (b)
**`%.Nf` rounded half-AWAY-from-zero** (axis 20) — `sprintf("%.0f",2.5)`→3,
`("%.0f",0.5)`→1; C/Perl round half-to-EVEN; fix = round the EXACT rational of the
double with CL `ROUND` (itself half-even). Lifted sprintf.t +54 / sprintf2.t +42.
**reduce/pair* layering lesson:** initial fix put List::Util block-var convention in
the parser; user flagged it → moved to the shim (caller `$a`/`$b` by symbolic ref).
See [[feedback_fix_at_right_layer]]. **DOCUMENTED-not-fixed:** `%.17g` ~15 sig-digits
(0.1 not 0.10000000000000001), `2**53` prints bignum not float — same representation
call as the deferred `**`.

**Earlier (session 242), 821 snippets, 818 match.** Commit `caf2c52` (axes 9-11) +
`60d8149` (axes 12-14). Remaining documented-deferred trio: length-plus named-unary,
()=split arity, 2**3**4 bigint.

**Axes 12–14 bugs (commit `60d8149`, gate 92/3330):** (1) **`/=`/`**=` leaked a CL
ratio** — `$x/=2`→"7/2", `$x**=-1`→"1/2"; macros computed raw (int/int→ratio) while
`p-/`/`p-**` coerce→float. Delegate `p-/=`→`p-/`, `p-**=`→`p-**`. (2) **slice in
string interp leaked scalar ctx** — `my $s="@a[1..2]"`→last elem not "2 3": single-
slice string bypassed join wrapper (`StringInterpolation.pm` ~152) + wrapped slices
inherited outer ctx (`gen_string_concat`); force LIST. (3) **anon arrayref `[...]`
leaked scalar ctx + tail_position** — `do{...;"@{[reverse @a]}"}`→"321" (reverse ran
scalar, reversed the joined string) not "3 2 1"; `gen_array_init` now forces LIST +
clears tail_position (bracket contents never the tail call). sort/map unaffected;
reverse exposed it. **UB note:** `$x += $x += 1` (perl 4, PCL 3) is UNDEFINED per
perlop ("modifying a variable twice in the same statement") → removed from axis, NOT
a bug (principle 9). Regression tests in `Pl/t/misc-fixes-02.t`.

**Axes 9–11 added session 242** (string builtins+sprintf, regex+tr,
numeric edge cases): 761 snippets, 757 match. **NEW BUG FOUND+FIXED — float
stringification** (`0.1+0.2` → PCL `0.30000000000000004`, perl `0.3`): PCL emitted
SBCL's shortest-round-trip form; Perl uses plain `%.15g` (15 sig digits then strip
trailing zeros — lossy by design). Fix in `stringify-value` (`cl/pcl-runtime.lisp`
~1204): fixed-notation branch now uses `(14 - exp10)` fraction digits; exponential
branch uses `~,14E` (rounds + bumps exponent, `9.999999999999999e15`→`1e+16`). sprintf
`%g/%e/%f` is a SEPARATE path (`sprintf-format-float-*` ~2247) already correct — not
touched. Regression test in `Pl/t/misc-fixes-02.t`. Remaining 3 mismatches all
documented/deferred (named-unary prec, `**` bigint, `()=split`).

**Built session 240 (2026-06-09), commit `5a61ef4`.** A *proactive* coverage tool —
differential fuzzing of PCL against real `perl` as the oracle. Motivated by the
ternary bug surviving 5 months because `perl-tests/cond.t` (17 lines) never tested
`?:` at all. Instead of finding bugs reactively (one per CPAN-module crash), this
*generates* small snippets over enumerable language axes and diffs PCL vs perl.
The user's framing: "this is how security people find holes" — differential fuzzing
against a reference implementation.

## The tool: `tools/difftest-ops.pl`
```
perl tools/difftest-ops.pl [--jobs N] [--limit N] [--show-ok]
```
- Generates templated snippets (NOT random) over axes:
  1. binary operator precedence PAIRS `2 OP1 3 OP2 4` (shifts excluded — large-shift
     is documented non-support);
  2. string/relational ops with mixed numeric-string operands;
  3. ternary nesting/associativity shapes (true-nest, false-nest, chains, deep,
     binop-in-branch) × all condition truth assignments;
  4. named-unary combined w/ binary/ternary (`ref $h eq "X" ? .. : ..`, etc.).
- Each snippet ends `print "[VALUE]\n"` (undef → `[undef]`) for robust extraction.
- Oracle = `perl FILE`; if perl rejects it (`$?!=0`) the snippet is SKIPPED (we are
  not a Perl validator — principle 9).
- PCL via `./runpcl` in a fork pool (parallel-safe; runpcl uses `$$` temp names).
- Report CLUSTERS mismatches by root-cause signature (so floods collapse): "false
  comparison '' vs undef", "float (**) vs exact bigint", "PCL parse error",
  "PCL runtime error", "numeric format", "other".
- ~445 valid snippets, runs in ~1-2 min at `--jobs 8`.

## SESSION 241 — fixed 4 real bugs + the false-cmp design issue. 351→437 match.
After fixes the fuzzer reports **437/445 match, 8 mismatch / 3 clusters**:
- `2 ** 3 ** 4` float-vs-bigint (representation, DEFERRED — see below);
- `length-plus` = named-unary precedence bug (REAL, DEFERRED — see below).
What got fixed (all in `cl/pcl-runtime.lisp` unless noted; regression tests in
`Pl/t/misc-fixes-02.t`):
1. **[10] chained string-cmp crash** — `cmp-op-to-fn` mapped `eq`→`p-eq` (only the
   numeric `p-==` family exists); string ops are `p-str-*`. Added the eq/ne/lt/gt/
   le/ge/cmp→`p-str-*` mapping.
2. **[9] bitwise `& | ^` signed→unsigned** — new `%pcl-to-u64` masks each integer
   operand to `#xFFFFFFFFFFFFFFFF` before logand/logior/logxor (Perl treats them
   unsigned-64-bit). `p-bit-not` already masked.
3. **[2] relational vs equality precedence** — `Pl/PExpr.pm` chained-cmp left-scan
   now requires `prev_info->{prec} == op_info->{prec}`, so `2 != 3 > 4` no longer
   cross-tier-chains; parses as `2 != (3 > 4)`. (Table was already correct: rel=40,
   eq=30.)
4. **[1] false comparison returns `\"\"` not nil** — new `p-bool` (`(if x 1 \"\")`);
   wrapped the `%def-overloaded-cmp` and `%def-overloaded-str-cmp` macro bodies in
   it. Perl false-cmp is `\"\"` (DEFINED), so `defined(2==3)` true and `2==3 // 4`
   = `\"\"`. SAFE: every consumer (p-if/while/unless, &&, ||, //, chain-cmp, the 5
   internal p-== uses, all pcl-pack.lisp p-str-eq uses) routes through p-true-p /
   %pcl-definedp. Fixed clusters [1]+[3]+the [2] remainder (~76 rows).
5. **bitwise both-string detection** — exposed by #4: `2 & (3==4)` = `2 & \"\"` wrongly
   went string-bitwise because `p-bit-*` used `(or str-a str-b)`. Perl string-bitwise
   needs BOTH operands strings → changed to `(and …)`. `2 & \"\"` now numeric → 0.

DEFERRED (user decision, session 241):
- **`**` float-vs-bigint** — Perl `**` ALWAYS returns NV; PCL returns exact bignum.
  Only differs for results > 2^53. Pure representation choice, real regression risk
  (2**N as int size/bitmask), no CPAN module needs the float imprecision → documented
  difference in `docs/sweep-bug-catalog.md`, NOT fixed.
- **named-unary precedence** (`length $s + 1`) — Perl named-unary is LOWER prec than
  `+ - .` so it's `length($s+1)`=1; PCL gives `length($s)+1`=5. REAL bug but involved
  (named unary is parsed by a special 'consume one term' mechanism in `Pl/PExpr.pm`
  ~line 2784, not the precedence table). Fix-target, deferred. In sweep-bug-catalog.
- **generated-CL whitespace** — `(p-if                       (cond)` big gap: condition
  sub-exprs carry a leading `indent_str x indent_level` prefix when inlined into
  `(p-if …)` (tail-if transform / statement-path render). Cosmetic codegen TODO.

## FIRST FULL RUN (session 240): 445 valid snippets, 351 match, 94 MISMATCH / 6 clusters.
TRIAGE (now mostly DONE in 241 — see above):

- **[69] false comparison: `''` vs `undef`** — `2==4`/`2<1`/`2 eq 4` → perl `[]`,
  PCL `[undef]`. TRUE returns `1` in both. **DESIGN ISSUE, not a trivial flip:** PCL
  comparison ops return CL `nil` for false so generated `(if (p-== a b) ..)` works
  (in CL only `nil` is false — `""` is TRUE). Returning `""` would break every
  bare-`if` consumer unless wrapped in `p-true-p`. DISCUSS with user before touching.
  NOT just cosmetic — see cluster [3] below, it changes `//` results.
- **[10] PCL runtime error** — `'2' eq '10' eq '3'` (chained string compare) →
  perl `[]`, PCL `[CL-ERROR]`. ROOT: `'2' eq '10'` returns undef (cluster [1]), then
  `undef eq '3'` CRASHES p-string-eq (nil operand). **REAL BUG** — string ops must
  coerce an undef operand to `""`. (A localized fix here is safe even if [1] stays.)
- **[9] bitwise ops signed vs unsigned** — `2 - 3 | 4` → perl `18446744073709551615`
  (= 2**64-1), PCL `-1`. **REAL BUG:** Perl's `& | ^ ~` treat operands as UNSIGNED
  64-bit; PCL does signed. Fix: mask `&|^~` results to unsigned 64-bit. ~9 rows.
- **[3] `2 == 3 // 4`** → perl `[]`, PCL `[4]`. DOWNSTREAM of [1]: `2==3` is undef in
  PCL so `undef // 4` = 4; in perl `2==3` is `""` (defined) so `"" // 4` = `""`. Shows
  [1] has real semantic consequences beyond `defined`. Fixing [1]/[2] fixes this.
- **[2] `2 != 3 > 4`** → perl `[1]`, PCL `[undef]`. **REAL PRECEDENCE BUG:** perl gives
  relational `< > <= >=` (prec tier "named higher") TIGHTER than equality `== != <=>
  eq ne cmp`, so `2 != (3 > 4)`. PCL groups them at the SAME precedence L-to-R →
  `(2 != 3) > 4`. Fix: split the two tiers in `Pl/PExpr/Config.pm` precedences. 2 rows.
- **[1] `**` float vs exact bigint** — `2**3**4` → perl `2.41785163922926e+24`, PCL
  `2417851639229258349412352`. Perl's `**` ALWAYS yields an NV (float). Representation
  choice; lower impact.

**Net: 3 genuinely fixable bugs found in ONE run** — string-eq-on-undef crash [10],
bitwise unsigned [9], equality-vs-relational precedence [2]. Plus 2 design/representation
issues [1-false-cmp],[**-float] (discuss first), and [3] is downstream of those.
This validates the proactive approach: 3 real bugs the whole `perl-tests/` suite missed.

## How to use the findings
Each "other"/"PCL parse error"/"PCL runtime error" cluster = candidate REAL bug →
triage → fix → drop the snippet into a `Pl/t/` regression test. The false-cmp and
**-float clusters are design/representation — discuss before acting; add a skip-list
so they stop flooding the operator-pair axis (the ternary/named-unary axes were CLEAN
this run — 0 mismatches, confirming session 240's ternary fix held).

## CONTEXT AXIS ADDED (session 241, user picked it). 493 snippets, 490 match.
Axis 5 in `tools/difftest-ops.pl`: each context-sensitive expr (`@a`, `reverse`,
`map`/`grep`/`sort`, `split`, list-literal, slices, `m//` captures, `unpack`,
`x`, `wantarray`) run in **list** (`my @r=(EXPR)`), **count** (goatse
`my $n=()=(EXPR)`), and where well-defined **scalar** (`scalar(EXPR)`); compared
to perl. Found **1 real (niche) bug**:
- **`() = split` LHS-arity LIMIT optimization** — `my $n=()=split/,/,"a,b,c"` →
  perl `1`, PCL `3`. Perl passes (LHS-lvalue-count + 1) as split's implicit LIMIT
  when split is the direct RHS of a list assignment; `()`=0 lvalues → LIMIT 1 →
  whole string = 1 field. PCL always splits fully. **DOCUMENTED, not fixed** (user:
  "just document for now") — `docs/not-supported.md` §split-LHS-arity. ALL common
  cases match (`my($a,$b)=split`, `my @a=split`, `scalar(my @t=split)`); only the
  `()=split` field-COUNT idiom diverges (real code uses `scalar(@parts)`). Fixing
  needs context-dependent split codegen (thread assignment arity as LIMIT).
- After 3 deterministic runs the axis is stable; remaining mismatches: this +
  the 2 deferred (`length-plus` named-unary, `2**3**4` bigint).

## ALL AXES BUILT (session 241, commit `0378336`). 622 snippets, 617 match.
Added Axis 6 (builtins×call-forms: `NAME(a)`/`NAME a`/`CORE::NAME(a)`/`CORE::NAME a`/
no-arg `$_`-default over length/uc/lc/ord/chr/hex/oct/abs/int/sqrt/quotemeta/ref/
defined — ALL match), Axis 7 (deref/sigil/slices/postfix-deref), Axis 8 (OO:
`->m`/`->$m`/`Class->m`/SUPER::/can/isa/ref/chained/own-vs-inherited — ALL match).
Harness robustness: `extract()` normalizes ref hex addrs (`ARRAY(0x..)`→`0xADDR`);
dropped feature-gated `fc` (bare `fc` = string "fc" w/o `use feature`).

**REAL BUG FOUND (deref axis) — FIXED commit `ffcc8b3`** (`docs/sweep-bug-catalog.md`):
braced block-deref + subscript `${$ar}[1]` → undef, `@{$ar}[0,2]` → empty, while
`$$ar[1]`, `@$ar[0,2]`, `$ar->[1]`, `$ar->@[0,2]`, `${$hr}{a}` ALL worked. **Root
cause: PPI mis-tokenizes the `[...]` after `${BLOCK}`/`@{BLOCK}` as a
`PPI::Structure::Constructor` (anon-array) not a `Subscript` — because it follows a
Block `}` not a Symbol (the hash `{a}` IS a Subscript → why hash worked). The
Cast+Block+Constructor triple matched no PExpr case → "Missing case" die → silent
`(progn ;; …)` = undef (never crashes → why the common idiom survived).** FIX: new
pass `_retag_braced_deref_subscript` (`Pl/PExpr.pm`) re-blesses the Constructor →
Subscript for `$`/`@` Cast+Block (NOT `%`/`*` — dedicated handlers). Gate 92/3326,
fuzzer 619/622. Test in `Pl/t/misc-fixes-02.t`.

Current documented/deferred mismatch set (3, all reviewed): `()=split` (niche,
not-supported.md), named-unary precedence (fix-target, Pl/PExpr.pm ~2784), `**`
float-vs-bigint (representation). OO + builtins + deref axes are clean.

## NEXT extensions (planned, not built)
- in-sub vs top-level builtin forms (`@_` defaulting differences — only top-level
  tested so far); context × builtins cross.
- Skip-list keyed to `docs/not-supported.md` so the tool stops re-reporting known
  non-support (eval-lexical-capture, DESTROY/GC, SV identity, large shifts) and the
  5 documented divergences above.

See [[project_cpan_debugging_horizon]] (this is the "proactive coverage pass" option),
[[project_cpan_module_survey]].
