---
name: project_cpan_pureperl_findings
description: "CPAN pure-Perl survey (2026-06-21): JSON::PP fixes (utf8::unicode_to_native + eval-mode constant subscript), Try::Tiny finally needs DESTROY/scope-guard, remaining JSON::PP number-vs-string needs string bitwise operators."
metadata: 
  node_type: memory
  type: project
  originSessionId: 81cb5bd6-d356-4caa-ba28-50715d6a6382
---

# Pure-Perl CPAN survey findings (2026-06-21)

Running already-installed pure-Perl CPAN modules through `./runpcl` to find PCL bugs.

## FIXED this session
- **`utf8::unicode_to_native` / `utf8::native_to_unicode` were undefined** →
  added as identity functions in `cl/pcl-runtime.lisp` (`:utf8` package). On any
  ASCII/non-EBCDIC platform (all PCL targets) both are identity. JSON::PP builds
  its invalid-char regex with `chr(utf8::unicode_to_native($i))`, so this blocked
  JSON::PP from even loading.
- **`use constant` name as ARRAY subscript inside `eval "string"`** was autoquoted
  to a string index (→ index 0). Fix in `Pl/PExpr.pm` `_bareword_subscript_autoquotes`:
  in `eval_mode`, an ALL-CAPS bareword in an array subscript stays callable (Perl
  only autoquotes HASH subscripts; the constant sub exists at runtime). This was the
  root cause of JSON::PP `->canonical` wrongly enabling ascii `\u`-escaping
  (`canonical` writes `PROPS[P_CANONICAL]` but `P_CANONICAL` resolved to 0 = the
  ascii slot). General win for all eval-heavy modules, not just JSON::PP.
  Regression test: `Pl/t/eval-constant-01.t`.

## STILL OPEN (deeper, documented)
- **JSON::PP encodes numbers as JSON strings** (`{"age":"30"}` vs `30`). Root cause:
  JSON::PP `_looks_like_number` (non-B path) (1) `return if utf8::is_utf8($value)`
  then (2) the dualvar trick `length(("") & $value)`. PCL fails BOTH:
  - `utf8::is_utf8` stub returns 1 ALWAYS → short-circuits everything to "not a number"
    → all values stringified. **NOTE (user corrected me 2026-06-21):** the `SvUTF8`
    flag is ORTHOGONAL to number-ness — a scalar CAN be utf8-flagged AND numeric
    (`my $x="123"; utf8::upgrade($x); $x+0==123` with is_utf8==1). JSON::PP's
    `return if is_utf8` is a *conservative heuristic* ("almost certainly started as a
    string"), NOT a correctness test. There is NO content predicate that reproduces
    SvUTF8 (it's representation/provenance, not chars; upgraded ASCII has it on,
    latin1 wide can have it off). PCL has NO SvUTF8 concept at all. So the right fix
    is `is_utf8` → **return false** (don't let it discriminate), NOT content-based.
  - **CORRECTION (2026-06-21): PCL's string bitwise operators ALREADY EXIST** — both
    auto-dispatching `& | ^ ~` (`p-bit-and`/`-or`/`-xor`/`-not`, ~line 4650
    pcl-runtime.lisp) AND explicit dotted `&. |. ^. ~.` (`p-str-bit-*`), with a
    correct byte-wise engine `p-string-bit-op` (truncate for `&`, NUL-pad for `|`/`^`).
    `"abc" & "abd"` works. The BUG is the DISCRIMINATOR `p-string-bitwise-operand-p`:
    `(and (stringp val) (not (looks-like-number val)))` — it routes by string CONTENT,
    so a numeric-LOOKING string `"30"`/`"12"` is sent to the NUMERIC branch
    (`"12" & "3"` → PCL `'0'` vs Perl `'1'`; `length("" & "30")` → PCL 1 vs Perl 0).
    Perl routes by SV FLAG (POK vs IOK/NOK), not content. p-box already encodes flavor
    in the CL TYPE of its `value` slot (`30`=int, `"30"`=string), so the Perl-faithful
    discriminator is ~`(stringp (p-box-value v))` — i.e. DROP the `looks-like-number`
    clause. WHY it's content-based today: pragmatic proxy because PCL doesn't yet
    GUARANTEE morally-numeric values flow as CL numbers in the box (+ lazy `sv` cache
    blurs flavor after stringify, like SvPOK-after-stringify in Perl). Flipping to
    type-based risks regressing number-arrived-as-string cases → entangled with the
    box/flavor invariant, belongs with `docs/type-flow-and-codegen-plan.md`, not a
    one-liner. Pair with `is_utf8`→false. utf8 is UNRELATED to the operators existing.
  OO JSON::PP otherwise works (canonical, pretty, decode round-trip, \1→true).
- **`decode_json`/`encode_json` (function forms)** call `->utf8` which pulls in
  `Encode` (XS) → `Encode::onBOOT` undefined. XS, out of scope. OO interface is the
  workaround.

## Try::Tiny — `finally` needs deterministic DESTROY (user: "maybe shim Try::Tiny")
- try/catch WORK. `finally` does NOT run: Try::Tiny implements `finally` via
  `Try::Tiny::ScopeGuard` objects whose **`DESTROY`** fires the block at scope exit
  (the `local $_finally_guards` goes out of scope when `try` returns). PCL never calls
  DESTROY (documented limitation, `docs/not-supported.md` "DESTROY by GC"). This is
  deterministic refcount-based destruction, not async GC.
- **User asked to note: we should maybe SHIM Try::Tiny** (`lib/Try/Tiny.pm`) — a
  PCL-friendly reimplementation where `finally` runs the block directly after
  try/catch instead of via a scope-guard DESTROY. Same layer as other `lib/*.pm`
  shims (CLAUDE.md principle 9a). Not yet done.

Related: [[project_case_sensitivity_general_fix]] (also validated CPAN under :invert),
[[project_cpan_convergence_survey]], [[project_cpan_module_survey]].
