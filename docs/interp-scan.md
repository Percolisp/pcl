# Pl::InterpScan — the shared variable-reference event scanner (#237)

**Status (s382, Fable): the scanner core + intuit_more classifier + probe
table are SHIPPED and verified against live perl.  NO consumer is wired yet —
this is the ruled Fable half of #237 (`docs/fable-answers-s378.md` §3 = b′,
split USER s379c).  The Opus half wires the consumers and the guard rows,
using this file as the contract.**

Design lineage: `docs/var-handling-review-s379.md` §4 (direction B — one
interpolation scanner; 26 hand-rolled scanning sites, this module is the one
copy of the answer).  Standing rule §8 applies: new interpolation behavior is
implemented in `Pl/InterpScan.pm` or not at all.

## What shipped

- `Pl/InterpScan.pm` — `scan($text, %opt)` (whole text → event list),
  `scan_one($text, $pos, %opt)` (one reference, for callers with their own
  outer loop), `intuit_more($text, $pos, %opt)` and `regcurly($text, $pos)` —
  line-faithful ports of perl 5.40.3's `S_intuit_more` (toke.c) and
  `Perl_regcurly` (regcomp.c), read from the running perl's own source.
  Event schema is documented in the module header (sigil/form/name/canon/
  span/name_span/expr_span/chain/slice/postderef).
- `Pl/t/interp-scan-01.t` — 180 assertions, three layers over one row table:
  the PROBE TABLE re-derives every recorded verdict from the running perl at
  test time (a perl drift fails loudly), the classifier must match each
  probed verdict, and the scanner's event shapes are pinned (spans exact —
  they are the rename machinery's splice targets).  47 ms.

## The probed facts (the #237 heart)

All taken live on perl 5.40.3 (s382) and frozen into the test:

| shape in a pattern | verdict |
|---|---|
| `$x[0]` `$x[42]` `$x[89]` `$x[-1]` `$x[$i]` `$x[@foo]` | subscript |
| `$x[abc]` `$x[^a]` `$x[a-z]` `$x[\w]` `$x[\n]` `$x[-12]` | charclass |
| `$x[100]` `$x[123]` | charclass (only 1–2 digit guts are subscripts) |
| `$x{k}` `$x{'k'}` `$x{$i}` `$x{-3}` `$x{}` `$x{2x}` | subscript |
| `$x{2}` `$x{2,}` `$x{2,3}` `$x{,3}` `$x{ 2, 3 }` | quantifier (regcurly) |
| `$ar->[anything]` `$hr->{anything}` | subscript, unconditionally |
| `$m[0][abc]`, `$h2{k}{2,3}` | continuation groups ALWAYS bind — only the FIRST group is classified |
| `@x[1]` / `@x{k}` vs `@x[abc]` | slices classify like elements |
| `${x}[0]` `${x}{k}` `${ar}->@*` | braces CLOSE the reference (both modes) |
| `$x[\n@foo]` vs `$x[\n@main::zqx]` | the weigher's symbol-table hook is real: @foo known → subscript, unknown → charclass |
| `a$\|b` `(a$)` `a@-` `a@+` | literal ($ anchor rule; @+/@- never interpolate in patterns) |
| `$#x` `$$sref` `a$$` `a$"b` `$+[0]` `$+{L}` `\c$x` | all interpolate in patterns (\c does not hide a following $) |
| `$x[abc$]` | charclass AND `$]` interpolates, eating the `]` — pattern breaks |

**Correction to DECIDED s378b**: its parenthetical guessed "`$x{2}` …
perl picks the subscript".  Probed: `{2}` is a QUANTIFIER.  The passing
shapes to preserve are exactly `/$x[abc]/`, `/$x[^a]/`, `/$x{2,3}/` (plus
`{2}`-style), and the failing side is `/$a[0]/`, `/$h{k}/`, `qr/\G$_[1]/`.

`known_name` (the `gv_fetchpvn_flags` stand-in) means "is NAME a package
GLOB already known" — subs count, lexicals do NOT.  Default `sub {0}` leans
charclass by at most 90 weight; the verdict only flips on charclass-heavy
guts (the `[\n@foo]` family).  Wiring may pass an Environment-backed oracle;
`sub {0}` is an acceptable v1 with that flip family understood.

## Divergence table — scanner (= perl) vs today's consumers

The scanner follows PERL.  Each entry names a spot where an existing
consumer is narrower or wrong; the wiring session handles each by
probe-and-guard (fix) or by clamping to the event subset it consumes today.

1. **`_gen_interp_regex_pattern` (ExprToCL) — the #237 cause.**  Handles
   only `${name}`, `$name`, `->[i]`/`->{k}` chains.  Direct subscripts
   (`$_[1]`, `$a[0]`, `$h{k}`) leak into the pattern as literal text;
   `@arr`/`@{...}`/`$#x`/`$$r`/punct-magic never interpolate at all (though
   `_has_regex_interpolation` fires on `@arr`, so the array SPELLING lands
   in the pattern).  Deleting this walk in favor of
   `scan($pat, in_regex => 1, ...)` is the ruled acceptance criterion.
2. **`_interp_fixer`'s `${x}[` arms have the WRONG sigil family**
   (pre-existing, found by the s382 probes): its `@`-family arm rewrites
   `${x}[` and its `$`-family arm refuses it — but `"${m}[0]"` under strict
   dies "Global symbol $m": braces close the reference, the SCALAR is read,
   `[0]` is literal.  A rename of `$x` must rewrite `${x}[`; a rename of
   `@x` must NOT.  Fixed for free when the fixer becomes a name_span splice.
3. **`$$r[0]` / `@$r[0]` / `$#$r`** — perl chains/derefs
   (`$r->[0]` / slice); StringInterpolation stops after the name, leaving
   `[0]` literal.  Probed: dq `"$$ar[0]"` prints the element.
4. **`@-[0]` / `@+[0]` in dq text** — perl slices; StringInterpolation
   falls through to literal.  Probed: `"a@-[1]b"` prints the offset.
5. **`${ x }` (blanks around a braced identifier)** — perl reads `$x`;
   StringInterpolation takes its expression-deref path.
6. **`$12abc`** — perl reads `${12} . "abc"` (digit names are digits-only);
   StringInterpolation's `\w+` grabs `$12abc` as one name.
7. **`$::a::b`** — perl reads the full `$main::a::b`;
   StringInterpolation's `$::` branch stops after one segment.
8. **`$Foo::.`** — a dangling `::` is swallowed into the reference
   (probed: `"a$Foo::.b"` prints "a.b").  Scanner: span includes the `::`,
   `name`/`name_span` do not.
9. **`$$$x`** — perl reads nested deref `${${$x}}`; scanner (and
   StringInterpolation today) read pid + `$x`.  Recorded, not fixed.
10. **`@x[1][0]`** — a bracket after a slice is a perl COMPILE error
    ("syntax error near ]["); scanner takes the slice and leaves `[0]`
    unconsumed.  No legal program is affected.
11. **Bracket-guts balance counting ignores quoting** (`$h{'}'}`
    miscounts) — StringInterpolation's rule, kept byte-for-byte.
12. **Regex-mode non-goals (v1)**: `(?#...)` comments, `//x` `#`-comments,
    `(?{...})` code blocks and charclass state are scanned as plain text
    (events inside them are still emitted); `@'foo` / `$pkg'var` (perl-4
    separator) unsupported.  Postderef `%`-forms never interpolate
    (matches perl).

## Wiring plan (the Opus half)

Recommended order — regex consumer first (it IS #237 and carries the ruled
guard rows), then the rename machinery, then StringInterpolation:

1. **Regex**: rebuild `_gen_interp_regex_pattern` as a consumer of
   `scan($pattern, in_regex => 1)`: literal text between events, event →
   CLForm ($var / p-aref / p-gethash / join for arrays / …).  Chain guts
   come as spans — re-parse non-literal guts the way the dq side does.
   Mandatory guards in the SAME commit (ruling §3): `/$x[abc]/`,
   `/$x[^a]/`, `/$x{2,3}/` keep passing; `qr/\G$_[1]/`, `/$a[0]/`,
   `/$h{k}/` start passing; one dq-string row proving strings did not move.
   Any NEW event kind the consumer starts honoring (`@arr`, `$#x`, …) gets
   its own guard row.  Then delete the private walk — the acceptance bar.
2. **Rename machinery**: `_interp_canon`/`_interp_names` become filters
   over event `canon`/`name`; `_interp_fixer`'s ten substitution regexes
   become `name_span` splices (killing the backslash-parity/`(?!::)` bug
   class and fixing divergence 2); then `%canon_pat` in
   `_block_captures_name`, `ExprToCL2::_string_literal_form`, and
   VarAnnotator's three quote scans.  Gate per port: corpus-diff
   byte-identical + Pl/t gate (+ sweep when `lib/` shims are reachable).
3. **StringInterpolation**: replace `parse_interpolated_variable`'s
   low-level scanning with `scan_one` events, keeping node building and the
   case-mod/literal outer loop.  Divergences 3–7 become behavior changes —
   each needs its probe row (they exist above) and a guard; corpus-diff
   flags any reachable movement.

Cache note for every wiring commit that changes emission: bump
`*pcl-cache-generation*`.

## Task state

#237 remains in_progress with the Fable half done; #286 (intuit_curly) is
NOT folded in (ruled) — the classifier lives at module level precisely so a
future #286 can sit beside it.
