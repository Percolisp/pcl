# Pl::InterpScan — the shared variable-reference event scanner (#237)

**Status (s382f, Opus): CONSUMER 1 OF 3 IS WIRED — the regex-pattern
interpolator, which IS #237.  `ExprToCL::_gen_interp_regex_pattern`'s private
walk is DELETED (the ruled acceptance bar) and both it and the
`_has_regex_interpolation` gate now go through `Pl::InterpScan::scan(…,
in_regex => 1)`; guard rows in `Pl/t/regex-interp-01.t` (28 of them re-derived
from live perl at test time + 5 emission shapes).  Consumers 2 (rename
machinery) and 3 (StringInterpolation) are NOT wired — wiring plan below.**

*(s382, Fable: the scanner core + intuit_more classifier + probe table —
the ruled Fable half of #237, `docs/fable-answers-s378.md` §3 = b′, split
USER s379c.)*

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

1. **Regex — DONE s382f.**  `_gen_interp_regex_pattern` is a `scan($pattern,
   in_regex => 1)` consumer: literal text between the events is copied
   VERBATIM (regex escapes must reach cl-ppcre unprocessed — the one way
   pattern text differs from dq text), and each event becomes a CLForm.
   *How each event lowers* was the design choice: rather than a second
   lowering table (`$x[i]` → p-aref, `$h{k}` → p-gethash, `@a` → join, `$#a`
   → …, `$$r` → …, `${EXPR}` → …) drifting beside the dq/code one, the
   consumer **compiles the reference's own source text as code** through the
   ordinary expression pipeline (`_compile_ref_text_form`, the move
   `_gen_interp_replacement` and `StringInterpolation::_parse_postfix_deref`
   already make), and joins `@`-sigil results with `$"` exactly as
   `gen_string_concat` does.  One shape is fast-pathed to its atom without a
   sub-parse — a plain unqualified non-magic scalar with no chain — which is
   both the common case and byte-identical to the old walk.  A reference the
   pipeline cannot read DIES (rule 12: it would otherwise become literal
   pattern text, a wrong value the match consumes); zero events on the
   corpus, the perl-tests sweep, or the Pl/t gate.
   `_has_regex_interpolation` became the same scan (behind a `[\$\@]`
   pre-filter), because a narrower gate silently un-did the consumer: the old
   predicate matched only `$name`/`@name`/`${`, so `/$1/`, `/$#a/`, `/$$/`,
   `/\Q$^O\E/` and the punctuation magics stayed literal where perl
   interpolates.  The s/// REPLACEMENT keeps the legacy predicate — it is dq
   text, not a pattern, and a bare `$1$2` replacement is better served by the
   runtime's native backref substitution than by a per-match lambda; widening
   that gate is its own measured change.
   Corpus reach (`tools/corpus-diff.pl`): 2 of 111 files — `qr.t`
   (`s/${qr||}/`, now the expr-form deref perl reads) and `sprintf.t`
   (`\Q$^O\E`).  `lib/**` + `cl/pack-impl.pl` emission byte-identical, so the
   checked-in artifacts did not need regenerating.  Compile time 91.4 s vs
   94.5 s at HEAD over the corpus (no regression).
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

## Residue after consumer 1 (s382f)

- **`postderef_qq` is off in pattern mode** (`_gen_interp_regex_pattern` does
  not pass it): the regex path never supported `->@*` and still does not, so
  `/$r->@*/` leaves the arrow literal exactly as before.  The dq side computes
  it lexically from the token (`_postderef_qq_active_for`); the pattern side
  would need the NODE, not just the pattern text, to do the same.  Unchanged
  behavior, recorded so it is a decision and not an oversight.
- **`${^TAINT}` reads undef, perl reads 0** — a pre-existing *runtime value*
  divergence (visible in plain code and dq text too), newly visible in
  patterns because the caret-brace magic now interpolates there.  Task #293.
- Divergences 3–11 of the table above are the dq side's, untouched by this
  consumer; they belong to wiring step 3.

## Task state

#237 remains in_progress: the Fable half and consumer 1 (the #237 cause) are
done, consumers 2–3 are not.  #286 (intuit_curly) is NOT folded in (ruled) —
the classifier lives at module level precisely so a future #286 can sit
beside it.
