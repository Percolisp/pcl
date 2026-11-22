# Symbol: a digit-named variable ends at its last digit  (PCL §75; group BI)

**Perl's rule.** perl reads a digit run after a sigil as the whole name of the variable and stops at the last digit (toke.c `scan_ident`), so `@1x 2` is the array `@1`, the repetition operator and `2`, and `@12x3` is `@12 x 3`; `@1::x` is a syntax error (the `::x` after the term is a bareword).

    perl -e 'no strict; @1 = (5); print @1x 2, "\n"'      # 11
    perl -e 'no strict; @12 = (3); print @12x3, "\n"'     # 111

**Before.** `PPI::Token::Symbol` sucked every word character after the sigil into the token and then classed the digit-led name as Magic: `@1x 2` was one `Magic(@1x)` followed by `Number(2)`, `@12x3` one `Magic(@12x3)` -- the array twin of the `%12x 2` that the `%` arm reads right since the digit-run hash name (group BF).
**After.** The Symbol tokenizer keeps only the digit run of a digit-led `$` / `@` name and hands the rest back to the tokenizer, which reads the `x` after the term as the repetition operator.  `@1 x 2`, `@119797`, `@a1x`, `@_x` and the capture variable `$1x` (Magic.pm's own digit run) are unchanged.

**Rows fixed:** PCL §75 (six rows; eight controls).  TODO tests commit `11038186`, then the fix `7fd02b75`.  Suite PASS plain + AUTHOR_TESTING.
