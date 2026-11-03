# Tokenizer: a quote-like word naming a braced element is a word  (PCL §61; group AT)

**Perl's rule.** perl reads an identifier straight after the `{` of a sigil (toke.c `S_scan_ident`), and a `[` or `{` after it (white space allowed) makes the block that element, whatever the identifier is -- also the name of a quote-like operator.  Only `sub` before a `{` is excluded.

    perl -e 'our %m = (q => "r"); our @y = (5, 6); print ${m{q}}, ${y[1]}, "\n"'   # r6

**Before.** The Word tokenizer saw `m`, `s`, `y`, `tr`, `q`, `qq`, `qw` ... followed by an opening bracket and started a quote-like token: `${m{q}}` held a match, `${y[1]}` a transliteration whose body ran on past the closing brace, and `${s{x}} . ${y[0]}` swallowed the rest of the statement into a substitution.
**After.** `__current_token_is_forced_word` forces the word when it follows the `{` of a sigil cast (`$ @ % & * $#`) and a `[` or `{` follows it, beside its existing rule for a word before `}`.  A quote-like that does not come first in the block (`${\ m{q}}`), `m{q}` and `s{x}{y}` as statements and a quote-like word as a hash key are unchanged (controls).

**Rows fixed:** PCL §61 (nine rows; four controls).  TODO tests commit `791606a8`, then the fix `a800b8cd` (11 lines).  Suite PASS plain + AUTHOR_TESTING.
