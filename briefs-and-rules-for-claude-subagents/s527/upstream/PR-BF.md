# Unknown: % and a digit run where a term is expected names a hash  (PCL §72; group BF)

**Perl's rule.** perl reads `%12` where it expects a term as the hash named `12`, as it reads `@12` as the array (toke.c: under `XTERM` a `%` starts `scan_ident`, and a digit run is an identifier); after a term the `%` is the modulus.

    perl -e 'no strict; %12 = (a => 1); print keys %12, "\n"'    # a
    perl -e 'print 7%12, "\n"'                                   # 7

**Before.** The `%` arm of `PPI::Token::Unknown` made an Operator of every `%` that a digit follows: `%12 = (a => 1)` was `Operator(%) Number(12) Operator(=) ...`, a modulus with no left operand, while `@12` is one Magic token.  A cast takes a term too, and PPI did not know it: `\%12`, `\*1` and `\<STDIN>` lexed as a modulus, a multiplication and a less-than.
**After.** The digit test asks `_term_expected`: where a term is expected `%12` is one Symbol whose name is the digit run -- it ends at the last digit, as perl's `scan_ident` reads it (`%12x 2` is `%12 x 2`); after a term, after `time`, after a user's word (a constant, for all PPI knows: `K %12`) and in `print $fh %12` (a digit cannot start the list there) it stays the modulus.  `_opcontext` reads a `PPI::Token::Cast` as an operand context, so `\%12`, `\*1` and `\<STDIN>` lex as perl reads them; `_ends_term_at` now reads only the operator entries of the class table (a cast does not end a term).

**Rows fixed:** PCL §72 (twelve rows; twelve controls).  TODO tests commit `d920e6ac`, then the fix `911e43a3`.  Suite PASS plain + AUTHOR_TESTING.
