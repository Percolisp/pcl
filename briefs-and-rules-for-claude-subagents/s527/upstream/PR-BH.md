# Magic: %^ where a term is expected is one token  (PCL §74; group BH)

**Perl's rule.** perl reads `%^` where it expects a term as the punctuation hash `%^` (the hash of `$^`), as it reads `%-` and `%+`, and `%^` followed by a caret name's character as that caret variable's hash, as `$^W` is one variable.

    perl -e 'no strict; %^ = (a => 1); print keys %^, "\n"'      # a
    perl -e 'no strict; %^W = (w => 1); print keys %^W, "\n"'    # w

**Before.** `PPI::Token::Magic` kept `%^` only when the one known name `%^H` followed and otherwise backed the `%` off to an Operator: `%^ ? 1 : 0`, `print %^` and `keys %^` were a modulus and a bit-xor with no operands, `print %^W` was `% ^ W`.
**After.** The `%` arm of `PPI::Token::Unknown` starts a Magic `%^` only where no term has ended (after a term `%` is the modulus), and there `PPI::Token::Magic` keeps `%^` + a caret name's character as one token and `%^` alone otherwise.

**Rows fixed:** PCL §74 (seven rows; five controls).  TODO tests commit `ac6b15c2`, then the fix `72d2bd9e`.  Suite PASS plain + AUTHOR_TESTING.
