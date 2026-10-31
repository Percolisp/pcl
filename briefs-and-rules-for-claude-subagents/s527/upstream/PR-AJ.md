# Lexer: a bare block takes a continue block  (PCL §51; group AJ)

**Perl's rule.** A bare block is a loop that runs once (perlsyn) and takes a continue block like any loop (perly.y: `block cont`):

    perl -e '{ $n++ } continue { $n += 10 } print "$n\n"'    # 11

**Before.** `_continues` ended a one-block statement unconditionally: `Statement::Compound[{ $n++ }]` then `Statement[continue { $n += 10 } print "$n\n";]` -- the continue block AND the next statement in one plain statement.  (`L: { } continue { }` was already right.)
**After.** `Statement::Compound[{ $n++ } continue { $n += 10 }]`, `Statement[print "$n\n";]`.

**Rows fixed:** PCL §51.  TODO tests commit, then the fix (11 lines in `_continues`).  Suite PASS plain + AUTHOR_TESTING.
