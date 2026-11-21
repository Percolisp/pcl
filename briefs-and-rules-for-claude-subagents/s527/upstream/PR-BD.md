# Lexer: a bare block opened by { ; ends its statement  (PCL §70; group BD)

**Perl's rule.** perl reads a `{` that starts a statement as a block unless its look-ahead finds an anonymous hash, and a `;` right after the `{` is perlref's way of saying "block" (toke.c `yyl_leftcurly` under `XSTATE`); a bare block is a statement of its own.

    perl -e '{;} print "after\n"'     # after
    perl -e '{; a => 1 } print 2'     # 2

**Before.** `_curly` blesses the statement a bare block starts into a `PPI::Statement::Compound` only when `_curly_lookahead` finds nothing.  When the look-ahead itself answered Block (the `;` at its first position) the class was returned at once and the statement stayed a plain `PPI::Statement`, which ran on past the `}`: `{;} print 1;` was ONE statement.
**After.** A Block from the look-ahead is the same bare block as its fall-through, so the statement is a Compound and ends at the `}`.  An anonymous hash the look-ahead finds (`{ 'a', 1 }`, `{ a => 1 }`, `{}`) stays inside its statement, as before.

**Not a bug (controls):** an EMPTY `{}` or `{ # comment }` at a statement start is an anonymous hash for perl itself (toke.c: a `}` right after the `{`, comments skipped, is `HASHBRACK`; `perl -e '{} print 1'` is a syntax error), so PPI's Constructor there is right.

**Rows fixed:** PCL §70 (seven rows; nine controls).  TODO tests commit `59e66c8a`, then the fix `842e6bbd`.  Suite PASS plain + AUTHOR_TESTING.
