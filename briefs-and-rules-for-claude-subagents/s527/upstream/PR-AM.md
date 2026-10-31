# Lexer: defer BLOCK is a compound statement of its own  (PCL §52; group AM)

**Perl's rule.** Under the defer feature (perl 5.36) `defer BLOCK` is a statement that ends at its block (perly.y `barestmt: KW_DEFER mblock`); the block runs when control leaves the enclosing block.

    perl -E 'use feature "defer"; no warnings; { defer { print "b\n" } print "a\n" }'   # a, then b

**Before.** PPI tracked the feature (`Statement::Include::feature_mods`, group AG) but the Lexer built one plain `PPI::Statement` from the word, the block and the NEXT statement.
**After.** Under the feature `defer` opens a `PPI::Statement::Compound` exactly as `try` does; `_continues` takes the block and ends the statement there; `Compound->type` answers `defer`.  Without the feature `defer` stays an ordinary word (control row).

**Rows fixed:** PCL §52 (two rows; one control).  TODO tests commit `d21a9b3f`, then the fix `41cf1558` (15 lines).  Suite PASS plain + AUTHOR_TESTING.
