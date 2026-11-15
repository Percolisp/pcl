# Lexer: the first { inside a constructor is an anonymous hash  (PCL §67; group AZ)

**Perl's rule.** perl expects a term after the `[` of an anonymous array and after the `{` of an anonymous hash, and where it expects a term every `{` is an anonymous hash (toke.c `yyl_leftcurly`: `XTERM` makes `HASHBRACK`).  The look-ahead that tells a block from a hash runs only where a statement or a block may start.

    perl -e 'my $x = "k"; my $r = [ { \1, "abc" }, { $x, 1 } ]; print ref $r->[0], ref $r->[1], "\n"'   # HASHHASH

**Before.** `_curly` sent the first `{` of a statement inside a constructor to the statement-start look-ahead, which knows a hash only by a literal and a comma after the `{`: `[ { 'abc', 1 } ]` was a Constructor, but `[ { \1, "abc" } ]`, `[ { $x, 1 } ]`, `[ { %h } ]` and `[ { foo(), 1 } ]` were Blocks, and the statement inside the `[` was reblessed a Compound.
**After.** `_curly` returns a Constructor for the first `{` of a statement whose parent is a `PPI::Structure::Constructor` -- the sibling of the rule that makes a `{` opening a list's statement a constructor.  A `{` after a word inside the constructor (`do`, `sub`, `eval`, `map`, `grep`) is decided by that word as before (controls).

**Rows fixed:** PCL §67 (seven rows; eight controls).  TODO tests commit `c1189f77`, then the fix `af11a2c3`.  Suite PASS plain + AUTHOR_TESTING.
