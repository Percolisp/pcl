# Lexer: a { first inside a map block or a dereference block is a hash  (PCL §59; group AP)

**Perl's rule.** perl lexes the block of `map`, `grep` and `sort`, the filehandle block of `print` / `printf` / `say` / `exec` / `system`, and the block of a `$ @ % & $#` dereference where it expects a REFERENCE (toke.c: those list operators are `LOP(..., XREF)`, a cast is `PREREF`).  A `{` right after that block's `{` makes it expect a TERM (`yyl_leftcurly`, `block_expectation`: "treat it as a term to make `${{...}}{k}` ... dwim"), so that `{` is an anonymous hash whatever follows it.

    perl -e 'my @c = map { { %$_, x => 1 } } ({a => 1}); print scalar(@c), " ", ref $c[0], "\n"'   # 1 HASH

**Before.** `_curly` decided the inner `{` by the statement-start look-ahead (a word or string followed by `,` / `=>`), which takes `{ %$_, x => 1 }` -- the copy-with-one-more-key idiom -- and `{ $_, 1 }` for bare blocks.
**After.** A new `_opens_xref_block` asks whether the (still empty) statement the `{` starts is the first child of such a block (a null statement counts: after `map {;` the `{` starts a statement and stays a block; whitespace, comments and POD do not); then the `{` is a `PPI::Structure::Constructor`.  `map({ ... } LIST)` is the same block one list deeper.  The blocks of `do`, `eval`, `sub` and a bare block keep the look-ahead -- perl opens those where it expects a statement (control rows).  `map { {; 1 } }` is a perl syntax error, so nothing valid reads a block there.

**Rows fixed:** PCL §59 (five rows; two controls).  TODO tests commit `fb23f551`, then the fix `0aae7704` (43 lines).  Suite PASS plain + AUTHOR_TESTING.
