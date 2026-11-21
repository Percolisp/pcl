# Word: _ is a word except as a filetest's or stat's operand  (PCL §73; group BG)

**Perl's rule.** perl reads `_` as a plain word (toke.c); the bareword `_` is the stat filehandle only as the operand of a filetest, `stat` or `lstat` (op.c `ck_ftst`).  A sub may be named `_` -- the gettext idiom -- and elsewhere the bareword is the string `"_"`.

    perl -e 'sub _ { "x" } print _(), "y\n"'           # xy
    perl -e 'my %h = (_ => 1); print keys %h, "\n"'    # _
    perl -MO=Deparse -e '_+1;'                         # '_' + 1;

**Before.** `PPI::Token::Word` made a lone `_` Magic everywhere but after `sub`: `_("text")` was the stat filehandle followed by a list, `(_ => 1)` and `$h{_}` had a filehandle for their key, `__PACKAGE__->_()` called a filehandle, `sort _ @x` sorted by one.
**After.** `__underscore_class` says Magic only after a filetest operator (`-e _`, `-f -w _`) and after `stat` / `lstat`, with or without a `(` between; everywhere else `_` is a Word.  The fuzzed fixture `t/data/08_regression/45_heredoc_w_paren_in_terminator` opens with `_+`, which perl reads as the bareword: its dump stored the Magic reading, and its one changed line is that token (nothing else in the fixture moves).

**Rows fixed:** PCL §73 (ten rows; nine controls).  TODO tests commit `96155372`, then the fix `f8630d3e`.  Suite PASS plain + AUTHOR_TESTING.
