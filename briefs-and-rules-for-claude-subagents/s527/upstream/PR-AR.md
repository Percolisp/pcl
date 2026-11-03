# Lexer: the [ after the ) of a list or a qw() is a subscript  (PCL §48; group AR)

**Perl's rule.** After the `)` of a list -- also of a statement modifier's condition -- or of a `qw()` word list, perl's lexer expects an operator: toke.c `yyl_rightparen` returns a term, and `yyl_leftsquare` under XOPERATOR makes the `[` a subscript, so `(LIST)[i]` is a list slice.

    perl -e 'print +(10, 20)[1], " ", qw(a b c)[2], "\n"'   # 20 c

**Before.** `_square` decided the `[` by the element before it and knew no list, so the slice came out as an anonymous-array `Structure::Constructor` -- and a `{` after its `]` then came out as a `Structure::Block`.
**After.** `_square` returns a `PPI::Structure::Subscript` when the element before the `[` is a `Structure::List`, a `Structure::Condition` or a `QuoteLike::Words`.  In valid perl nothing else can follow such a `)` with a `[`: `f(1)[0]` and `$o->m()[0]` are syntax errors, and `$code->()[0]` is the same subscript with the arrow left out.  A `{` after the slice's `]` is then a Subscript by `_curly`'s existing rule.  The anonymous array after `print`, `return`, an operator or an opening `(` is unchanged (controls).

**Found by:** PCL's expect-state checker -- `list-slice-as-constructor`, the largest kind left (298 rows over perl's t/, its core lib, PCL's corpus and a CPAN board).

**Rows fixed:** PCL §48 (thirteen rows: twelve spellings of the slice -- the print argument, a named unary in the list, a negative index, a qw slice of two, a hash key, statement start, a statement modifier's condition, a code call with the arrow left out, a slice inside an anonymous array -- and the `{` after it; five controls).  TODO tests commit `84281eff`, then the fix `4637db29` (9 lines).  Suite PASS plain + AUTHOR_TESTING.
