# Tokenizer: x<digits> is a word where perl expects a term  (PCL §66 and §19; group AY)

**Perl's rule.** `x3` is the repetition operator `x 3` only where perl expects an operator (toke.c, `yylex`: `case 'x'` tests `PL_expect == XOPERATOR`).  After a builtin that takes an operand (`print`, `return`, `sort`, `defined`, the `if` of a statement modifier, ...) perl expects a term; in the filehandle slot of `print $fh LIST` it reads the whole word after the handle and keeps the operator only for a binary-operator keyword such as `x` itself (toke.c, `case '$'`); and a number is never followed by an opening paren, so `x3(` is a call wherever it stands.

    perl -e 'sub x3 { "X3" } print x3(), "\n"; print STDOUT x3(), "\n"; print "a" x3, "\n"'   # X3 / X3 / aaa

**Before.** `_current_x_is_operator` took every Word before the `x` as the end of a term, so `print x64()` lexed as `Word(print) Operator(x) Number(64)` and `print STDOUT x3()` as `... Word(STDOUT) Operator(x) Number(3)`: the call was lost (and after a filehandle the statement printed nothing).
**After.** After a Word, `_current_x_is_operator` asks `_term_expected` -- the rule the readline and `~~` decisions already use -- so after a builtin that takes an operand `x` and `x3` are words, while after a builtin that takes no operand (`time x 3`), a method name (`$o->print x 3`), `__PACKAGE__` and a user's word the operator stays.  The Whitespace caller says whether the `x` is glued to digits, so the `print $fh` slot gets perl's whole-word answer (`print $fh x3()` calls, `print $s x 3` repeats).  The look-ahead that splits `x3` into `x` and `3` leaves `x3(` whole.  A bareword handle before a paren-less `x3` (`print STDOUT x3`) stays the operator: the word could be a constant (`print C x3` repeats in perl), which the tokenizer cannot know.

**Rows fixed:** PCL §66 (fourteen rows; twelve controls) and the `print x()` row of §19.  TODO tests commit `d340180c`, then the fix `a9a16511`.  Suite PASS plain + AUTHOR_TESTING.
