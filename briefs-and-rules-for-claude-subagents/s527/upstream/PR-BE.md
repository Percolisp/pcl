# Whitespace: / after a builtin that takes an operand starts a match  (PCL §71; group BE)

**Perl's rule.** perl reads a `/` as the start of a match wherever it expects a term (toke.c `yyl_slash`: `scan_pat` unless `PL_expect` is `XOPERATOR`), and after a builtin that takes an operand -- a list operator, a named unary, the `when` / `while` / `until` / `for` modifiers -- it expects one.

    perl -e '$_ = "x"; print /x/, "\n"'                              # 1
    perl -e '$_ = "a;b"; my @r = (sin /;/, 9); print "@r\n"'         # 0.841470984807897 9
    perl -e 'my $x = shift // 7; print "$x\n"'                       # 7 (the defined-or)

**Before.** The `/` arm of `PPI::Token::Whitespace` asked a word list of its own (`return split if unless grep map`) and read every other builtin's `/` as a division, so the pattern was tokenized as code: `sin / 25 ; # / ; die "x";` ended its statement at the `;` inside the pattern and commented out the `die`, and `1 while /(a+b?)x/g;` swallowed the rest of the file.  After a method name the list misread the other way: `$o->split / 3` was a match.
**After.** After a Word the arm asks `PPI::Tokenizer::_term_expected` -- the rule the `<`, `x` and `~~` decisions already use -- in place of the list.  Two neighbours of the same question, both perl's: a `//` after a builtin whose operand may be left out (`getc pop pos readline readlink readpipe shift undef umask`, toke.c's `UNIDOR` / `XTERMORDORDOR`) stays the defined-or; in the filehandle slot of `print $fh LIST` (whitespace before the `/`, none after it) a `/` starts the list (toke.c, case `'$'`: "e.g. print $fh /.../").  `_term_expected` learns that a Word spelled like an operator or a quote-like operator (`x`, `lt`, `s`, `y`) is a user's sub, not a builtin that takes an operand (toke.c sends `KEY_x` in term position to `just_a_word`).

**Not changed (the user's word):** after a word that is not a builtin (`ok /x/` with `sub ok` declared or imported) only a declaration says the word takes a list; PPI keeps the division and PCL's §11 rows stay TODO.

**Rows fixed:** PCL §71 (eighteen rows; eighteen controls + one for the operator-spelled word) and the two `while` rows of §11.  TODO tests commit `40b87033`, then the fix `d27d0c37`.  Suite PASS plain + AUTHOR_TESTING.
