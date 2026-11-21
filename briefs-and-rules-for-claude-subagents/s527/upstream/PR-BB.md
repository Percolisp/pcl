# Singletons: __FILE__, __LINE__, __PACKAGE__, __SUB__, __CLASS__ end a term  (PCL §68; group BB)

**Perl's rule.** perl's lexer returns each of these words as a whole term and expects an operator after it (toke.c: `FUN0OP` for `__FILE__` `__LINE__` `__PACKAGE__` `__SUB__`, `FUN0` for `__CLASS__`; both set `PL_expect = XOPERATOR`) -- the rule it applies after `time` or `wantarray`.

    perl -e 'print __LINE__-1, " ", __FILE__.1, " ", __LINE__.5, "\n"'   # 0 -e1 15

**Before.** `%NO_OPERAND`, the table `_ends_term_at` reads, listed the builtins that take no operand but not these five words, so `__LINE__-1` was `Word Number<-1>`, `__FILE__.1` was `Word Number::Float<.1>` and `print __LINE__ %h` was the hash `%h`.
**After.** The five words join `%NO_OPERAND`: every term-end decision -- the sign of a number (`_opcontext`), `.digit`, the `%` and `*` arms, `~~`, `x` -- reads an operator after them.  (The upstream `%X_CAN_FOLLOW_WORD` list already counted `__SUB__` as a word that takes no operand.)

**Rows fixed:** PCL §68 (eleven rows; seven controls).  TODO tests commit `ff1cd0e7`, then the fix `b341df79`.  Suite PASS plain + AUTHOR_TESTING.
