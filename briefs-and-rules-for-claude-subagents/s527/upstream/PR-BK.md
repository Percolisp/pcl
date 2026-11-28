# Whitespace: a spaced <...> where a term is expected is a glob  (PCL §77; group BK)

**Perl's rule.** Where perl expects a term, a `<` starts a readline or a glob whatever character follows it (toke.c `yyl_leftpointy`: under any expectation but XOPERATOR `scan_inputsymbol` reads to the first `>`, and a `<...>` holding more than a filehandle name is a glob), so a word list written with spaces inside the brackets is a glob too.

    perl -e 'my @r = ([< 4 5 >], [< 6 7 >]); print "@{$r[1]}\n"'   # 6 7
    perl -e 'my $s = "p q"; my @w = < $s >; print "@w\n"'          # p q

**Before.** The `<` arm's two readline tests -- after an operator, a builtin that takes an operand or `use Module` (`_term_expected`), and after the `}` of a statement or `map` / `grep` block (`_after_statement_block`) -- asked for a closed `<...>` whose first character was not whitespace, so `[< 4 5 >]`, `{ < a b > }`, `k => < a b >`, `return < a b >`, `sort < ./g-*.t >`, `$x ? < a > : < b >`, `$s . < a >` and `map { ... } < a b >` were a less-than, words and a greater-than; only an older rule after `(` `;` `=` `,` and `while` took the spaced list.
**After.** Both tests ask one helper, `_closed_angle`: `<>`, or a `<` whose next character is not `<`, `>` or `=`, closed by a `>` on the line.  The term tests themselves are unchanged, so after a variable, a number, a call, a subscript, a method name or a builtin that takes no operand the `<` still compares.  Over 2,697 files (PCL's wide population: perl's own t/ and core lib, a CPAN board, Rosetta Code's Perl programs) the token streams of this group and BJ differ from the base only in the 13 programs that hold a word-list glob.

**Rows fixed:** PCL §77 (eleven rows; twelve controls: the unspaced list, the spaced one after `(` and `=`, a here-doc in the same slot, and the less-thans after a subscript, an element, a call, an array, a number, a method name, `time` and a dereference block).  TODO tests commit `48f53347`, then the fix `ea961bca`.  Suite PASS plain + AUTHOR_TESTING.
