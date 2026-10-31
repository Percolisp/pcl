# PR AH2 -- §14 (the after-an-operator / after-a-builtin rows; s527g)

**The perl rule.** Where perl expects a term -- after an operator, and after a
builtin that takes an operand (`print`, `sort`, `reverse`, `return`, ...) -- a
closed `<...>` is a readline or a glob, whatever it holds (toke.c
`yyl_leftpointy`: `PL_expect` is not `XOPERATOR`).

    perl -e 'my @b = sort </tmp/x-1/y/*.pl>; print scalar(@b), "\n"; print "next\n"'   # 0 / next
    echo x | perl -e 'print <STDIN>'                                                  # x

**PPI before / after:** `dump-AH2.txt`.  Before, the `<` is a less-than
everywhere but after `=`, `,`, `(`, `;` and `while`; with a path that holds
`-DIGIT` the `-1` becomes a Number and `y/` a transliteration that swallows
the next statement.  After, the five shapes read as perl reads them; after a
USER's word (`foo <STDIN>`) the `<` stays a less-than (only a declaration says
`foo` is a list operator), and after a builtin that takes no operand
(`time < 5`) it stays a comparison.

**How.** The term test `Operator.pm` already used for a term-initial `~~`
(`__term_expected`) moves to `PPI::Tokenizer::_term_expected` and is shared
(it now also sees through a `CORE::` prefix); `Whitespace.pm`'s `<` branch asks
it before its old default.  A block's `}` keeps the rule of PR AH.

**Rows fixed in t/ppi_bugs_pcl.t:** the five PCL §14 rows of "Bug 11" (`sort
<STDIN>`, the two ternary cascades, the two glob-pattern cascades) and the new
last §14 row (`sort </tmp/x-1/y/*.pl>`).

**Commits:** `30d7356c` TODO tests, `5c74a167` the fix.  PPI's own suite passes
after each, plain and AUTHOR_TESTING=1.
