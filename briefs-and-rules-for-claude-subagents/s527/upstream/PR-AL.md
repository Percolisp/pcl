# PR AL -- §43 (a lexical sub named like a keyword; s527g)

**The perl rule.** Since perl 5.18 `my sub NAME`, `state sub NAME` and `our
sub NAME` may take a keyword's name, and in the rest of the enclosing block the
bare NAME is the sub: toke.c `yyl_keylookup` looks a word up in the pad before
the keyword table, wherever perl does not expect an operator.

    perl -e 'no warnings; my sub s { "S@_" } print s(1), "\n"; my sub y { "Y@_" } print y(2), "\n"'   # S1 / Y2

**PPI before / after:** `dump-AL.txt`.  Before, `s(1)` is a substitution that
runs to the end of the text, `y(1)` a transliteration, `x(5)` the repetition
operator.  After, each is a Word and a call; `3 x 2` still repeats (after a
term perl expects an operator); after the block's `}` the keyword is back
(`y/a/b/` transliterates).

**How.** The tokenizer keeps the names a lexical sub declaration introduces,
each with the brace depth of its block: a bounded set, kept only once a file
declares a lexical sub, which the `}` that closes the block empties again
(`_lexical_sub_word`, `_lexical_sub_scope`).  `__current_token_is_forced_word`
consults it, so every path that asks whether a keyword is forced to be a word
sees it: both paths of `PPI::Token::Word` (which now pass the word) and the
`x` test of `_current_x_is_operator`.  `my sub if` + `if if if` is a statement
question for the Lexer, not a token one: the token is a Word either way.

**Rows fixed in t/ppi_bugs_pcl.t:** the three PCL §43 rows.

**Commits:** `585ea9dd` TODO tests, `c4d0da86` the fix.  PPI's own suite passes
after each, plain and AUTHOR_TESTING=1.
