# Lexer: the ( after a method named if or while holds its arguments  (PCL §58; group AO)

**Perl's rule.** A word after `->` is a method name: toke.c reads it where it expects an operator (a METHCALL), never as a keyword, so a method may be named like a condition keyword and the `(` after it is its argument list.

    perl -e 'sub F::if { "if$_[1]" } print F->if(1), "\n"'   # if1

**Before.** `PPI::Lexer::_round` looked the word up in its keyword table without asking whether it follows `->`: `$f->if(1)`, `->elsif(`, `->unless(`, `->while(`, `->until(` built a `PPI::Structure::Condition` (`->for(` and `->foreach(` came out a List only because the ForLoop check reblesses a For with fewer than two statements).
**After.** `_round` skips the table when the word's previous significant sibling is `->` (the test `_curly` / `_square` make for `->{` / `->[`), so every such `(` is a `PPI::Structure::List` holding a `Statement::Expression`, as after `$f->foo(`.  `if (...)`, `elsif (...)`, `while (...)` and the statement modifiers are unchanged (control row).

**Found by:** Template Toolkit's generated grammar (`Template/Grammar.pm`) calls `$factory->if(...)` and `$factory->while(...)`.

**Rows fixed:** PCL §58 (five rows; one control).  TODO tests commit `7bd8dfcd`, then the fix `db20f1eb` (7 lines).  Suite PASS plain + AUTHOR_TESTING.
