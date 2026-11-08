# Tokenizer: x and x<digits> after a block that ends a statement are words  (PCL §63 and §19b; group AW)

**Perl's rule.** `x3` is the repetition operator `x 3` only where perl expects an operator (toke.c, `yylex`: `case 'x'` tests `PL_expect == XOPERATOR`).  After the `}` of a block that opens a statement or takes a list -- a sub definition, a bare block, `BEGIN`, a compound statement's block, a `map` / `grep` / `sort` block -- perl expects a statement or a term, so `x3` (and `x`) is a word: a call.

    perl -e 'sub x3 { print "called\n" } x3(); my @m = map { 1 } x3();'   # called / called

**Before.** `_current_x_is_operator` took every `}` as the end of a term, so `sub x3 { } x3();` lexed as `Operator(x) Number(3)` and the call was lost; `sub x { } x` began a statement with the infix operator.
**After.** `_current_x_is_operator` asks `_after_statement_block` -- the rule the `-`, `/` and `<` decisions already use.  That helper learns the two block shapes it missed, for all four callers: a sub definition whose body follows a prototype or attributes (`sub f ($) { }`, `sub f :lvalue { }`; `_before_sub_decorations` steps back to the NAME, or to `sub` for an anonymous sub, a term), and a block after the `)` of a compound statement's condition, a foreach list or a named sub's signature (`_paren_heads_statement_block`; an XBLOCK pushes XSTATE in toke.c), so `if (1) { } -e $f` is a filetest.  After a term, a `do` block, an anonymous hash, an anonymous sub, a list and a subscript `x` stays the operator, and `do { 5 } -1` subtracts (controls).

**Rows fixed:** PCL §63 (eleven rows; seven controls) and the statement-start row of §19 (19b).  TODO tests commit `845b663c`, then the fix `e45f69ab` (64 lines).  Suite PASS plain + AUTHOR_TESTING.
