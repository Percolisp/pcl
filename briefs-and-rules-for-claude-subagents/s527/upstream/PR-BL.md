# Lexer: after an operator a { is an anonymous hash  (PCL §78; group BL)

**Perl's rule.** After an operator, a `\` or the named unary operators `defined` and `ref`, perl expects a term, and a `{` there is always an anonymous hash (toke.c `yyl_leftcurly`: under XTERM the `{` is HASHBRACK, whatever follows it).

    perl -e 'my @l = (3, 4); print "no\n" if not { map { $_ => 1 } @l }->{5}'   # no
    perl -MO=Deparse -e '$w = not {a=>1}->{a}'                                # $w = !{'a', 1}->{'a'};

**Before.** `Lexer::_curly` made the `{` a Constructor only after the operators `%CURLY_CLASSES` names (`=` `||=` `&&=` `//=` `||` `&&` `//` `?` `:` `,` `=>` `+`) and a few words (`scalar` `return` `bless` `die` `warn` `exists` `delete`); after `not`, `!`, `-`, `~`, `and`, `.`, `*`, a `\` cast, `defined` and `ref` it was a Block, so `not { map { ... } @l }->{5}` was a block holding the map, with a subscript after it.
**After.** A Constructor after any Operator but a postfix `++` / `--` (which ends a term) and after a `\` cast; `%CURLY_CLASSES` learns `defined` and `ref`.  A sigil cast's `{` is still its dereference block, and the `{` of `do`, `sub`, `eval`, `map`, `grep` and `sort` still opens their block.

**PPI's own fixtures.** `t/ppi_token_unknown.t` stored the old reading of the braces after `*`, `%`, `&`, `**`, `*=` and the other operator rows, marked there "should be constructor".  They are updated in the fix commit, under four conditions: the fixture reproduced this bug; perl's reading was probed (`perl -MO=Deparse -e '$z = 1 * {x=>2}'` prints `$z = 1 * {'x', 2};`); the diff is exactly the class of those braces (and the `Statement::Expression` inside `{x=>2}`, which every anonymous hash holding `=>` gets); nothing else changes.  The `{$args}` fixture serves two kinds of rows -- after an operator, where it is now a hash, and after a sigil cast (`+*{$args}`), where it is a dereference block -- so the operator rows take a twin of it, `@hashctor3_term`, and the cast rows keep it as it was.

**Rows fixed:** PCL §78 (ten rows; thirteen controls).  TODO tests commit `fabedb61`, then the fix `9a7338de`.  Suite PASS plain + AUTHOR_TESTING.
