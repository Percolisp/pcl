# Unknown: the attributes of a lexical sub  (PCL §60; group AS)

**Perl's rule.** perl reads the attribute list of a lexical sub (`my sub NAME`, `our sub NAME`, `state sub NAME`) as it reads a named sub's -- toke.c `yyl_sub` takes the same path after the name.

    perl -e 'use feature "lexical_subs"; no warnings; my $x = 43; my sub y :prototype() {$x} print y, "\n"'   # 43

**Before.** `__TOKENIZER__is_an_attribute` accepted `sub NAME :` only when `sub` starts the statement (nothing, a Structure or an empty Whitespace before it), so after `my` / `our` / `state` the `:` was an Operator, the attribute name a Word and its parameters a List -- the prototype was lost.
**After.** It also accepts `sub NAME :` when the word before `sub` is `my`, `our` or `state` (`Token/Whitespace.pm`'s prototype test already knows the lexical case).  A named sub, a variable declaration's attributes and the ternary `:` after an anonymous sub are unchanged (controls).

**Found by:** perl's own `t/op/lexsub.t` (line 850).

**Rows fixed:** PCL §60 (four rows: my, state, our, a second attribute after a prototype; three controls).  TODO tests commit `063c917a`, then the fix `c020b142` (6 lines).  Suite PASS plain + AUTHOR_TESTING.
