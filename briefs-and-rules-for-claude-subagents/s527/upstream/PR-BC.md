# Operator: << NAME after a term is the left shift  (PCL §69; group BC)

**Perl's rule.** perl starts a here-doc only where it expects a term (toke.c `yyl_leftpointy`: under `XOPERATOR` `<<` is `SHop(OP_LEFT_SHIFT)`; `scan_heredoc` runs otherwise), so after a complete term `<<NAME` is a left shift by a call of NAME.

    perl -e 'my $n = (1<<index("ab","b")); print "$n\n"'   # 2
    # core Devel::Peek, debug_flags():  $num |= (1<<index($D_flags, $_)) for split //, $on;

**Before.** `PPI::Token::Operator` made a here-doc of every `<<` followed by a word, a quote or `\word`: `1<<index(...)` was `Number HereDoc<<<index>`, and the here-doc body swallowed every following line up to one reading `index` (in Devel/Peek.pm: the rest of `debug_flags`).  `1<<2` was right (a digit cannot name a here-doc).
**After.** The `<<` arm asks `_after_term` -- the term-end rule of the `%`, `*`, `.digit` and `~~` decisions -- before it makes a here-doc of `<<NAME`: after a number, a variable, a `)`, a `]`, a subscript's `}`, a method name, a postfix `++`, a string or a builtin that takes no operand it is the shift.  In the filehandle slot `print $fh <<EOT` (whitespace before the `<<`) the here-doc stays, as for perl; `print $fh<<EOT` is a shift, as for perl.

**Not changed:** a QUOTED terminator after a term (`1 << "EOT"`, a shift by a string for perl, which no real program writes) keeps the here-doc reading: the fuzzed fixtures `t/data/08_regression/45_heredoc_w_paren_in_terminator` and `47_...` exercise the here-doc terminator scanner on exactly that shape (`)<<~'...'`), and reading them as shifts would leave the scanner untested.  Its row stays TODO.

**Rows fixed:** PCL §69 (fourteen of fifteen rows; fourteen controls).  TODO tests commit `71e44672`, then the fix `eebdaacc`.  Suite PASS plain + AUTHOR_TESTING.
