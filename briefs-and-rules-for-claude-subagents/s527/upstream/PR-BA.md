# Document: keep_line_endings tokenizes the source as it is  (PCL §65; group BA)

**Perl's rule.** perl reads a source line up to its LF.  A lone CR is a character of the program: inside a string, in a comment (it ends no comment and no line), between tokens.  In a string eval every CR of a literal is kept; the `__DATA__` / `__END__` section is read raw; only a source FILE's line reader turns CR LF into LF (toke.c without `PERL_STRICT_CR`), and a here-doc's terminator line may end in CR LF.

    perl -e 'print length eval "q{a\r\nb}"'               # 4
    printf 'print length "c\rd";\n' > t.pl; perl t.pl     # 3

**Before.** `PPI::Tokenizer->new` rewrites every CR LF, CR CR LF and lone CR of the source to LF before it splits the source into lines, so no token can hold a CR.  That normalisation is PPI's documented behaviour -- `t/06_round_trip.t` compares against it -- so this is an opt-in, not a behaviour change.
**After.** `PPI::Document->new(\$source, keep_line_endings => 1)` (and the same argument to `PPI::Lexer->lex_source` / `lex_file` and `PPI::Tokenizer->new`) skips the rewrite: lines end at LF only, a CR stays a character of its line.  What reads a line end learns CR LF: a here-doc's terminator line, the line an indented here-doc's indentation is measured on and its blank lines; the rest of an `__END__` / `__DATA__` line keeps its CR LF as the Whitespace after the separator.  The document serializes to the exact bytes it was made from.  The document cache stores normalised documents only and is not used for one made with the attribute.  The default is unchanged (a control row).

**Rows fixed:** PCL §65 (eight rows; two controls -- the three earlier §65 rows now pass the attribute).  TODO tests commit `cc0b1f30`, then the fix `66e690d2`.  Suite PASS plain + AUTHOR_TESTING.
