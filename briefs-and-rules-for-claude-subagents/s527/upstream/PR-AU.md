# Tokenizer, Lexer: a format body is one token, and the statement ends with it  (PCL §40; group AU)

**Perl's rule.** perl reads the lines after `format NAME =` as the format's picture and argument lines, never as code, up to and including the line that holds a lone `.` (perlform; toke.c `scan_formline`).

    printf 'our ($a, $b) = ("x", "y");\nformat STDOUT =\n@<< @>>\n$a, $b\n.\nwrite;\nprint "after\\n";\n' | perl
    # x    y
    # after

**Before.** PPI has no format support: it lexed the picture and argument lines as code, and the statement it built ran on past the `.` into the next statement, or past the `}` of the block holding the format.
**After.** A new `PPI::Token::Format` holds those lines, the `.` line included, like a Pod token holds its section (`lines` returns them without the terminator).  When the word `format` starts a statement and the rest of its line is `[NAME] =`, the new `Tokenizer::_format_declaration` makes that line's tokens itself -- Word `format`, the name as one Word (perl takes `'one` and `::two` there), Operator `=` -- and the next line starts the Format token, which adds lines up to the `.` line.  The Lexer ends the statement after the Format token, as after a `;`, and after it a statement starts (`_label_position`, `_after_statement_block`, `_opcontext`'s table).  The text of the document is unchanged; `format` as a hash key, a method name or a sub name stays a Word (controls).

**Found by:** perl's own `t/op/write.t`, `t/op/gv.t`, `t/op/rt119311.t`, `t/comp/parser.t`.

**Three more pairs** (each found on perl's own t/ by PCL's instruments after the first pair landed):
* `f19956ec` / `87166cf8` -- after a format body a statement starts in the three context tests that did not know the new token: `_opcontext` (operand; the token is not in `%OBVIOUS_CLASS`, whose entries `_ends_term_at` reads as term ends), `_cast_or_op` (a `*{` cast) and Whitespace's `/` rule (a match).  t/comp/form_scope.t: `*STDOUT = *STDOUT7{FORMAT};` right after a format.
* `10d805e3` / `800aedcb` -- perlform: an argument line that starts with `{` spreads over more lines, and that block is code, so a lone `.` inside it (closing a NESTED format) does not end the outer one; `Token::Format` counts the braces of such a line.  t/op/write.t 'nested formats'.
* `a9b7a1ac` / `86623347` -- the prototype test (Whitespace.pm) and the attribute test (Unknown.pm) take `sub NAME` after a Format body as a declaration, as after a `;`.  t/op/write.t: `sub has_format ($)` after three formats.

**Rows fixed:** PCL §40 (six rows: the picture line, the format / write / print statements, `format =` for STDOUT, a quoted name inside a block, two formats in a row; then three, one and two rows for the later pairs; five controls).  TODO tests commit `db021031`, then the fix `1ff6ccd8` (a new token class + 60 lines), and the three pairs above.  Suite PASS plain + AUTHOR_TESTING after every commit.
