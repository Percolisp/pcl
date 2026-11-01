# Tokenizer, Document: a here-doc in the code of a quote-like takes the lines after it  (PCL §42; group AQ)

**Perl's rule.** A here-doc introduced in the CODE of a quote-like -- the replacement of `s///e`, a `(?{ })` or `(??{ })` block of a pattern -- takes its body from the lines after the line its marker is on (perl5180delta: "The body of a here-document inside a quote-like operator now always begins on the line after the "<<foo" marker").

    printf '%s\n' '$_ = "a"; s/a/<<END/e;' foo END 'print;' | perl   # foo

**Before.** PPI read a body for a `PPI::Token::HereDoc` only: after `s/a/<<END/e` the body lines were lexed as code (`Word=foo Word=END`), and a here-doc later on the same line took them as its own body.

**After.** Two halves that only work together:

* `_QuoteEngine::Full::_fill` ends a `Regexp::Match`, `Regexp::Substitute` or `QuoteLike::Regexp` token by asking `_read_code_heredocs`: for each code part ending on the token's last line it tokenizes that code, and for each here-doc there that found no body it reads the following source lines up to the terminator (as `PPI::Token::HereDoc` does, the end-of-document space included) and keeps them as written in `_heredoc_lines`; `heredoc_lines` returns them.  A marker on an earlier line of the token has its body inside the token's text and is left alone; `1 << 2` is no here-doc to the tokenizer.
* `Document::serialize` writes a token's `_heredoc_lines` after the next newline, exactly as it writes a HereDoc body, and `index_locations` counts them: the round trip and the line numbers of what follows are unchanged (two guard rows outside the TODO block).

**Found by:** perl's own `t/base/lex.t` (`s/^not /substr(<<EOF, 0, 0)/e`, `qr/(?{<<END})/`, `s//"#" . <<END/e`) and `t/comp/parser.t`.

**Rows fixed:** PCL §42 (three rows; two guards; two controls).  TODO tests commit `e876be8b`, then the fix `a376a6d8` (89 + 17 lines).  Suite PASS plain + AUTHOR_TESTING.
