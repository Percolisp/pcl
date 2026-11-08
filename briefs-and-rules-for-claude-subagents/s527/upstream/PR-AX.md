# Word: __DATA__ and __END__ before a fat comma are words  (PCL §64; group AX)

**Perl's rule.** perl tests for `=>` right after a word before it looks the word up as a keyword (toke.c `yyl_keylookup`), on the same source line, so `__DATA__` and `__END__` followed by `=>` are the strings `"__DATA__"` / `"__END__"` anywhere, and the code goes on.  `CORE::__DATA__` / `CORE::__END__` reach the keyword without that test.

    perl -e 'our %K = (__DATA__ => 2, __END__ => 3); print join(",", sort keys %K), "\n"'   # __DATA__,__END__

**Before.** `PPI::Token::Word::__TOKENIZER__commit` started the End / Data zone at the word, so the rest of the file -- here the end of the list and every statement after it -- was one `PPI::Token::Data`.
**After.** The word is ordinary when the rest of its line starts with `=>`.  A real `__DATA__` line and `__END__` followed by text still start the section (controls).

**Rows fixed:** PCL §64 (five rows; two controls).  TODO tests commit `36eccb52`, then the fix `e395a534` (11 lines).  Suite PASS plain + AUTHOR_TESTING.
