# PR X -- Lexer: the caller's $/ does not reach the tokenizer

**The rule.** `$/` is perl's input record separator; it has nothing to do with how source text is tokenized.  The tokenizer splits on `"\n"` and then `chomp`s the rest of a comment line and of the `__END__` / `__DATA__` lines -- and `chomp` removes `$/`.

    perl -MPPI -e 'my $s = "# c\n__END__\n"; { local $/; print length PPI::Document->new(\$s)->serialize }'   # 13 before, 12 after

**PPI before / after** (`# c\nx; # d\n__DATA__\nfoo\n` with `local $/`):

```
BEFORE (master 455bfa73)
Comment(# c\n) ... Comment(# d\n) Separator(__DATA__) Comment(\n) Whitespace(\n) Data(foo\n)
AFTER (branch pcl)
Comment(# c) Whitespace(\n) ... Comment(# d) Whitespace(\n) Separator(__DATA__) Whitespace(\n) Data(foo\n)
```

`lex_tokenizer`, the one place every document is lexed, sets `local $/ = "\n"` (the tokenizer runs lazily inside it).  A standalone `PPI::Tokenizer` still depends on `$/`; replacing the `chomp`s in Token/Word.pm and Token/Comment.pm with `s/\n\z//` would close that too.

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §13 (three rows).

**Commits:**

- `bd99765f` TODO tests: the tokens of a document do not depend on $/
- `3a08ed1f` Lexer: the caller's $/ does not reach the tokenizer
