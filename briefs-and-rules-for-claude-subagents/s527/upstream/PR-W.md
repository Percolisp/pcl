# PR W -- Lexer: { STRING, ... } where a statement starts is an anonymous hash

**The perl rule** (toke.c, the curly look-ahead).  A statement-initial `{` is an anonymous hash when `}` follows at once, when the first term is followed by `=>`, or when the first term is a string (a quote, `qw`, backticks, `qx`) or a run of word characters that does not start with a lower-case letter (`1`, `0x1F`, `Foo`) and a comma follows it.  `{ foo, 1 }`, `{ $x, 1 }` and `{ 1.5, 2 }` are blocks.

    perl -e 'sub f { { "a", "foo" } } print ref f(), "\n"'    # HASH
    perl -e 'sub g { { foo, 1 } } print scalar(() = g()), "\n"'   # 2 (a block)

**PPI before / after:**

```
BEFORE (master 455bfa73)
{ 'a', 'foo' };    Statement::Compound: Block
{ 1, 2 };          Statement::Compound: Block

AFTER (branch pcl)
{ 'a', 'foo' };    Statement: Constructor
{ 1, 2 };          Statement: Constructor
{ foo, 1 };        Statement::Compound: Block   (unchanged, as in perl)
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §5 (five rows, five controls).  Regression pair `63_hash_comma`.

**Commits:**

- `e1c736f9` TODO tests: { STRING, ... } where a statement starts is an anonymous hash
- `a2f28ed9` Lexer: { STRING, ... } where a statement starts is an anonymous hash
