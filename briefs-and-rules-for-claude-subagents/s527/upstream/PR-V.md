# PR V -- Lexer: a [ or { after a dereference block or a key/value slice name subscripts it

**The perl rule.** After a term perl expects an operator, and a `[` or `{` there subscripts the term: `${$r}[0]`, `${a}[1]`, `@{$r}[0, 1]`, `%{$r}[0, 1]`, `%{$r}{'a'}`, and 5.20's key/value slices `%h{'a', 'b'}`, `%a[0, 1]`.

    perl -e '@a = (1, 2, 3); print ${a}[1], "\n"'                       # 2
    perl -e '%h = (a => 1, b => 2); %s = %h{"a"}; print keys %s'         # a

**PPI before / after:**

```
BEFORE (master 455bfa73)
${$r}[0]     Cast($) Block({$r}) Constructor([0])
%h{'a'}      Symbol(%h) Block({'a'})
%a[0, 1]     Symbol(%a) Constructor([0, 1])

AFTER (branch pcl)
${$r}[0]     Cast($) Block({$r}) Subscript([0])
%h{'a'}      Symbol(%h) Subscript({'a'})
%a[0, 1]     Symbol(%a) Subscript([0, 1])
```

`print {$fh} [1]` keeps its anonymous-array Constructor (the block there is a filehandle, not a dereference).

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §17 and §36.  Regression pair `62_deref_subscript`.

**Commits:**

- `0e07c904` TODO tests: a [ or { after a dereference block or a key/value slice name subscripts it
- `75fc24e8` Lexer: a [ or { after a dereference block or a key/value slice name subscripts it
