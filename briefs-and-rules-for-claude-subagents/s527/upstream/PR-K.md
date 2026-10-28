# PR K -- A { that opens the document is a block

**The perl rule.** perl reads a { at the start of a statement as a block, so `{ 1 } *g = \&f` assigns a glob after a bare block.

    perl -e '{ 1 } *g = sub { 7 }; print g(), "\n"'   # 7

**PPI before / after:**

```
BEFORE (master 455bfa73)
{ 1 } *g = \&f;
    Structure({) Number(1) Structure(}) Symbol(*g) Operator(=) Cast(\) Symbol(&f) Structure(;)

AFTER (branch pcl)
{ 1 } *g = \&f;
    Structure({) Number(1) Structure(}) Symbol(*g) Operator(=) Cast(\) Symbol(&f) Structure(;)
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §38.

**Commits** (branch `pcl`, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each, plain and AUTHOR_TESTING=1):

- `d8bcdfa1` Tokenizer: a method name, a postfix ++/-- and a subscript's } end a term
- `0a536af5` Tokenizer: a { that opens the document is a block
