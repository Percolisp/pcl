# PR O -- Whitespace: a / after a statement block starts a match

**The perl rule.** After the } of a block that opens a statement or follows a word, perl expects a term, so a / starts a match.

    perl -e '$_="x"; { 1 } /x/ and print "m\n"'   # m

**PPI before / after:**

```
BEFORE (master 455bfa73)
BEGIN { 1 } /x/ and print;
    Word(BEGIN) Structure({) Number(1) Structure(}) Operator(/) Word(x) Operator(/) Operator(and) Word(print) Structure(;)
@a = grep { 1 } /(x)/;
    Symbol(@a) Operator(=) Word(grep) Structure({) Number(1) Structure(}) Operator(/) Structure(() Word(x) Structure()) Operator(/) Structure(;)
$x = $h{a} /2;
    Symbol($x) Operator(=) Symbol($h) Structure({) Word(a) Structure(}) Operator(/) Number(2) Structure(;)

AFTER (branch pcl)
BEGIN { 1 } /x/ and print;
    Word(BEGIN) Structure({) Number(1) Structure(}) Regexp::Match(/x/) Operator(and) Word(print) Structure(;)
@a = grep { 1 } /(x)/;
    Symbol(@a) Operator(=) Word(grep) Structure({) Number(1) Structure(}) Regexp::Match(/(x)/) Structure(;)
$x = $h{a} /2;
    Symbol($x) Operator(=) Symbol($h) Structure({) Word(a) Structure(}) Operator(/) Number(2) Structure(;)
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §56.

**Commits** (branch `pcl`, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each, plain and AUTHOR_TESTING=1):

- `89306003` TODO tests: a / after a statement block starts a match
- `c1058d05` Whitespace: a / after a statement block starts a match
