# PR N -- Tokenizer: a - after a filehandle or a statement block starts a term

**The perl rule.** In print $fh LIST a - with whitespace before and none after starts the list (toke.c's XTERM rule), and after the } of a block that ends a statement or takes a list a term is expected: print $fh -e $f is a filetest, BEGIN { } -e $f too.

    perl -e 'open my $fh, ">", \my $b; print $fh -1; close $fh; print "[$b]\n"'   # [-1]

**PPI before / after:**

```
BEFORE (master 455bfa73)
print $fh -e $f;
    Word(print) Symbol($fh) Operator(-) Word(e) Symbol($f) Structure(;)
print $fh -1;
    Word(print) Symbol($fh) Operator(-) Number(1) Structure(;)
BEGIN { 1 } -e $f;
    Word(BEGIN) Structure({) Number(1) Structure(}) Operator(-) Word(e) Symbol($f) Structure(;)
sort { $a <=> $b } -1, 2;
    Word(sort) Structure({) Symbol($a) Operator(<=>) Symbol($b) Structure(}) Operator(-) Number(1) Operator(,) Number(2) Structure(;)
print $fh - 1;
    Word(print) Symbol($fh) Operator(-) Number(1) Structure(;)

AFTER (branch pcl)
print $fh -e $f;
    Word(print) Symbol($fh) Operator(-e) Symbol($f) Structure(;)
print $fh -1;
    Word(print) Symbol($fh) Number(-1) Structure(;)
BEGIN { 1 } -e $f;
    Word(BEGIN) Structure({) Number(1) Structure(}) Operator(-e) Symbol($f) Structure(;)
sort { $a <=> $b } -1, 2;
    Word(sort) Structure({) Symbol($a) Operator(<=>) Symbol($b) Structure(}) Number(-1) Operator(,) Number(2) Structure(;)
print $fh - 1;
    Word(print) Symbol($fh) Operator(-) Number(1) Structure(;)
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §22 PCL §35.

**Commits** (branch `pcl`, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each, plain and AUTHOR_TESTING=1):

- `3ddbee5e` TODO tests: a - that starts a term after a filehandle or a block
- `5b822dea` Tokenizer: a - after a filehandle or a statement block starts a term
