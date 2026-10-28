# PR E3 -- ~~ after a block's } is two complements (third pair of group E)

**The perl rule.** After the } of a block a term is expected, so ~~ there is two complements, not a smart match.

    perl -e 'sub f { 1 } print ~~5, "\n"'   # 5

**PPI before / after:**

```
BEFORE (master 455bfa73)
sub f { 1 } ~~$y;
    Word(sub) Word(f) Structure({) Number(1) Structure(}) Operator(~~) Symbol($y) Structure(;)

AFTER (branch pcl)
sub f { 1 } ~~$y;
    Word(sub) Word(f) Structure({) Number(1) Structure(}) Operator(~) Operator(~) Symbol($y) Structure(;)
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §21.

**Commits** (branch `pcl`, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each, plain and AUTHOR_TESTING=1):

- `23ec7b86` TODO tests: ~~ after a block's } is two complements
- `4affc7de` Operator: ~~ after a block's } is two complements
