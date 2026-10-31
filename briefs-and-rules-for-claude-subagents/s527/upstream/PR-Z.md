# PR Z -- §47

**The perl rule.** A label stands only where a statement starts, so `WORD :` inside a ternary's middle operand is a word and the colon (the rule of PR S; this group is its regression row).

    perl -e 'sub f { my $c = 0 ? ref shift : shift; $c } print f(5), "\n"'   # 5

**PPI before / after:**

```
BEFORE (master 455bfa73)
my $c = 0 ? ref shift : shift;
    Word(my) Symbol($c) Operator(=) Number(0) Operator(?) Word(ref) Label(shift :) Word(shift) Structure(;)

AFTER (branch pcl)
my $c = 0 ? ref shift : shift;
    Word(my) Symbol($c) Operator(=) Number(0) Operator(?) Word(ref) Word(shift) Operator(:) Word(shift) Structure(;)
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §47.

**Commits** (branch `pcl`, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each, plain and AUTHOR_TESTING=1): see `git log` of the branch; the group's commits are listed in `vendor/PPI/UPSTREAM` of PCL under letter Z.
