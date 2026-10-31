# PR AB -- §41

**The perl rule.** A run of :: inside a name is part of it (toke.c scan_word6 / scan_ident).

    perl -e 'sub foo::::bar { 7 } print foo::::bar(), "\n"'   # 7

**PPI before / after:**

```
BEFORE (master 455bfa73)
sub foo::::bar { 7 } print foo::::bar();
    Word(sub) Word(foo::) Word(::bar) Structure({) Number(7) Structure(}) Word(print) Word(foo::) Word(::bar) Structure(() Structure()) Structure(;)
$foo::::bar = 3;
    Symbol($foo::) Word(::bar) Operator(=) Number(3) Structure(;)

AFTER (branch pcl)
sub foo::::bar { 7 } print foo::::bar();
    Word(sub) Word(foo::::bar) Structure({) Number(7) Structure(}) Word(print) Word(foo::::bar) Structure(() Structure()) Structure(;)
$foo::::bar = 3;
    Symbol($foo::::bar) Operator(=) Number(3) Structure(;)
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §41.

**Commits** (branch `pcl`, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each, plain and AUTHOR_TESTING=1): see `git log` of the branch; the group's commits are listed in `vendor/PPI/UPSTREAM` of PCL under letter AB.
