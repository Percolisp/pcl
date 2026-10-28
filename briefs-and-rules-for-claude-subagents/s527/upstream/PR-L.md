# PR L -- Symbol: a * cast trumps the braces in ->symbol

**The perl rule.** *$x{SLOT} is perl's glob-slot syntax *{$x}{SLOT}, whose operand is the scalar $x, so ->symbol must answer $x, not %x.

    perl -e 'our $n = "g"; *{$n} = \10; print ${*$n{SCALAR}}, "\n"'   # 10

**PPI before / after:**

```
BEFORE (master 455bfa73)
my $x = *$a{SCALAR};
    Word(my) Symbol($x) Operator(=) Cast(*) Symbol($a) Structure({) Word(SCALAR) Structure(}) Structure(;)

AFTER (branch pcl)
my $x = *$a{SCALAR};
    Word(my) Symbol($x) Operator(=) Cast(*) Symbol($a) Structure({) Word(SCALAR) Structure(}) Structure(;)
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §27.

**Commits** (branch `pcl`, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each, plain and AUTHOR_TESTING=1):

- `46ba7e81` Document: load YAML::PP only to read PPI_CUSTOM_FEATURE_INCLUDES
- `dc242138` Symbol: a * cast trumps the braces in ->symbol
