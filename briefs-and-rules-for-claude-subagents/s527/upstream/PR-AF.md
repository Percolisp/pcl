# PR AF -- §9

**The perl rule.** Braces around a punctuation name are the variable itself (toke.c scan_ident): @{+} IS @+, as ${^MATCH} is a Magic token.

    perl -e '"ab" =~ /(a)/; my @p = @{+}; print "@p\n"'   # 1 1

**PPI before / after:**

```
BEFORE (master 455bfa73)
my @plus = @{+}; my $x = ${ + };
    Word(my) Symbol(@plus) Operator(=) Cast(@) Structure({) Operator(+) Structure(}) Structure(;) Word(my) Symbol($x) Operator(=) Cast($) Structure({) Operator(+) Structure(}) Structure(;)
$y = $x %{-};
    Symbol($y) Operator(=) Symbol($x) Operator(%) Structure({) Operator(-) Structure(}) Structure(;)

AFTER (branch pcl)
my @plus = @{+}; my $x = ${ + };
    Word(my) Symbol(@plus) Operator(=) Magic(@{+}) Structure(;) Word(my) Symbol($x) Operator(=) Magic(${ + }) Structure(;)
$y = $x %{-};
    Symbol($y) Operator(=) Symbol($x) Operator(%) Structure({) Operator(-) Structure(}) Structure(;)
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §9.

**Commits** (branch `pcl`, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each, plain and AUTHOR_TESTING=1): see `git log` of the branch; the group's commits are listed in `vendor/PPI/UPSTREAM` of PCL under letter AF.
