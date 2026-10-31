# PR AH -- §14 (the after-a-block rows)

**The perl rule.** After the } of a block that opens a statement or follows a word perl expects a term, so a closed <...> is a readline or a glob.

    perl -e 'open my $fh, "<", \"a\nb\n"; my @l = map { $_ } <$fh>; print scalar(@l), "\n"'   # 2

**PPI before / after:**

```
BEFORE (master 455bfa73)
my @l = map { $h{$_}++ } <op/*>;
    Word(my) Symbol(@l) Operator(=) Word(map) Structure({) Symbol($h) Structure({) Magic($_) Structure(}) Operator(++) Structure(}) Operator(<) Word(op) Operator(/) Operator(*) Operator(>) Structure(;)
$x = $h{a} < $b;
    Symbol($x) Operator(=) Symbol($h) Structure({) Word(a) Structure(}) Operator(<) Symbol($b) Structure(;)

AFTER (branch pcl)
my @l = map { $h{$_}++ } <op/*>;
    Word(my) Symbol(@l) Operator(=) Word(map) Structure({) Symbol($h) Structure({) Magic($_) Structure(}) Operator(++) Structure(}) QuoteLike::Readline(<op/*>) Structure(;)
$x = $h{a} < $b;
    Symbol($x) Operator(=) Symbol($h) Structure({) Word(a) Structure(}) Operator(<) Symbol($b) Structure(;)
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §14 (the after-a-block rows).

**Commits** (branch `pcl`, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each, plain and AUTHOR_TESTING=1): see `git log` of the branch; the group's commits are listed in `vendor/PPI/UPSTREAM` of PCL under letter AH.
