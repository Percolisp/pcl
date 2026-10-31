# PR AD -- §45

**The perl rule.** Where a term is expected & is the code sigil, whitespace or not (toke.c yyl_ampersand: not XOPERATOR => scan_ident).

    perl -e '& {sub { print "ok\n" }};'   # ok

**PPI before / after:**

```
BEFORE (master 455bfa73)
& {sub { print "ok\n" }};
    Operator(&) Structure({) Word(sub) Structure({) Word(print) Quote::Double("ok\n") Structure(}) Structure(}) Structure(;)
my $r = & $code(1);
    Word(my) Symbol($r) Operator(=) Operator(&) Symbol($code) Structure(() Number(1) Structure()) Structure(;)
$x = $a & $b;
    Symbol($x) Operator(=) Symbol($a) Operator(&) Symbol($b) Structure(;)

AFTER (branch pcl)
& {sub { print "ok\n" }};
    Cast(&) Structure({) Word(sub) Structure({) Word(print) Quote::Double("ok\n") Structure(}) Structure(}) Structure(;)
my $r = & $code(1);
    Word(my) Symbol($r) Operator(=) Cast(&) Symbol($code) Structure(() Number(1) Structure()) Structure(;)
$x = $a & $b;
    Symbol($x) Operator(=) Symbol($a) Operator(&) Symbol($b) Structure(;)
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §45.

**Commits** (branch `pcl`, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each, plain and AUTHOR_TESTING=1): see `git log` of the branch; the group's commits are listed in `vendor/PPI/UPSTREAM` of PCL under letter AD.
