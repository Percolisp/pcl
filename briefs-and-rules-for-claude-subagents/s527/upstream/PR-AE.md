# PR AE -- §39

**The perl rule.** Whitespace may stand between a sigil and its name (toke.c scan_ident skips it): $ bits is $bits.  The Symbol keeps the text; canonical() drops the space.

    perl -e 'my $ bits = 5; print $bits, "\n"'   # 5

**PPI before / after:**

```
BEFORE (master 455bfa73)
my $ bits = 5;
    Word(my) Cast($) Word(bits) Operator(=) Number(5) Structure(;)
$ x = 1; $ s = 2;
    Cast($) Operator(x) Operator(=) Number(1) Structure(;) Cast($) Regexp::Substitute(s = 2;)
$ _ = 2;
    Cast($) Magic(_) Operator(=) Number(2) Structure(;)

AFTER (branch pcl)
my $ bits = 5;
    Word(my) Symbol($ bits) Operator(=) Number(5) Structure(;)
$ x = 1; $ s = 2;
    Symbol($ x) Operator(=) Number(1) Structure(;) Symbol($ s) Operator(=) Number(2) Structure(;)
$ _ = 2;
    Magic($ _) Operator(=) Number(2) Structure(;)
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §39.

**Commits** (branch `pcl`, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each, plain and AUTHOR_TESTING=1): see `git log` of the branch; the group's commits are listed in `vendor/PPI/UPSTREAM` of PCL under letter AE.
