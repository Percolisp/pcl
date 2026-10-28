# PR P -- Unknown: a glob may be named by punctuation, digits or a caret letter

**The perl rule.** perl names a glob with whatever names a scalar: *-, *!, *@, *], *1, *^R are globs wherever a term is expected.

    perl -e '"X"=~/(?<X>X)/; our %X; *X = *-; print keys %X, "\n"'   # X

**PPI before / after:**

```
BEFORE (master 455bfa73)
*X = *-;
    Symbol(*X) Operator(=) Operator(*) Operator(-) Structure(;)
local *a = *1;
    Word(local) Symbol(*a) Operator(=) Operator(*) Number(1) Structure(;)
*^R = *g;
    Operator(*) Operator(^) Word(R) Operator(=) Symbol(*g) Structure(;)
local *@;
    Word(local) Cast(*) Cast(@) Structure(;)
return *!;
    Word(return) Operator(*) Operator(!) Structure(;)
$x = $y *-1;
    Symbol($x) Operator(=) Symbol($y) Operator(*) Number(-1) Structure(;)

AFTER (branch pcl)
*X = *-;
    Symbol(*X) Operator(=) Symbol(*-) Structure(;)
local *a = *1;
    Word(local) Symbol(*a) Operator(=) Symbol(*1) Structure(;)
*^R = *g;
    Symbol(*^R) Operator(=) Symbol(*g) Structure(;)
local *@;
    Word(local) Symbol(*@) Structure(;)
return *!;
    Word(return) Symbol(*!) Structure(;)
$x = $y *-1;
    Symbol($x) Operator(=) Symbol($y) Operator(*) Number(-1) Structure(;)
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §26.

**Commits** (branch `pcl`, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each, plain and AUTHOR_TESTING=1):

- `74ee2b19` TODO tests: a glob named by punctuation or digits, in more positions
- `6100d8b3` Unknown: a glob may be named by punctuation, digits or a caret letter
- `436169bb` TODO test: *: is a glob
- `5ef65f34` Unknown: *: is a glob
