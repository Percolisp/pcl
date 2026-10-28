# PR D2 -- Every punctuation-named array and hash is a magic variable (second and third pairs of group D)

**The perl rule.** perl names an array or hash with any punctuation character (@?, %., @^), as it names a scalar.

    perl -e '@^ = (1,2); print scalar(@^), "\n"'   # 2

**PPI before / after:**

```
BEFORE (master 455bfa73)
@? = (1,2);
    Cast(@) Operator(?) Operator(=) Structure(() Number(1) Operator(,) Number(2) Structure()) Structure(;)
%%. = ();
    Cast(%) Operator(%) Operator(.) Operator(=) Structure(() Structure()) Structure(;)
@^ = (1,2);
    Cast(@) Operator(^) Operator(=) Structure(() Number(1) Operator(,) Number(2) Structure()) Structure(;)

AFTER (branch pcl)
@? = (1,2);
    Magic(@?) Operator(=) Structure(() Number(1) Operator(,) Number(2) Structure()) Structure(;)
%%. = ();
    Magic(%%) Operator(.) Operator(=) Structure(() Structure()) Structure(;)
@^ = (1,2);
    Magic(@^) Operator(=) Structure(() Number(1) Operator(,) Number(2) Structure()) Structure(;)
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §24.

**Commits** (branch `pcl`, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each, plain and AUTHOR_TESTING=1):

- `fb2d6140` TODO tests: every punctuation-named array and hash is one Magic token
- `1dc58208` Singletons: every punctuation-named array and hash is a magic variable
- `76dce24a` TODO test: @^ is one Magic token
- `ecd87059` Singletons: @^ is a magic variable
