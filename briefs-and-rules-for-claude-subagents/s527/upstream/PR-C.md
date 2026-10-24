# PR C -- Singletons: add ^^, perl 5.40's logical exclusive or

**The perl rule.** perl 5.40 added `^^` (there is no `^^=`).  Two loops of t/ppi_token_operator.t enumerate %OPERATOR, so its plan grows by 8.

    perl -e 'print 1 ^^ 0, "\n"'   # 1

**PPI before / after:**

```
BEFORE (master 455bfa73)
$a ^^ $b
    Symbol($a) Operator(^) Operator(^) Symbol($b)

AFTER (branch pcl)
$a ^^ $b
    Symbol($a) Operator(^^) Symbol($b)
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §16.

**Commits** (branch `pcl` of github.com/Percolisp/PPI, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each):

- `5060abc9` TODO test: ^^ is one operator
- `21394cc3` Singletons: add ^^, perl 5.40's logical exclusive or

**Depends on:** none.  The series applies in order A..I on master (`git am` the patches in this directory: 0001 adds t/ppi_bugs_pcl.t, which every PR's fix commit edits).
