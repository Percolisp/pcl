# PR H -- Word: the _ after sub names the sub

**The perl rule.** A lone `_` is the stat filehandle except right after `sub`, where it names the sub; both paths that class a lone `_` now ask one helper.

    perl -e 'sub _ { "x" } print _(), "\n"'   # x

**PPI before / after:**

```
BEFORE (master 455bfa73)
sub _ { "x" } print 1;
    Word(sub) Magic(_) Structure({) Quote::Double("x") Structure(}) Word(print) Number(1) Structure(;)

AFTER (branch pcl)
sub _ { "x" } print 1;
    Word(sub) Word(_) Structure({) Quote::Double("x") Structure(}) Word(print) Number(1) Structure(;)
    statements: Statement::Sub Statement
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §31.

**Commits** (branch `pcl` of github.com/Percolisp/PPI, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each):

- `a4597353` TODO tests: sub _ is a Statement::Sub named _
- `8cefefce` Word: the _ after sub names the sub

**Depends on:** none.  The series applies in order A..I on master (`git am` the patches in this directory: 0001 adds t/ppi_bugs_pcl.t, which every PR's fix commit edits).
