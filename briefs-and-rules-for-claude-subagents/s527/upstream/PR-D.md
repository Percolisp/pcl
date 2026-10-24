# PR D -- Singletons: @? is a magic variable

**The perl rule.** Every punctuation variable name is an array name too; PPI lexes `@?` as a Cast and an Operator.  This PR adds `@?` only; `@!` and `%?` (rows 44 and 45 of t/ppi_bugs_pcl.t) stay TODO.

    perl -e '@? = (1,2); print scalar(@?), "\n"'   # 2

**PPI before / after:**

```
BEFORE (master 455bfa73)
@? = (1,2)
    Cast(@) Operator(?) Operator(=) Structure(() Number(1) Operator(,) Number(2) Structure())

AFTER (branch pcl)
@? = (1,2)
    Magic(@?) Operator(=) Structure(() Number(1) Operator(,) Number(2) Structure())
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §24 (the `@?` row).

**Commits** (branch `pcl` of github.com/Percolisp/PPI, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each):

- `176ab5fe` TODO test: @? is one Magic token
- `6876d6eb` Singletons: @? is a magic variable

**Depends on:** none.  The series applies in order A..I on master (`git am` the patches in this directory: 0001 adds t/ppi_bugs_pcl.t, which every PR's fix commit edits).
