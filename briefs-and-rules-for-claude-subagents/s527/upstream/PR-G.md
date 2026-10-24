# PR G -- Unknown: a $ scalar name may start with any letter

**The perl rule.** Under `use utf8` the `@ % * &` arms already take a non-ASCII name; the `$` arm took ASCII only and split the name off as a Word.

    perl -Mutf8 -e '$Ｘ = 1; print $Ｘ, "\n"'   # 1

**PPI before / after:**

```
BEFORE (master 455bfa73)
use utf8; $Ｘ{a} = 1
    Word(use) Word(utf8) Structure(;) Cast($) Word(Ｘ) Structure({) Word(a) Structure(}) Operator(=) Number(1)
    statements: Statement::Include Statement

AFTER (branch pcl)
use utf8; $Ｘ{a} = 1
    Word(use) Word(utf8) Structure(;) Symbol($Ｘ) Structure({) Word(a) Structure(}) Operator(=) Number(1)
    statements: Statement::Include Statement
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §23.

**Commits** (branch `pcl` of github.com/Percolisp/PPI, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each):

- `ba8fd795` TODO tests: a $ scalar with a non-ASCII name is one Symbol
- `c6b17b5e` Unknown: a $ scalar name may start with any letter

**Depends on:** none.  The series applies in order A..I on master (`git am` the patches in this directory: 0001 adds t/ppi_bugs_pcl.t, which every PR's fix commit edits).
