# PR F -- Operator: .digit after a term is concatenation

**The perl rule.** `.5` starts a number only where a term is expected; after a term it is concatenation -- and after a scalar print filehandle (`print $fh .5`) it is the number again.  The second pair moves the scalar-filehandle test of PR B into the Tokenizer so both arms share it.

    perl -e '$_ = 1; print $_.2, "\n"'   # 12

**PPI before / after:**

```
BEFORE (master 455bfa73)
$_.2
    Magic($_) Number::Float(.2)
print $fh .5
    Word(print) Symbol($fh) Number::Float(.5)
print $fh.5
    Word(print) Symbol($fh) Number::Float(.5)

AFTER (branch pcl)
$_.2
    Magic($_) Operator(.) Number(2)
print $fh .5
    Word(print) Symbol($fh) Number::Float(.5)
print $fh.5
    Word(print) Symbol($fh) Operator(.) Number(5)
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §28.

**Commits** (branch `pcl` of github.com/Percolisp/PPI, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each):

- `a268a29b` TODO tests: .digit after a term is concatenation
- `a39e723d` Operator: .digit after a term is concatenation
- `d26d7ce4` TODO test: .digit after a print filehandle is a number
- `8955ba30` Tokenizer: one scalar-filehandle test; .digit after print $fh is a number

**Depends on:** A, B.  The series applies in order A..I on master (`git am` the patches in this directory: 0001 adds t/ppi_bugs_pcl.t, which every PR's fix commit edits).
