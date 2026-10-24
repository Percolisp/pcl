# PR E -- Operator: ~~ where a term is expected is two complements

**The perl rule.** In term position `~~` is two bitwise complements (the numify idiom of perl's own t/op/bop.t).  Only a KNOWN term position splits it: after an operator, an opening bracket, a `;`, the document start, or a builtin from %KEYWORDS that takes an operand (not after `->`); after a match, a method name or a user word it stays the smart match.

    perl -e '$y = "3"; print ~~$y, "\n"'   # 3

**PPI before / after:**

```
BEFORE (master 455bfa73)
is(~~$y, 3)
    Word(is) Structure(() Operator(~~) Symbol($y) Operator(,) Number(3) Structure())
print ~~$y
    Word(print) Operator(~~) Symbol($y)
my $r = (/X/ ~~ @a)
    Word(my) Symbol($r) Operator(=) Structure(() Regexp::Match(/X/) Operator(~~) Symbol(@a) Structure())
    statements: Statement::Variable

AFTER (branch pcl)
is(~~$y, 3)
    Word(is) Structure(() Operator(~) Operator(~) Symbol($y) Operator(,) Number(3) Structure())
print ~~$y
    Word(print) Operator(~) Operator(~) Symbol($y)
my $r = (/X/ ~~ @a)
    Word(my) Symbol($r) Operator(=) Structure(() Regexp::Match(/X/) Operator(~~) Symbol(@a) Structure())
    statements: Statement::Variable
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §21.

**Commits** (branch `pcl` of github.com/Percolisp/PPI, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each):

- `af5345e9` TODO tests: ~~ where a term is expected is two complements
- `5caa125f` Operator: ~~ where a term is expected is two complements
- `d387ad54` TODO tests: ~~ after a builtin list operator is two complements
- `e309445b` Operator: ~~ after a builtin that takes an operand is two complements

**Depends on:** A.  The series applies in order A..I on master (`git am` the patches in this directory: 0001 adds t/ppi_bugs_pcl.t, which every PR's fix commit edits).
