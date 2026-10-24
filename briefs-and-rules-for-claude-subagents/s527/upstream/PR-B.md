# PR B -- Unknown: % and * right after a term are operators

**The perl rule.** A `%` or `*` right after a term is modulo or multiplication whatever follows it -- except after a scalar print filehandle (`print $fh %h`), which perl decides from whitespace before the sigil and none after it.

    perl -e 'print 7%-3, "\n"'   # -2

**PPI before / after:**

```
BEFORE (master 455bfa73)
7%-3
    Number(7) Magic(%-) Number(3)
length($k)*length($k)
    Word(length) Structure(() Symbol($k) Structure()) Symbol(*length) Structure(() Symbol($k) Structure())
print $fh %h
    Word(print) Symbol($fh) Symbol(%h)

AFTER (branch pcl)
7%-3
    Number(7) Operator(%) Number(-3)
length($k)*length($k)
    Word(length) Structure(() Symbol($k) Structure()) Operator(*) Word(length) Structure(() Symbol($k) Structure())
print $fh %h
    Word(print) Symbol($fh) Symbol(%h)
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §4 and §12.  Reported as #451 (`7%-3`).

**Commits** (branch `pcl` of github.com/Percolisp/PPI, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each):

- `fe5aeb64` TODO tests: % and * right after a term are operators
- `1cff9293` Unknown: % and * right after a term are operators

**Depends on:** A (the term test reads `_opcontext`, which A widens).  The series applies in order A..I on master (`git am` the patches in this directory: 0001 adds t/ppi_bugs_pcl.t, which every PR's fix commit edits).
