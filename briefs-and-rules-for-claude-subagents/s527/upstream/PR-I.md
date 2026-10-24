# PR I -- Number::Hex: a hexadecimal float is one Number::Float

**The perl rule.** perl 5.22 hexadecimal floating point: a `.` or `p` that starts a binary exponent after the hex digits continues the number; `literal()` computes the value.  Without an exponent nothing changes.

    perl -e 'print 0x1.8p+1, "\n"'   # 3

**PPI before / after:**

```
BEFORE (master 455bfa73)
0x1.8p+1
    Number::Hex(0x1) Number::Float(.8) Word(p) Operator(+) Number(1)
0x1.8
    Number::Hex(0x1) Number::Float(.8)

AFTER (branch pcl)
0x1.8p+1
    Number::Float(0x1.8p+1)
0x1.8
    Number::Hex(0x1) Operator(.) Number(8)
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §2.  Reported as #449 (with `$$$ref`, which is not in this PR).

**Commits** (branch `pcl` of github.com/Percolisp/PPI, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each):

- `5be090db` TODO tests: a hexadecimal float is one Number::Float
- `d93bd718` Number::Hex: a hexadecimal float is one Number::Float

**Depends on:** none.  The series applies in order A..I on master (`git am` the patches in this directory: 0001 adds t/ppi_bugs_pcl.t, which every PR's fix commit edits).
