# PR A -- Tokenizer: a closing ) or ], and a number of any kind, end a term

**The perl rule.** After a term perl expects an operator, so a `-` there is subtraction. `_opcontext` knew this for `}` and a plain Number only.

    perl -e 'my @x=(5,9); print $x[1]-2, " ", 1.5-0.5, " ", 0x10-1, "\n"'   # 7 1 15

**PPI before / after:**

```
BEFORE (master 455bfa73)
(1)-2
    Structure(() Number(1) Structure()) Number(-2)
$x[1]-2
    Symbol($x) Structure([) Number(1) Structure(]) Number(-2)
1.5-0.5
    Number::Float(1.5) Number::Float(-0.5)
f(1)-length
    Word(f) Structure(() Number(1) Structure()) Word(-length)

AFTER (branch pcl)
(1)-2
    Structure(() Number(1) Structure()) Operator(-) Number(2)
$x[1]-2
    Symbol($x) Structure([) Number(1) Structure(]) Operator(-) Number(2)
1.5-0.5
    Number::Float(1.5) Operator(-) Number::Float(0.5)
f(1)-length
    Word(f) Structure(() Number(1) Structure()) Operator(-) Word(length)
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §15, §25 (the `)-length` half), §34; resolves the existing TODO "(1)-2 not parsed correctly" in t/ppi_token_operator.t (its comment asked for `(1)`, `2` to join the operand table, and they have).

**Commits** (branch `pcl` of github.com/Percolisp/PPI, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each):

- `62f45f31` TODO tests: a - after ], a float or a non-decimal number is subtraction
- `a737c524` Tokenizer: a closing ) or ], and a number of any kind, end a term

**Depends on:** none.  The series applies in order A..I on master (`git am` the patches in this directory: 0001 adds t/ppi_bugs_pcl.t, which every PR's fix commit edits).
