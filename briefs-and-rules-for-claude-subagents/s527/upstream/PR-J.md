# PR J -- A method name, a postfix ++/-- and a subscript's } end a term

**The perl rule.** After a TERM perl expects an operator (toke.c: PL_expect == XOPERATOR): a method name after ->, a builtin that takes no operand, a postfix ++/--, and the } of a subscript, a dereference block or do/eval's block end a term, so a following * % / . - ~~ is an operator.  A block's } does not end a term; a { after a block's } opens the next block.

    perl -e 'my %h=(a=>5); print $h{a}%-1, "\n"'   # 0

**PPI before / after:**

```
BEFORE (master 455bfa73)
$o->w*w();
    Symbol($o) Operator(->) Word(w) Symbol(*w) Structure(() Structure()) Structure(;)
$i++ / 2;
    Symbol($i) Operator(++) Regexp::Match(/ 2;)
$h{a}%%-1;
    Symbol($h) Structure({) Word(a) Structure(}) Operator(%) Magic(%-) Number(1) Structure(;)
@{$h}*$y;
    Cast(@) Structure({) Symbol($h) Structure(}) Cast(*) Symbol($y) Structure(;)
{ 1 } { 2 } *trit = *scile;
    Structure({) Number(1) Structure(}) Structure({) Number(2) Structure(}) Symbol(*trit) Operator(=) Symbol(*scile) Structure(;)
print time.2;
    Word(print) Word(time) Number::Float(.2) Structure(;)

AFTER (branch pcl)
$o->w*w();
    Symbol($o) Operator(->) Word(w) Operator(*) Word(w) Structure(() Structure()) Structure(;)
$i++ / 2;
    Symbol($i) Operator(++) Operator(/) Number(2) Structure(;)
$h{a}%%-1;
    Symbol($h) Structure({) Word(a) Structure(}) Operator(%) Magic(%-) Number(1) Structure(;)
@{$h}*$y;
    Cast(@) Structure({) Symbol($h) Structure(}) Operator(*) Symbol($y) Structure(;)
{ 1 } { 2 } *trit = *scile;
    Structure({) Number(1) Structure(}) Structure({) Number(2) Structure(}) Symbol(*trit) Operator(=) Symbol(*scile) Structure(;)
print time.2;
    Word(print) Word(time) Operator(.) Number(2) Structure(;)
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §33 PCL §55.

**Commits** (branch `pcl`, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each, plain and AUTHOR_TESTING=1):

- `e29c38cc` TODO tests: a method name, a postfix ++/-- and a subscript's } end a term
- `d8bcdfa1` Tokenizer: a method name, a postfix ++/-- and a subscript's } end a term
- `555ef6af` TODO test: a { after a block's } opens the next block
- `29cb43ae` Tokenizer: a { after a block's } opens the next block
- `df9e06cb` TODO tests: a builtin that takes no operand ends a term
- `487563de` Tokenizer: a builtin that takes no operand ends a term
