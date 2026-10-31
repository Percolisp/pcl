# PR S -- Word, Unknown: a label only at a statement's start; a declaration's attributes

**The perl rule.** perl reads WORD: as a label only where it expects a statement (toke.c's XSTATE), so (sub :lvalue { 1 }) is an anonymous sub; and the colon after my/our/state + a variable or list introduces attributes.

    perl -e 'my @a = (sub :lvalue { 1 }); print ref $a[0], "\n"'   # CODE

**PPI before / after:**

```
BEFORE (master 455bfa73)
my @a = (sub :lvalue { 1 });
    Word(my) Symbol(@a) Operator(=) Structure(() Label(sub :) Word(lvalue) Structure({) Number(1) Structure(}) Structure()) Structure(;)
my $x : shared = 1;
    Word(my) Symbol($x) Operator(:) Word(shared) Operator(=) Number(1) Structure(;)
our ($p, $q) : unique;
    Word(our) Structure(() Symbol($p) Operator(,) Symbol($q) Structure()) Operator(:) Word(unique) Structure(;)
LINE: for (1) { 1 }
    Label(LINE:) Word(for) Structure(() Number(1) Structure()) Structure({) Number(1) Structure(})

AFTER (branch pcl)
my @a = (sub :lvalue { 1 });
    Word(my) Symbol(@a) Operator(=) Structure(() Word(sub) Operator(:) Attribute(lvalue) Structure({) Number(1) Structure(}) Structure()) Structure(;)
my $x : shared = 1;
    Word(my) Symbol($x) Operator(:) Attribute(shared) Operator(=) Number(1) Structure(;)
our ($p, $q) : unique;
    Word(our) Structure(() Symbol($p) Operator(,) Symbol($q) Structure()) Operator(:) Attribute(unique) Structure(;)
LINE: for (1) { 1 }
    Label(LINE:) Word(for) Structure(() Number(1) Structure()) Structure({) Number(1) Structure(})
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §7 PCL §8.

**Commits** (branch `pcl`, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each, plain and AUTHOR_TESTING=1):

- `a7f3847f` TODO tests: an attribute after a list's anonymous sub or a declaration
- `6a5de5c1` Word, Unknown: a label only at a statement's start; a declaration's attributes

## Second pair (s530): a TYPED declaration

perl's typed lexical puts a class name between the declarator and the variable
-- `my Dog $spot : attr`, `our Dog ($p, $q) : attr` -- and the colon after the
variable still introduces the declaration's attributes (toke.c's `yyl_colon`
with `PL_in_my`).  The first pair stopped at the word before the variable;
`_after_declared_variables` now looks one word further back when that word is
not `my` / `our` / `state`.

```
BEFORE (upstream master 455bfa73, and the first pair)
my A $x : plugh;
    Word(my) Word(A) Symbol($x) Operator(:) Word(plugh) Structure(;)

AFTER (branch pcl)
my A $x : plugh;
    Word(my) Word(A) Symbol($x) Operator(:) Attribute(plugh) Structure(;)
my $y = $c ? print $x : 1;        (control: the colon of a ternary stays an operator)
    ... Word(print) Symbol($x) Operator(:) Number(1) ...
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §8 (three rows, 194-196; row 197 is the control).

- `9d43dfa9` TODO tests: a typed declaration's attribute
- `ded727c3` Tokenizer: a typed declaration's colon introduces attributes

(`ded727c3`'s message lacks the "Fixes PCL §8" sentence, so `tools/vendor-ppi`
lists no `fix:` line for it in `vendor/PPI/UPSTREAM`; it belongs to this group.
When this PR is opened upstream, the two pairs can be squashed into one pair.)
