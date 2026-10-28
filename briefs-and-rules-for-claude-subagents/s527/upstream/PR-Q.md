# PR Q -- Unknown: $$ before a $, a word or a { is a dereference cast

**The perl rule.** perl reads $$$a as ${${$a}} and $${$r} as ${${$r}}; $$ alone stays the process id.  This is upstream PR #428 (toddr-bot, issue #60), taken as it stands and widened to $${EXPR}.

    perl -e 'my $v=7; my $r=\\$v; print $$$r, "\n"'   # 7

**PPI before / after:**

```
BEFORE (master 455bfa73)
$$$a = 3;
    Magic($$) Symbol($a) Operator(=) Number(3) Structure(;)
$${$ref};
    Magic($$) Structure({) Symbol($ref) Structure(}) Structure(;)
$$ + 1;
    Magic($$) Operator(+) Number(1) Structure(;)

AFTER (branch pcl)
$$$a = 3;
    Cast($) Cast($) Symbol($a) Operator(=) Number(3) Structure(;)
$${$ref};
    Cast($) Cast($) Structure({) Symbol($ref) Structure(}) Structure(;)
$$ + 1;
    Magic($$) Operator(+) Number(1) Structure(;)
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §1.

**Commits** (branch `pcl`, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each, plain and AUTHOR_TESTING=1):

- `59e0c3b9` TODO tests: $$$a is two dereferences of $a
- `eb16e9d2` Unknown: $$ before a $, a word or a { is a dereference cast
