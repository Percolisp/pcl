# PR R -- Tokenizer: a nameless parameter only inside a signature

**The perl rule.** Under the signatures feature a $ before ) , or = is a nameless parameter only inside a signature; elsewhere $) $, $= are the magic variables.

    perl -e 'use v5.36; my $g = $); print defined $g ? "gid\n" : "u\n"'   # gid

**PPI before / after:**

```
BEFORE (master 455bfa73)
use v5.36;\nmy $e = $);
    Word(use) Number::Version(v5.36) Structure(;) Word(my) Symbol($e) Operator(=) Symbol($) Structure()) Structure(;)
use v5.36;\nmy $s = $,;
    Word(use) Number::Version(v5.36) Structure(;) Word(my) Symbol($s) Operator(=) Symbol($) Operator(,) Structure(;)
use v5.36;\nsub f ($, $y) { 1 }
    Word(use) Number::Version(v5.36) Structure(;) Word(sub) Word(f) Structure(() Symbol($) Operator(,) Symbol($y) Structure()) Structure({) Number(1) Structure(})

AFTER (branch pcl)
use v5.36;\nmy $e = $);
    Word(use) Number::Version(v5.36) Structure(;) Word(my) Symbol($e) Operator(=) Magic($)) Structure(;)
use v5.36;\nmy $s = $,;
    Word(use) Number::Version(v5.36) Structure(;) Word(my) Symbol($s) Operator(=) Magic($,) Structure(;)
use v5.36;\nsub f ($, $y) { 1 }
    Word(use) Number::Version(v5.36) Structure(;) Word(sub) Word(f) Structure(() Symbol($) Operator(,) Symbol($y) Structure()) Structure({) Number(1) Structure(})
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §32.

**Commits** (branch `pcl`, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each, plain and AUTHOR_TESTING=1):

- `2e173336` TODO tests: $, and $= under the signatures feature
- `61817c99` Tokenizer: a nameless parameter only inside a signature
