# PR AG -- §20

**The perl rule.** use experimental 'try' (= feature->import('try')) and the 5.40 bundle (use v5.40) enable try; experimental names only the features it is given.  The experimental change is upstream PR #361's (credited).

    perl -e 'use experimental "try"; no warnings; try { die "e\n" } catch ($e) { print "caught $e" } print "after\n"'

**PPI before / after:**

```
BEFORE (master 455bfa73)
use experimental 'try';\ntry { 1 } catch ($e) { 2 }\nprint 1;\n
    Statement::Include: use experimental 'try'; | Statement: try { 1 } catch ($e) { 2 } print 1;
use v5.40;\ntry { 1 } catch ($e) { 2 }\nprint 1;\n
    Statement::Include: use v5.40; | Statement: try { 1 } catch ($e) { 2 } print 1;

AFTER (branch pcl)
use experimental 'try';\ntry { 1 } catch ($e) { 2 }\nprint 1;\n
    Statement::Include: use experimental 'try'; | Statement::Compound: try { 1 } catch ($e) { 2 } | Statement: print 1;
use v5.40;\ntry { 1 } catch ($e) { 2 }\nprint 1;\n
    Statement::Include: use v5.40; | Statement::Compound: try { 1 } catch ($e) { 2 } | Statement: print 1;
```

**Second pair** (`b91ced6c` TODO tests, `40fed036` the fix): `defer` (5.36) and `class` (5.38) are features `feature_mods` tracks, named by `use feature` / `use experimental` and in no version bundle -- the switch a lexer rule for `defer {` and `class NAME` reads (PCL §52 §53; the Lexer half is a later group).

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §20.

**Commits** (branch `pcl`, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each, plain and AUTHOR_TESTING=1): see `git log` of the branch; the group's commits are listed in `vendor/PPI/UPSTREAM` of PCL under letter AG.
