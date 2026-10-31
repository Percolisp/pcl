# PR AC -- §46

**The perl rule.** After sub perl reads a name, and ' (deprecated, still valid in 5.40) separates its parts: sub 'Hello'_he_said is Hello::_he_said.

    perl -e 'no warnings; sub '"'"'Hello'"'"'_he_said { "hi" } print Hello::_he_said(), "\n"'   # hi

**PPI before / after:**

```
BEFORE (master 455bfa73)
sub 'Hello'_he_said { "hi" }
    Word(sub) Quote::Single('Hello') Word(_he_said) Structure({) Quote::Double("hi") Structure(})

AFTER (branch pcl)
sub 'Hello'_he_said { "hi" }
    Word(sub) Word('Hello'_he_said) Structure({) Quote::Double("hi") Structure(})
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §46.

**Commits** (branch `pcl`, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each, plain and AUTHOR_TESTING=1): see `git log` of the branch; the group's commits are listed in `vendor/PPI/UPSTREAM` of PCL under letter AC.
