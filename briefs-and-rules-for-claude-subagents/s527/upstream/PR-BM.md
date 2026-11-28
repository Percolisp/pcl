# Tokenizer: under the class feature the word after method is its name  (PCL §79; group BM)

**Perl's rule.** Under the class feature `method NAME` declares a method, and perl reads the word after `method` as its name whatever it is spelled like, as after `sub`.

    perl -E 'use feature "class"; no warnings; class C { method x { 7 } method y { 8 } } say C->new->x, C->new->y'   # 78

**Before.** `__current_token_is_forced_word` forces the word after `sub`, `package`, `use` and `no` only, so in `method x { ... }` the `x` was the repetition operator (after a word) and `method y { ... }` / `method s { ... }` read a transliteration and a substitution that swallowed the method body; the statement was not the `Statement::Sub` the class-feature statements give `method NAME`.
**After.** The word after a `method` Word is forced while the tokenizer's feature set has `class` on (not after `->`, where `method` is itself a method name).  Without the feature `method` is an ordinary word and nothing changes.  The feature is on from the line after its `use`: the tokenizer reads a line ahead of the lexer that switches it on, so the test rows put the `use feature` on a line of its own.

**Rows fixed:** PCL §79 (one row naming four methods, x y s q; one control).  TODO tests commit `39279909`, then the fix `eca93377`.  Suite PASS plain + AUTHOR_TESTING.  Found when BL changed the class of the `{` after the mis-lexed `x` in five of perl's own t/class/*.t files.
