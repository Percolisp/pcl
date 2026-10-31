# Lexer: an anonymous hash where perl expects a term  (PCL §50; group AK)

**Perl's rule.** After die, warn, exists and delete perl expects a term (toke.c XTERM) and `{` is an anonymous hash.  After map and grep perl looks ahead (toke.c yyl_leftcurly): an immediate `}`, or a word / number / string followed by `=>` -- or by a comma unless it is a word starting with a lower-case letter -- opens an anonymous hash; anything else is the block.

    perl -e 'eval { die { code => 42 } }; print $@->{code}, "\n"'          # 42
    perl -e 'my @r = map {a => $_}, 1, 2; print scalar(@r), ref $r[0]'    # 2HASH
    perl -e 'my @t = map { $_ => 1 } 1, 2; print scalar(@t)'              # 4

**Before.** `_curly` answered Block for every `{` after an element it had no rule for (its own FIXME), and `map`/`grep` were Block unconditionally.
**After.** die/warn/exists/delete in `%CURLY_CLASSES` (Constructor); `_curly_hash_lookahead` after map/grep, with and without parentheses; sort/eval/do keep their block.

**Rows fixed:** PCL §50.  TODO tests commit, then the fix (55 lines).  Suite PASS plain + AUTHOR_TESTING.
