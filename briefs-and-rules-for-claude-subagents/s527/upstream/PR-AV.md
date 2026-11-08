# DashedWord: a negated bareword takes its whole package-qualified name  (PCL §62; group AV)

**Perl's rule.** perl reads a minus, then the whole word after it, package part and all (toke.c `yyl_hyphen`): only a single letter followed by a non-word character is a filetest.  So `-Foo::bar` is unary minus on the bareword `Foo::bar` -- the string `-Foo::bar`, or the negated call when the sub is declared -- and `-CORE::log(...)` negates the builtin.

    perl -e 'print -Foo::bar, "\n"; sub Foo::baz { 3 } print -Foo::baz, " ", -CORE::abs(-2), "\n"'   # -Foo::bar / -3 -2

**Before.** `PPI::Token::DashedWord` read `\w+` only, so `-Foo::bar` was the two Words `-Foo` and `::bar`, and core Math::Complex's `-CORE::exp(...)` / `-CORE::log(...)` came out as `-CORE` followed by `::exp` / `::log`.
**After.** After the filetest test, the dashed word reads the qualified tail with the name pattern `PPI::Token::Word` uses for a bareword: `-Foo::bar`, `-CORE::log`, `-foo::bar::baz`, `-Foo::`, `-a::b` are one Word.  A filetest letter before `::` (`-e::foo`), a spaced minus, a plain dashed word and a subtraction are unchanged (controls).

**Rows fixed:** PCL §62 (six rows; four controls).  TODO tests commit `e0f12cd1`, then the fix `ce2aaa80` (8 lines).  Suite PASS plain + AUTHOR_TESTING.
