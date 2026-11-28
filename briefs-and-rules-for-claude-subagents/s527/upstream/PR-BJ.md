# Tokenizer: <...> after use Module is the import list  (PCL §76; group BJ)

**Perl's rule.** After the module name of a `use` or `no` statement, and after the version that may follow it, perl expects a term, the import list (toke.c `S_tokenize_use` sets `PL_expect = XTERM` before it forces the name and the version), so a `<...>` there is a glob: a word list written as a glob is the list of names.

    perl -e 'use List::Util <max sum>; print max(3, 9), sum(1, 2), "\n"'   # 93

**Before.** `use Mod <a b>;` was `Word(use) Word(Mod) Operator(<) Word(a) Word(b) Operator(>) Structure(;)` -- the same for `use Mod<a b>`, `no Mod <a b>`, `use Mod::Sub <a b>`, `use Mod 1.2 <a b>`, and `use lib <./x ./y>` (where the `/` then started a match).
**After.** `Word(use) Word(Mod) QuoteLike::Readline(<a b>) Structure(;)`.  `Tokenizer::_term_expected` asks a new `_after_use_module` (the significant tokens before the `<` are a word and a `use` / `no` that opens its statement -- nothing, a `;`, a brace or a label before it -- with an optional Number, the version, after the word); the `<` arm of `Token/Whitespace.pm` asks `_term_expected` after a Number too, which a version is (anywhere else a number still ends a term).  `Statement/Include.pm`'s feature tracking reads such an argument: `use feature <say try>` names `say` and `try` as `qw<say try>` does (a `<...>` holding only a filehandle name, `<try>`, names nothing -- perl reads it as a readline; one holding a wildcard keeps the old "possibly unrecognized feature" warning).

**Rows fixed:** PCL §76 (seven rows; six controls: the statement after a use, a version alone, `qw<>`, a list in parentheses, a less-than after a number, the feature list as a glob).  TODO tests commit `d5e6120d`, then the fix `d20ff42c`.  Suite PASS plain + AUTHOR_TESTING.
