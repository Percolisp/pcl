# Perl Test Files - Priority Order

Tests from `perl-5.40.3/perl-5.40.3/t/op/`

## Tier 1 - Simple, likely to pass
These use basic features already tested in our simplified tests.

1. repeat.t - string/list `x` operator
2. ord.t - ord/chr functions
3. pos.t - pos() function
4. study.t - study() (probably no-op)
5. reset.t - reset() function
6. vec.t - vec() function

## Tier 2 - Core operations
These test core features that should mostly work.

7. range.t - `..` operator (needs Config stub)
8. wantarray.t - wantarray function
9. each.t - each/keys/values on hashes
10. push.t - push/splice
11. unshift.t - unshift
12. sprintf.t - sprintf formatting

## Tier 3 - More complex
These have more dependencies or complex features.

13. bless.t - OO blessing
14. ref.t - references
15. tie.t - tied variables (won't work)
16. local.t - local variables
17. caller.t - caller() function
18. goto.t - goto (won't work)

## Tier 4 - Advanced/Unicode
These need special handling or won't work yet.

19. quotemeta.t - needs locale tools
20. lc.t - Unicode case folding
21. pack.t - pack/unpack
22. regex*.t - regex tests

## Notes

- Files requiring `charset_tools.pl` or `loc_tools.pl` need stubs
- Files using `Config` module need a stub
- Files with `use feature` may need handling
- Skip tests using `tie`, `goto`, `format`
