# PPI Glob/Diamond Operator Disambiguation Bug

> **RESOLVED (session 253b): FIXED upstream in PPI 1.291.** The chains below now
> tokenize correctly as comparisons, and PCL transpiles them fine. Kept as a
> historical record. Regression guard lives in `Pl/t/misc-fixes-02.t` ("chained
> < > comparison is not misparsed as a glob/readline"); see also
> `docs/ppi-upstream-bugs.md` §3. No PCL workaround is needed any longer.

## Problem

PPI (the Perl parser used by PCL) misinterprets certain comparison chains as
glob or diamond-operator tokens. Specifically, when an expression contains
`< EXPR >` where the outer `<` and `>` are comparison operators but PPI sees
them as balanced angle brackets, the entire token sequence is misclassified.

### Reproducing Example

```perl
my @data = ([5,3,2], [5,5,4]);
foreach my $item (@data) {
    my $r = $item->[0] < $item->[1] > $item->[2];
    print "got=[$r]\n";
}
```

Or in cmpchain.t, tests like:

```perl
is join(",", "x", $_->[0] < $_->[1] > $_->[2], "y"), ...
```

### Symptom

When PCL transpiles such code, the generated CL looks like:

```lisp
;; is join(",", "x", $_->[0] < $_->[1] > $_->[2], "y"), ...
;; PARSE ERROR: Bug. Fell through. Missing case: [
```

The entire `(pl-is ...)` call is **dropped** from the generated CL. The
test loads fine (no CL error) but those tests simply don't run — silent failure.

## Root Cause (CORRECTED — 2026-02-21)

This is NOT a PPI tokenization bug. PPI correctly parses `<` and `>` as
separate operators. The bug is in PCL's own `_fix_ppi_glob_after_block`
function in `Pl/PExpr.pm`.

`_fix_ppi_glob_after_block` is a PPI bug workaround for the case where after
a block `{ }`, PPI gives `<*.txt>` as separate tokens instead of a glob. The
function scans for `<` … `>` and checks for "glob metacharacters" (`*?[]`).

**The bug**: the check `$has_glob_chars = 1 if $c =~ /[\*\?\[\]]/` uses the
`content()` of every token between `<` and `>`. A `PPI::Structure::Subscript`
with content `[1]` contains `[` and `]`, which match the glob char class. So
`$a < $b->[1] > $c` triggers the "found a glob" heuristic, and PCL creates a
bogus `PPI::Token::QuoteLike::Readline` with content `<$b->[1]>`.

**Two compound errors**:
1. `has_glob_chars` was set from `PPI::Structure::Subscript` content (e.g.,
   `[1]` matches `[\*\?\[\]]`)
2. No bail-out when `->` appears between `<` and `>` (which can't appear in
   a real glob pattern)

## Impact

This affects **any comparison chain** of the form `A < B > C` where B is a
complex expression (array ref subscript, hash access, method call, etc.).
Simple scalars like `$a < $b > $c` may or may not trigger it depending on
PPI's heuristic.

In cmpchain.t, many tests exercise `<`/`>` mixed chains:
- `$_->[0] < $_->[1] > $_->[2]`  (triggers bug)
- Other combinations with subscripts

This is a significant source of the regression from 187→139 passes in cmpchain.t
after the N-term chain fix (session 41).

## Affected Tests

In `cmpchain.t` (perl-tests/), tests involving mixed `<` and `>` comparisons
with complex LHS/RHS expressions (especially `$_->[N]` subscripts).

## Fix (Applied 2026-02-21)

Fixed in `Pl/PExpr.pm` `_fix_ppi_glob_after_block` function, two changes:

1. **Don't count structure content as glob chars**: changed
   ```perl
   $has_glob_chars = 1 if $c =~ /[\*\?\[\]]/;
   ```
   to
   ```perl
   $has_glob_chars = 1
       if ref($t) !~ /^PPI::Structure/
       && $c =~ /[\*\?\[\]]/;
   ```

2. **Bail out on `->` operator**: added
   ```perl
   last if ref($t) eq 'PPI::Token::Operator' && $c eq '->'; # $ref->[n] not a glob
   ```

## Result

`$a < $b->[1] > $c` now correctly transpiles as
`(pl-chain-cmp (pl-aref-deref $a ...) '< (pl-aref-deref $b ...) '> ...)`.

cmpchain.t: 139 → 142 passes after fix (was also limited by other issues).
PCL suite: 2402 tests, all still passing.

## Related

- Session 41: N-term chain parsing fix + this bug fix
- cmpchain.t regression: 187→139 (session 41 N-term fix) → 142 (this fix)
- The `_fix_ppi_glob_after_block` function was originally added as a workaround
  for PPI parsing `<*.txt>` as separate tokens after a block `{ }`.

## Status

**FIXED 2026-02-21.**
