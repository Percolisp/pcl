# Operator Issues Found

Analysis of potential issues with operator handling in the expression parser.

## Fixed Issues ✅

### 1. Unary Minus/Plus ✅ FIXED
```perl
-$y + 5   # Now works
+$x       # Now works
```
**Fix:** Added context-aware detection in the operator loop (PExpr.pm:716-729).
When `-` or `+` appears at start of expression or after another operator, treat as unary with prec 90.

### 2. Bitwise Operators ✅ FIXED
```perl
$x & $y   # Now works - bitwise AND
$x | $y   # Now works - bitwise OR
$x ^ $y   # Now works - bitwise XOR
```
**Fix:** Added to Config.pm precedences:
- `&` at prec 25
- `|` at prec 24
- `^` at prec 24

### 3. `xor` Operator ✅ FIXED
```perl
$x xor $y   # Now works
```
**Fix:** Added `xor` at prec 1 in Config.pm.

**Tests:** `Pl/t/operators-01.t` (41 tests)

### 4. Named Unary Operator Precedence ✅ FIXED
```perl
defined $x && $y   # Now correctly parses as (defined $x) && $y
ref $x eq 'HASH'   # Now correctly parses as (ref $x) eq 'HASH'
```
**Fix:** Added `named_unary` hash to Config.pm with the 5 true named unary operators:
- `defined`, `ref`, `scalar`, `exists`, `delete`

Added `is_named_unary()` method to PExpr.pm. In `handle_subcalls()` (PExpr.pm:1060-1063),
when processing a named unary operator, limit `end_pars` to only consume the next single term.

**Tests:** `Pl/t/named-unary-01.t` (26 tests)

---

## Medium Priority Issues

### ~~5. Range Operator Precedence~~ ✅ FIXED
```perl
$x || 1..10   # Now correctly parses as ($x || 1) .. 10
$a = 1..10    # Now correctly parses as $a = (1..10)
```
**Fix:** Changed `..` and `...` from prec 45 to prec 17 in Config.pm.
Per perlop, `..` should be between `||`/`//` (prec 19) and `?:` (handled specially).

**Tests:** Added to `Pl/t/phase1-01.t`

### ~~6. Comma Operator~~ ✅ Works Correctly
The comma `,` is handled implicitly via `progn` nodes. Tested cases:
- `$x, $y, $z` → progn ✓
- `$x = 1, $y = 2` → progn with assignment subtrees ✓
- `foo($x, $y)` → funcall with separate args ✓
- `$x ? ($a, $b) : ($c, $d)` → ternary with progn children ✓

## Lower Priority / Working Correctly

### Named Unary Operators (File Tests)
The file test operators (`-f`, `-r`, `-e`, etc.) are in the precedence table at prec 52 and seem to work for simple cases.

### Dereference Operators
Cast operators (`$`, `@`, `%`, `&`, `*`) are now handled correctly via special case in `op_info()`.

### Reference Operator
`\` is in precedence table at prec 90 and works.

## Recommended Fix Priority

1. ~~**Unary minus/plus**~~ ✅ Fixed
2. ~~**Bitwise operators**~~ ✅ Fixed
3. ~~**xor operator**~~ ✅ Fixed
4. ~~**Named unary precedence**~~ ✅ Fixed
5. ~~**Range operator precedence**~~ ✅ Fixed

## Perl Precedence Reference (from perlop)

From highest to lowest:
```
terms and list operators (leftward)
->
++ --
**
! ~ \ and unary + and -
=~ !~
* / % x
+ - .
<< >>
named unary operators           <- defined, ref, etc. ✅ FIXED
< > <= >= lt gt le ge
== != <=> eq ne cmp ~~
&                               <- ✅ FIXED
| ^                             <- ✅ FIXED
&&
|| //
..  ...                         <- ✅ FIXED (moved to prec 17)
?:
= += -= *= etc.
, =>
list operators (rightward)
not
and
or xor                          <- ✅ FIXED
```
