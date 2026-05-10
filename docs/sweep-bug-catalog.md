# PCL Sweep Bug Catalog

Generated 2026-05-07. Baseline: 18209 pass / 10159 fail across 100 files, 40 fully passing.
Skips sprintf.t (deprioritized) and Unicode/utf8-encode issues (documented not-supported).

---

## Cross-cutting bugs (high value targets)

### 1. `state ++$var` / `state $var++` — parser drops `state` or the `++`

**Files:** state.t (tests 77–82, ~6 failures)

**Root cause (in Parser.pm):**
- `++ state $y` at top level: the `state` keyword is stripped; `$y` becomes a global `defvar`.
  Generated: `(p-pre++ $y)` with no init guard.
- `state $z ++`: generates the `unless $init` guard but never emits `(p-post++ $state__...__z)`.
  The post-increment is simply missing from output.

**Fix area:** `_process_statement` / `_process_expression_statement` — how `state` is handled
when combined with a prefix/postfix `++`/`--` operator.

---

### 2. `scalar(%hash)` returns hash-table debug string instead of key count ✅ FIXED (session 175)

**Files:** each.t (tests 47, 53) — fixed. hashassign.t (test 209) — separate issue (group 3).

**Root cause:** In Perl 5.26+, `scalar(%hash)` returns the number of keys (integer).

**Fix:** Three changes in `pcl-runtime.lisp`:
- `box-set`: when storing a raw hash-table (not wrapped in p-box) to a scalar box, converts to key count (mirrors existing array→length logic). `p-ensure-hashref` updated to wrap new hash in `make-p-box` to prevent conversion during autovivification.
- `p-scalar`: added hash-table case returning `(hash-table-count v)` when val is not a p-box.
- `to-number`: added hash-table case returning `(hash-table-count val)`.

hashassign.t test 209 `scalar(%h = list)` is a different issue — the assignment return value, not `scalar(%hash)` itself (see group 3).

---

### 3. `%hash = (...)` in list context doesn't return the list

**Files:** hashassign.t (tests 207–208, 211–214, and more — ~13 failures)

**Root cause:** `(join ':', %h = (1) x 8)` — the `%h = (...)` assignment in list context
should return the flattened key-value list. PCL returns the hash ref string instead.
In scalar context (test 209) it should return the count of elements assigned.

**Fix area:** `p-hash-=` or the assignment operator result in different contexts.
The hash assignment needs to return `(coerce (hash-table-contents ht) 'list)` in list ctx
and the element count in scalar ctx.

---

### 4. `substr` out-of-bounds: no "substr outside of string" warning or error ✅ FIXED (session 174)

**Files:** substr.t — ~38 OOB failures fixed; now 357/397 passing.

**Fix:** `p-substr` in `pcl-runtime.lisp` has `oob` bounds check (lines ~1382-1404):
- OOB reads: `(p-warn "substr outside of string\n")`
- OOB writes: `(error "substr outside of string")`
Remaining failures (tests 313-397) are lvalue substr and \substr — documented not-supported.

---

### 5. `\(list_expr)` takes ref to ARRAY instead of SCALAR in some contexts

**Files:** bless.t (test 11, 105, ~3 failures), ref.t

**Root cause:** `bless \(map "$_", "test"), "C"` — `map` returns a list; `\(list)` in
scalar context should take a ref to the last scalar element, giving a SCALAR ref.
PCL is treating `map` as returning an array-ref and `\` on that gives ARRAY ref.

**Fix area:** ExprToCL.pm or PExpr.pm — `\` operator applied to a list expression.
When `\(expr)` is in scalar context and `expr` is a list, it should dereference to the
last element and take `\$last_elem`.

---

### 6. `p-/` produces CL rational `1/4` instead of float `0.25` ✅ FIXED (session 172)

**Files:** hexfp.t (tests 42–46, ~5 failures)

**Root cause:** `1 / (1 << 2)` — CL's `(/ 1 4)` returns the ratio `1/4`. `to-string`
of a CL ratio prints `"1/4"` instead of `"0.25"`.

**Fix:** `p-/` in `pcl-runtime.lisp` wraps result with `(if (rationalp r) (coerce r 'double-float) r)`.
hexfp.t still fails entirely (PPI can't parse `0x1p-2` hex float literals) — documented not-supported.

---

### 7. `infnan.t` — wrong error format for `chr(Inf)` / `chr(NaN)`

**Files:** infnan.t (~6 failures)

**Root cause:** PCL raises:
`"Can't decode NaN or infinity: #.SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY."`
Perl expects: `"Cannot chr X..."` pattern.

**Fix area:** `p-chr` in `pcl-runtime.lisp` — change the error message to match
Perl's "Cannot chr" wording.

---

### 8. Dynamic loop labels: `last $var` / `next $var` not supported

**Files:** loopctl.t (tests 62–64, 3 failures)

**Root cause:** `eval { last $label }` where `$label` is a runtime string variable.
PCL only handles literal label names. `p-last` / `p-next` / `p-redo` don't support
a variable label argument.

**Fix area:** ExprToCL.pm — when `last LABEL` / `next LABEL` / `redo LABEL` has
a non-literal label expression, emit something like `(p-last-dynamic label-value)`.
Runtime: walk up the block structure to find the right catch tag (hard; alternative:
just throw a named-catch with the label string and catch it in the right loop).

---

### 9. `join.t` — `join(undef, list)` wrong length / no warning

**Files:** join.t (tests 9–10, 18)

**Root cause (two separate bugs):**
- Tests 9–10 (**not fixable without lazy eval**): separator is `':'`, but list elements include `undef` which trigger `$SIG{__WARN__}` that increments `$s`. The modified `$s` should be read lazily on subsequent elements. CL evaluates all args before calling `p-join`, so `$s` is already bound at call time. Would need thunk-based lazy args.
- Test 18 (**FIXED, session 175**): `join(undef, ())` with empty list should warn. Fix: `p-join` now checks for undef sep before item-count check, skipping the check for tied boxes to avoid premature FETCH.

**Status:** test 18 passes; tests 9–10 are not fixable (lazy eval limitation).

---

### 10. `each.t` — `each` and `keys` use different iteration orders

**Files:** each.t (test 3 — 1 failure, but probably bigger latent issue)

**Root cause:** The test does `@keys = keys %h` then iterates with `each %h`, expecting
the same order. In Perl they share the internal bucket iterator. In PCL, `keys` and
`each` may iterate CL hash-tables in different orders (CL doesn't guarantee order).

**Fix area:** `p-keys` / `p-each` in `pcl-runtime.lisp` — both should use the same
underlying iteration order. One approach: `p-keys` should use the same index-based
iterator that `p-each` uses, so their orders agree.

---

## Per-file issues (smaller scope)

### sort.t (51 failures)

- **Inplace sort** (tests 66–70): `@a = sort @a` expected `'a-a-a b c'` but got
  `'$SCALAR(0x...)-$SCALAR(0x...)-a b c'`. The `$a` and `$b` comparator variables
  are holding boxes instead of their string values during the sort. Bug in how `$a`/`$b`
  are set up or unboxed during sort.
- **Wantarray in sort** (tests 56–62): `wantarray` inside sort comparator should
  return false (sort calls comparator in scalar ctx) but PCL returns list ctx.
- **Error message** (test 22): "CORE::revers" vs "Undefined sort subroutine" — not supported.
- **Overloaded objects** (tests ~88–90): sort with `use overload '<=>'` not working.
- **UTF-8/locale** (test 3): locale-aware sort (not supported).

### ref.t (59 failures)

- **NUL in symbolic refs** (tests 87–113): `${"foo\0bar"}` — symbolic ref with NUL
  in name. In Perl, NUL terminates the package lookup. PCL doesn't model this.
- **DESTROY on array/closure** (tests 63–64): DESTROY called when blessed object
  leaves scope — PCL GC doesn't call DESTROY (documented).
- **`&{""}` call** (test 21): calling code ref with empty string name → should be
  "SCALAR" but got different ref type.
- **Weak ref** (test 38–39): `ref()` and stringify for ref-to-undef.
- **UTF8 representation** (test 83): `length` of UTF-8 string (use bytes context).

### method.t (47 failures)

- **`&{1}()` call** (tests 5–9): `&$one()` where `$one=1` — PCL raises "Not a CODE
  reference" instead of "Undefined subroutine &main::1" and fails to dispatch via
  `local *1 = sub{...}`.
- **`local @ISA` method cache** (tests 49–57): after `local @ISA = qw(C)`, method
  lookup still uses old ISA. The method dispatch cache isn't invalidated on `local @ISA`.
- **`UNIVERSAL::AUTOLOAD`** (tests 97–99): `$AUTOLOAD` set to `Foo::ARRAY` instead of
  `Foo::Bar` when AUTOLOAD is in UNIVERSAL. Variable scoping for `$AUTOLOAD` across
  packages.

### state.t (64 failures)

- **`++ state $y` / `state $z ++`**: see bug #1 above.
- **`goto state $label`** (tests 70–73): computed goto with state var as label —
  not implemented.
- **`state $x` in map/grep** (tests 74–75): state vars inside map/grep blocks don't
  preserve value between map iterations.
- **lvalue `\substr` as state** (tests 83+): `state $c = \substr $s, $i, 1` — lvalue
  ref to substr (documented not-supported).

### local.t (24 failures)

- **Array size after `local($a[5])`** (tests 119–120): after `local($a[5]) = 'z'` and
  `$a[4] = 'y'` in a block, when block exits `$a[5]` is restored (deleted) but `$a[4]`
  is also being lost. The local-restore is incorrectly trimming the array.
- **`local` sub via stash** (tests 271–278): `local *{$pkg}{method}` syntax for
  temporarily replacing a method via stash slice — not supported.
- **`local $_` with filetest/match** (tests 255–264): `local $_` interactions with
  filetest operators and pattern matching on default `$_`.

### each.t (22 failures)

- **`each`/`keys`/`values` with no args** (tests 40–42): PCL says "invalid number of
  arguments: 0"; Perl says "Not enough arguments for each" — error message mismatch.
- **`keys %h = N`** (tests 5, 8, 14–20): `keys %h = 10` sets pre-allocated bucket count
  — not implemented in PCL.
- **Iterator order mismatch** (test 3): see bug #10 above.

### aassign.t (89 failures)

Most failures trace to:
- **List-context function return** (test 1): `($a,$b) = f_ret_14()` where `f_ret_14`
  returns `1..4`. Gets `(:)` meaning empty. Root cause: the wantarray/VOID_CTX regression
  from sessions 162–163 — sub body wrapped with void ctx, return value lost.
- **Array alias assignments** (tests 12–16): `my $a = $pkg_var` where the assignment
  is supposed to create an alias. Various alias forms.
- **Tied variable assignments** (tests tied-*): tie interactions with list assignment.

### do.t (20 failures)

- **`return do { }` in scalar context** (tests 17–38): `return do { @a }` in scalar
  context returns the full array value (e.g., sum 7+8+...= 15) instead of scalar count.
  Context not propagated through `do {}` block to `return`.

### substr.t (105 failures)

- **Out-of-bounds warning/error**: see bug #4 above. Accounts for ~38 failures.
- **Other failures** (tests 38–48 and beyond): likely lvalue substr, scalar-lvalue
  distinction, and ref-to-substr forms (documented not-supported).

### bless.t (23 failures)

- **`\(map ...)`** (tests 11, 105): see bug #5 above.
- **`\substr` lvalue ref** (tests 26–28): documented not-supported.
- **POSIX errno values** (tests 77–78): `POSIX::EINVAL` gives wrong errno string
  ("Operation not permitted" vs "Invalid argument") — POSIX errno stubs wrong.
- **Bless-into-ref detection** (test 101): `bless $obj, $ref_ref` should die "Attempt
  to bless into a reference" but PCL doesn't validate the class argument is a string.

### loopctl.t (8 failures)

- **Dynamic labels** (tests 62–64): see bug #8 above.
- **Reverse with sparse array** (test 47): `reverse` on array with empty slots gives
  wrong result (leading spaces).
- **`$_` in continue block** (tests 49–53): `$_` set in `continue` block is not
  propagating back to the test after the loop.

### for.t (7 failures)

- **Invalid Perl error detection** (tests 131–138): `for CORE::my Dog $spot (...)` —
  PCL doesn't reject invalid declarator forms. Per principle 9, these tests should
  be commented out (needs user approval).

### grep.t (6 failures)

- **DESTROY in grep** (tests 69–76): blessed objects not destroyed when `@a = ()` — GC issue.
- **Invalid grep syntax** (test 61): `grep $var, @list` — PCL doesn't detect error.

### pos.t (18 failures)

- **`pos(*glob)`** (test 12): pos() on a glob — not supported.
- **pos through defelem** (tests 14–20): passing pos through default element (`$_[0]`
  aliasing) — `@_` aliasing is documented not-supported.

### qr.t (20 failures)

- **`ref(\$qr)` returns SCALAR not REGEXP** (tests ~5–10): A compiled regex ref should
  have `ref()` return `"REGEXP"`. PCL wraps qr// in a box, so `ref(\$box)` = SCALAR.
- **`$a + 0 == $b + 0`** (test 3): two separate `qr//` objects stringify to the same
  address-like number — coercion of a regex object to a number gives same result.

### readline.t (17 failures)

- **Read-only `<STDIN>` glob** (test 1): writing to `STDIN` filehandle should be
  "Modification of a read-only value" — PCL doesn't enforce read-only on standard filehandles.
- **`rcatline`** (tests 4–7): `$a .= <FH>` where the FH has been opened to a specific
  string/array — something about append-readline not working.
- **SIGALRM / interrupted readline** (tests 16, 18): `readline` interrupted by signal —
  not implemented.

### reset.t (20 failures)

- **`?pat?` one-match regex** (all failures): documented not-supported (removed in Perl 5.38).

### auto.t (2 failures)

- **`$x-- on a glob copy`** (tests 45, 47): `$x = *foo` (glob assigned to scalar), then
  `$x--` should give `-1`. PCL `p-post--` / `p-pre--` on a glob value produces a huge
  number (address) instead of numeric decrement.

### my.t (7 failures)

- **`my $x if 0` error** (tests 53–59): `eval "my \$x if 0"` should raise "This use of
  my() in false conditional is no longer allowed". Per principle 9 (invalid Perl), these
  should be commented out (needs user approval).

### vec.t (8 failures)

- **Error on code point > 0xFF** (tests ~3): error message format mismatch.
- **Lvalue vec** (test ~1): `vec($s, $i, 8) = $val` — lvalue form.

### splice.t (10 failures)

- **`j(1..12)` evaluates as flip-flop** (tests 2,4,6,8,10,12): `j(1..12)` inside `is()`
  — the `..` range is being evaluated in scalar context (flip-flop) instead of list context.
  Argument to function call not getting LIST_CTX. Root: broader context-propagation issue.

### join.t (6 failures)

- See bug #9 above for tests 9–10, 18.
- **Test 29**: `join(const, const)` — consecutive calls shouldn't return the same scalar
  (ref identity check). PCL likely interning or caching the result.

### flip.t (2 failures)

- **Test 10**: Unknown (no description, need to investigate).
- **Test 12**: `\scalar($a..$b)` giving same scalar address each time — consecutive
  `\scalar(...)` calls should give distinct refs.

### wantarray.t (1 failure)

- **Test 11**: Returns `'S'` (scalar) instead of `'V'` (void) in some context. Wantarray
  void-context detection.

### unshift.t / push.t / delete.t (few failures each)

- **Read-only array error** (push, unshift): pushing/unshifting onto a read-only array
  should die "Modification of a read-only value" — PCL doesn't mark arrays as read-only.
- **push onto scalar/literal/hashref** (push.t): error message format differences.
- **delete.t**: croak when deleting from read-only — same read-only issue.

### ord.t (3 failures)

- **Over-Unicode-plane chars** (tests 33–35): `ord(chr(0x110000))` returns 65533
  (U+FFFD) instead of `1114112`. PCL's `p-chr` clamps to valid Unicode range but
  shouldn't for very large code points.

### chr.t (30 failures)

- **`chr(-N)` should return U+FFFD** (test 6+): `chr(-0.1)` should return the Unicode
  replacement character U+FFFD. PCL probably returns `""` or `chr(0)`.
- **`use bytes`** (tests 11–13): `use bytes; chr(-1)` = `"\xFF"` — not supported.
- **Tied scalar with chr** (tests 14–15): `chr($tied_var)` where tied var is -1 —
  PCL either doesn't fetch or fetches wrong value.

### infnan.t (383 failures)

- **String eval of NaN/Inf string addition** (tests 535+, ~70 failures): `eval '$a = "nan(123)" + 1'`
  — should give NaN, but string eval subprocess doesn't return value correctly.
- **`chr(Inf/NaN)` error message** (~6 failures): see bug #7 above.
- **`sprintf %a`** (~2 failures): hex float format not implemented.
- **Various pack with NaN/Inf** (~50 failures): `pack("c", NaN)` etc. — error message format.

---

## Documented not-supported (no fix needed)

- `?pat?` one-match regex (reset.t)
- `@_` aliasing (pos.t defelem, aassign.t alias tests)
- `lvalue substr` (state.t, substr.t, bless.t)
- DESTROY via GC (ref.t, grep.t, bless.t)
- `use bytes` (chr.t)
- Error detection for invalid Perl (for.t 131–138, my.t 53–59) — per principle 9, comment out

---

## Priority ranking (fixable, multi-file impact)

| # | Bug | Files affected | Status |
|---|-----|---------------|--------|
| 1 | `state ++$var` / `state $var++` parser | state.t | ✅ FIXED (session 172) |
| 2 | `scalar(%hash)` key count | each.t | ✅ FIXED (session 175, +2 tests) |
| 3 | `%hash = (...)` list-context return | hashassign.t | open (~13 failures) |
| 4 | `substr` out-of-bounds warning/error | substr.t | ✅ FIXED (session 174, +38 tests) |
| 5 | `\(list_expr)` → ARRAY not SCALAR | bless.t, ref.t | open (~3 failures) |
| 6 | `p-/` → CL ratio not float | hexfp.t (whole file skipped) | ✅ FIXED (session 172) |
| 7 | `chr(Inf/NaN)` error message | infnan.t | ✅ FIXED (session 172) |
| 8 | Dynamic loop labels `last $var` | loopctl.t | open (~3 failures) |
| 9 | `join(undef, ...)` warning | join.t | ✅ test 18 FIXED (session 175); tests 9-10 not fixable (lazy eval) |
| 10 | `each`/`keys` order mismatch | each.t | open (~1 failure) |
| 11 | `p-sort` inplace (`@a = sort @a`) box issue | sort.t | open (~5 failures) |
| 12 | `bless into ref` detection | bless.t | open (~1 failure) |
| 13 | POSIX errno stubs | bless.t | open (~2 failures) |
