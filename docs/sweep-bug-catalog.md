# PCL Sweep Bug Catalog

Generated 2026-05-07. Baseline: 18209 pass / 10159 fail across 100 files, 40 fully passing.
Updated 2026-05-15 (session 185). Current: ~12506 pass / ~2396 fail, 42 fully passing.
Updated 2026-05-23 (session 200). Current: 27439 pass / 2230 fail, 58 fully passing (107 files + 2 skipped).
Updated 2026-05-23 (session 201). Current: **27443 pass / 2226 fail, 58 fully passing** (107 files + 2 skipped).
Updated 2026-05-23 (session 202). **`%a`/`%A` hex-float sprintf implemented** (cl/pcl-runtime.lisp sprintf-one). `parse-perl-number` overflow bug identified but NOT YET fixed (see infnan.t entry).
Updated 2026-05-24 (session 207). **Fixed: time.t fully passing (72/72); chdir.t fully passing (44/44); qr.t 18→21 passing; args.t 15→18 passing**. Skips sprintf.t (deprioritized) and Unicode/utf8-encode issues (documented not-supported).
Updated 2026-05-26 (session 210). **PPI 1.291 upgrade. 27727 pass / 903 fail, 58 fully passing. Dotted bitwise ops (&./|./^./~.) implemented. newline-in-use-comment bug fixed. bop.t: 434→446 passing (crash vs early-stop). sprintf.t: POSIX::DBL_MAX crash (pre-existing, no stub).**

**Session 207 fixes:**
- **time.t** (previous session): `(EXPR)[N]` subscript now forces LIST_CTX on inner expression (`list_ctx_subscript` metadata in PExpr.pm + `gen_array_ref_access` in ExprToCL.pm). `pl-like`/`pl-unlike` now respect regex modifiers (pcl-test.lisp). `times` added to Config.pm `known_no_of_params`. `$ENV{TZ}` limitation documented in not-supported.md.
- **qr.t** (21/37): `box-nv` now returns object-address for p-regex-match structs (not 0). `p-ref` returns "REGEXP" when inner value is p-regex-match (ref to qr// = "REGEXP"). Fixes tests 3, 9, 22, 29.
- **args.t** (18/23): `local(@_)` now correctly handled — `_find_symbols_and_undefs_in_list` now accepts `PPI::Token::Magic` tokens (was Symbol-only; `@_` is Magic). Single array/hash local with init now emits var as default return value in let body.
- **chdir.t** (FULLY PASSING): `p-chdir` now captures `sb-posix:syscall-errno` on failure and stores in `*p-stored-errno*`.
Skips sprintf.t (deprioritized) and Unicode/utf8-encode issues (documented not-supported).

---

## Cross-cutting bugs (high value targets)

### 1. `state ++$var` / `state $var++` — parser drops `state` or the `++` ✅ FIXED (session 172)

**Files:** state.t (tests 77–82) — now passing.

---

### 2. `scalar(%hash)` returns hash-table debug string instead of key count ✅ FIXED (session 175)

**Files:** each.t (tests 47, 53) — fixed. hashassign.t (test 209) — separate issue (group 3).

---

### 3. `%hash = (...)` in list context doesn't return the list ✅ FIXED (sessions 183–185)

**Files:** hashassign.t — 42 → **4 failures** (tests 304, 307–309 = lvalue aliasing, documented not-supported)

---

### 4. `substr` out-of-bounds: no "substr outside of string" warning or error ✅ FIXED (session 174)

**Files:** substr.t — ~38 OOB failures fixed; now 358/397 passing.
Remaining failures (tests 313-397) are lvalue substr and \substr — documented not-supported.

---

### 5. `\(list_expr)` takes ref to ARRAY instead of SCALAR in some contexts

**Files:** bless.t (test 11, 105, ~3 failures), ref.t

**Root cause:** `bless \(map "$_", "test"), "C"` — `map` returns a list; `\(list)` in
scalar context should take a ref to the last scalar element, giving a SCALAR ref.
PCL is treating `map` as returning an array-ref and `\` on that gives ARRAY ref.

---

### 6. `p-/` produces CL rational `1/4` instead of float `0.25` ✅ FIXED (session 172)

---

### 7. `infnan.t` — wrong error format for `chr(Inf)` / `chr(NaN)` ✅ FIXED (session 172)

---

### 8. Dynamic loop labels: `last $var` / `next $var` ✅ FIXED indirectly (loopctl.t now fully passing)

---

### 9. `join.t` — `join(undef, list)` ✅ CLOSED (join.t fully passing)

---

### 10. `each.t` ✅ FULLY PASSING (session 200)

---

## Per-file issues (smaller scope, current as of session 201)

Files ordered roughly by remaining failure count.

---

### hash.t (269 failures, 225/494 passing)

Three distinct bug classes:

- **`Hash::Util` bucket statistics** (~225 tests): `bucket_ratio`, `bucket_array`,
  `num_buckets`, `bucket_stats` — all test CL internal hash layout (number of buckets,
  fill ratios). Not implementable: CL hash tables have opaque internals.
  These tests should be treated as documented not-supported.

- **`scalar(%hash)` string format** (~24 tests): In older Perl, `scalar(%hash)` returned
  `"used/total-buckets"` e.g. `"2/8"`. As of 5.26 it returns just the count. Tests that
  compare `scalar(%h) eq scalar(0+keys %h)` still pass, but tests that expect the old
  ratio string fail. PCL returns an integer key count which is correct for 5.26+.

- **Hash copy identity** (~13 tests): `a .. zz (+N copy) has same keys` — copy of hash
  should have identical key set. `@{%copy} == @{%orig}` fails, probably because PCL's
  hash copy iterates differently or misses some keys.

- **DESTROY / weak-ref** (3 tests): GC-based destruction — documented not-supported.

- **`ref hash keys at compile`** (test 3): `ref hash keys are not stringified during
  compilation` — compile-time constant folding check.

- **`magic keys`** (test 493): hash magic keys not implemented.

---

### infnan.t (396 failures, 716/1112 passing)

- **`sprintf("%a", Inf)` case** (tests 21, 25, ~2 total): Returns `'inf'` instead of
  `'Inf'`. `%a` format for Inf/NaN should capitalize. Fix: `p-sprintf` special-case
  for Inf/NaN with `%a` format.

- **`pack c/C/s/S/...` with Inf/NaN error messages** (tests 56–167, ~112 failures):
  `pack('c', Inf)` should die `"Cannot pack Inf in pack"`. PCL returns wrong error
  message or undef. Fix area: `_pack_tmpl` in `cl/pcl-pack.lisp` — add Inf/NaN checks
  before integer conversion.

- **`pack W/U` with Inf** (tests 175–207, ~33 failures): `pack('W', Inf)` should die
  `"Cannot pack Inf"`. PCL currently returns empty string. W/U formats don't have
  the Inf guard that integer formats have.

- **`pack p/P` with Inf** (tests 252–255, 4 failures): `pack p Inf` should return
  a pointer to the string "Inf" (not undef). p/P format is disabled (skipped) in PCL.

- **`sprintf` Inf/NaN with various formats** (tests 365–440, ~73 failures): padding,
  width, precision flags with Inf/NaN — edge cases in `p-sprintf`.

- **`parse-perl-number` overflow for `'1e9999'`** (~10 failures): `(ignore-errors
  (read-from-string "1e9999"))` silently catches SBCL's `FLOATING-POINT-OVERFLOW`
  READ-ERROR and returns `nil`, so `parse-perl-number` returns 0 instead of Inf.
  Fix: change to `handler-case` catching `floating-point-overflow` → return
  `sb-ext:double-float-positive-infinity` (or negative Inf if leading minus present).

- **String eval of NaN/Inf** (tests 500+, ~126 failures): `eval { $x + 1 }` with NaN
  raises SBCL `FLOATING-POINT-INVALID-OPERATION` instead of returning NaN. The
  `p-eval-block` trap doesn't catch SBCL arithmetic signals. Needs SBCL condition
  handler inside `p-eval-block`.

---

### sprintf2.t (171 failures, 1507/1678 passing)

- **`%a`/`%A` hex float format** ✅ **IMPLEMENTED (session 202)**: `sprintf-one` now has a
  full `(#\a)` arm using `integer-decode-float`. Verified: `printf "%a\n", 3.14` →
  `0x1.91eb851eb851fp+1`. The 171 remaining failures in sprintf2.t are NOT `%a` tests —
  they are: `%NNN$s` positional args, missing/redundant argument warnings, UTF-8 string widths.
  The `%a` test block in sprintf2.t is gated on `$Config{nvsize} == 8` — investigate `$Config`
  support as a next step to enable those gated tests.

- **`%a` precision overflow** (tests 1570–1572, 3 failures): Integer overflow in extreme
  precision values for `%a`.

- **`%n` format** (tests 1577–1578, 2 failures): `sprintf("%n", ...)` — stores character
  count to an argument. Not implemented (and intentionally not in Perl's standard sprintf).

- **`.=` on array element with sprintf** (test 1657, 1 failure): `$ary[3] .= sprintf(...)` —
  probably lvalue context issue on array element .=.

- **Croak for large numeric format** (tests 1673–1678, 6 failures): `sprintf("%7000000000E", 1)`
  should die "Integer overflow in format string". Error message format mismatch.

---

### aassign.t (85 failures, 104/177 failing — PARTIAL, early stop not shown)

- **List-context function return** (test 1): `($a,$b) = f_ret_14()` where `f_ret_14`
  returns `1..4`. Gets `(:)` meaning empty. Root cause: the wantarray/VOID_CTX regression
  from sessions 162–163. **Do NOT fix wantarray issues without explicit user request**.

- **Array alias assignments** (tests 12–16): lvalue aliasing forms — documented not-supported.

- **Tied variable assignments** (tests tied-*): tie interactions with list assignment.

---

### array.t (67 failures, 128/195 passing)

Two root causes:

- **List-of-arrays slice** (test 30): `join('', (@foo,@bar)[0..5])` — result is
  `'ARRAY(0x...)ARRAY(0x...)'` instead of `'acebdf'`. When two arrays appear in a list
  and a slice is taken, PCL boxes each array as an array-ref instead of flattening.
  Fix area: list context expansion of array variables inside slice operand.

- **AASSIGN_COMMON self-assignment** (tests 33–62, ~27 failures): Patterns like
  `@a = @a`, `(undef,@a) = @a`, `@a = ('X',@a,'Y')`, `local @b = @b` — these require
  that the RHS is fully evaluated before assignment begins. PCL evaluates and assigns
  simultaneously, so `@a = @a` truncates and `(undef,@a) = @a` drops first element.
  Fix area: `p-list-=` / `p-array-=` in `pcl-runtime.lisp` — snapshot RHS into a temp
  vector before assigning.

- **Negative-index error** (test 82): `-1` index assignment: `$a[-1] = 0` on non-existent
  array should error. PCL doesn't check.

- **`$#ary` on freed array** (test 83): Should be undef, PCL returns `-1`.

---

### ref.t (62 failures, 168/245 passing — PARTIAL, stops at ~230)

- **`&{""}` call** (test 21): `ref eval {\&{""}}` should return "CODE". PCL raises error.
- **IO/FORMAT dereference errors** (tests 32–40): `*STDOUT{IO}` and `*STDERR{FORMAT}` derefs.
- **ref to lvalue types** (tests 64–73): documented not-supported (lvalue subs).
- **ref to format / IO refs** (tests 88–90): format system not implemented.
- **UTF-8 symbol names** (tests 134–168): Unicode stash name lookup not supported.
- **Deref from list slice** (tests 173–177): `@{(...)}{...}` — hash deref from list slice.
- **Early stop**: investigate tests ~230 onwards.

---

### bop.t (62 failures, 434/510 passing — PARTIAL, stops at test 451)

- **Large shift / `use integer` edge cases** (tests 50–125): `4 << 2147483648` should yield 0;
  CL integers are bignums, not 64-bit fixed-width. Documented not-supported.
- **UTF-8 flag operations** (test 158): bitwise ops with UTF-8 flagged strings — `use bytes`.
- **`~.` string complement with number** (tests 172, 186): `~."string"` where the operand
  is a number — PCL returns large integer instead of complement string.
- **Duplicate warning suppression** (tests 202, 215): `no warnings 'uninitialized'` should
  prevent repeated warnings.
- **Glob bitwise ops** (tests 320–328): `*STDOUT | "string"` — bitwise ops between glob and string.
- **Early stop at 451**: test 451 "correct error" — error message mismatch triggers exit.

---

### caller.t (53 failures, 12/112 passing — PARTIAL)

Mostly not-supported — see `docs/not-supported.md`. Caller returns `"(unknown)"` filename and line 0.

---

### method.t (47 failures, 113/163 passing — PARTIAL)

- **`&$one()` where `$one=1`** (tests 5–12): PCL raises "Not a CODE reference" instead of
  "Undefined subroutine &main::1 called". Error message mismatch.
- **AUTOLOAD via @ISA chain** (tests 40–58): method resolution with AUTOLOAD + goto &$AUTOLOAD
  returns wrong result across multi-level inheritance.
- **`SUPER` inside moved package** (tests 77–78): `$obj->Bminor::SUPER::test(...)` fails.
- **DESTROY/AUTOLOAD interaction** (tests 100, 103, 114–115): documented (GC doesn't call DESTROY).
- **`UNIVERSAL::AUTOLOAD`** (tests 97–99): `$AUTOLOAD` scoping across packages.
- **Error message format** (tests 116–118): `"new{}"` in error message.
- **Method call on typeglob** (tests 128–131): `*glob->method`.

---

### do.t (20 failures, 53/73 passing)

- **`return do { }` scalar context** (tests 17–38, ~14 failures): `do { }` block doesn't
  receive caller's scalar context when used as the return expression of a sub. Got empty
  instead of the block's scalar value. Fix area: context propagation into `p-eval-block`
  when used as tail expression of a `pl-sub`.

- **`do subname(arg)` / `do $subref("arg")`** (tests 63–68, 4 failures): `do subname("arg")`
  is a syntax error in Perl; `do subname(arg)` (no parens) calls the sub. PCL probably
  parses both forms as sub calls and doesn't distinguish.

- **RT 124248** (test 70): Edge case not yet investigated.

- **`$! is EISDIR on do dir`** (test 73): `do "/tmp"` (a directory) should set `$!` to EISDIR.

---

### local.t (23 failures, 298/319 passing, previously 24)

- **Array size after `local($a[5])`** (tests 119–120): after `local($a[5]) = 'z'` and
  `$a[4] = 'y'` in a block, when block exits `$a[5]` is restored but `$a[4]` is also lost.
  The local-restore trims the array too aggressively.
- **`local $_` with filetest/match** (tests 255–264): `local $_` interactions with
  filetest operators and pattern matching on default `$_`.
- **`local *{$pkg}{method}`** (tests 271–278): temporarily replacing a method via stash
  slot — not supported.

---

### pos.t (16 failures, 14/30 passing)

- **`pos()` set inside `//g`** (test 4): PCL's `pos()` tracking doesn't update correctly
  inside a `//g` loop when using `$&` or other side effects.
- **Lvalue pos DESTROY** (test 9): documented not-supported (GC).
- **`pos` refuses @array / %hash** (tests 10–11): error detection — principle 9.
- **`pos *glob`** (test 12): `pos` on a typeglob not supported.
- **`pos` through defelem** (tests 13–21): accessing/setting pos through aliased array
  elements (defelem). PCL returns undef. Defelem not fully supported.
- **`use bytes` pos** (tests 26, 29–30): byte-offset vs char-offset — `use bytes` not supported.

---

### state.t (20 failures, 142/166 passing — PARTIAL)

**Fixed in session 200** (4 more tests):
- Test 36: `p-post++` `*p-undef*` check fixed.
- Tests 38–40: `state %x = qw(...)` init codegen fixed.

**Remaining:**
- **State hash `:shared`** (tests 41–43): `%f :shared` attribute.
- **Computed goto with state label** (tests 70–73): `goto state $label` — not implemented.
- **State in map/grep** (tests 74–75): `state $x` inside `map/grep` block loses value.
- **Reference to state variable** (test 76): `\state $x` gives same address each call.
- **Lvalue substr as state** (tests 83–92): documented not-supported.

---

### qr.t (16 failures, 21/37 passing)

- **Regex object identity** ✅ FIXED (session 207): `p-regex-match-p` now handled in `box-nv` — qr// objects return unique addresses for `+0` comparison.

- **`ref(\$qr)` returns "REGEXP"** ✅ FIXED (session 207, tests 22, 29): `p-ref` now returns "REGEXP" when the inner value is a p-regex-match struct.

- **Scalar aliasing after bless** (test 6): `my $b1 = $b; bless $b, 'Pie'` — `$b1` should also be Pie. Scalar assignment copies in PCL, can't share identity.

- **PVLV deref `${qr//}`** (tests 11, 12, 16, 24, 25, 27, 31, 32, 37): `${qr//}` should return a magical PVLV that stringifies to the pattern. Complex. Not supported.

- **Stringification of blessed qr// objects** (tests 14, 18): `"$qr"` should match `Foo=REGEXP(0x...)`. Not supported.

- **DESTROY via GC** (test 36): documented not-supported.

---

### index.t (30 failures, 90/120 passing)

- **`utf8::encode` octet-mode index** (tests 49–58, ~10 failures): After `utf8::encode($s)`
  the string is raw bytes. `index($encoded, $encoded_pattern)` should find at byte position,
  not character position. PCL treats everything as characters.

- **NUL character search** (tests 63–72, ~10 failures): `index($str, "\0")` returns -1 when
  it should find the NUL byte. CL strings are NUL-safe but PCL's `p-index` may stop at NUL
  or use a C-string-based search. Fix: use CL's `search` on character sequences.

---

### length.t (15 failures, 32/49 passing — PARTIAL, 2 tests unreached)

- **`use bytes; length(unicode_str)`** (tests 7–23): not supported.
- **`length(undef)` returns `''` instead of `undef`** (test 34).
- **Overloaded `length`** (tests 35, 41, 43): `length($obj)` where obj overloads `""`.
- **Missing "uninitialized" warnings** (tests 36, 42).
- **Tests 48–49 not reached**: depend on `charset_tools.pl`.

---

### substr.t (~39 failures, ~358/397 passing)

- **Out-of-bounds warning/error**: ✅ FIXED session 174.
- **Lvalue substr** (tests 313–397): documented not-supported.

---

### range.t (18 failures, 144/162 passing)

- **LHS array slice in list assignment** (test 4): `($a,@bcd[0..2],$e) = ('a','b','c','d','e')` —
  list assignment with array slice on the LHS. Got `'a:b:c:d:e'` expected but PCL returns
  `'a:ARRAY(...)...'`. Fix area: `p-list-=` / array slice LHS handling.

- **Range in `/e` eval** (tests 15, 17): `s/(\w)-(\w)/join ':', $1..$2/e` — the `..` range
  inside `/e` gets scalar context (returns flip-flop 1) instead of list context. Result
  is `'1E0'` (i.e. float 1). Fix: ensure eval'd expression in `/e` captures list context
  from the surrounding `join`.

- **`scalar range`** (tests 15–16): `my $n = (() = "0"..-1)` — count of empty range.
  Gets `'1E0'` instead of `0`. Same root cause as above — `..` in context of counting.

- **Bignum range bounds not rejected** (tests 78–118, ~18 failures): `(9223372036854775808..10)`
  should die "Range iterator outside integer range". SBCL bignums don't overflow 64-bit
  bounds, so no error is raised. Documented as "not-supported" (SBCL is infinite precision).

- **Modifiable variable range** (tests 156, 159): `for my $x (...) { ... }` where loop
  var appears on both sides — needs investigation.

---

### readline.t (12 failures, 24/36 passing)

- **Read-only modification** (test 1): error message format mismatch for
  `readline()` on read-only value.

- **`readline` reads first line** (test 16): `readline STDIN` in a specific context
  returns `''` instead of reading a line. Likely a filehandle setup issue in the test.

- **UTF-8 append** (tests 19–22): `$str .= <FH>` when string or filehandle is UTF-8
  encoded — appending truncates content. `'ascii'` instead of `'ascii...'`.

- **`<>` autovivification** (tests 26–27): `<>` and `readline` should not autovivify
  a scalar when the filehandle doesn't exist.

- **`tell()` after GV unglobbed** (tests 28–30): `tell()` should return -1 after the
  last typeglob pointing to a filehandle is destroyed. PCL returns 0.

- **Error message on unopened FH** (test 32): `readline()` on unopened FH — error
  message format `"readline() on unopened filehandle y"` not matching.

---

### bless.t (~11 failures, 95/106 passing)

- **`\(map ...)`** (tests 11, 105): see cross-cutting bug #5.
- **`\substr` lvalue ref** (tests 26–28): documented not-supported.
- **POSIX errno values** (tests 77–78): `POSIX::EINVAL` gives wrong errno string.
- **Bless-into-ref detection** (test 101): `bless $obj, $ref_ref` should die.

---

### grep.t (6 failures, 71/77 passing)

- **DESTROY in grep** (tests 69–76): documented (GC doesn't call DESTROY).
- **Invalid grep syntax** (test 61): error detection — principle 9.

---

### split.t (9 failures, 210/219 passing)

- **Replacement interpolation** (test 58): split with `/e`-style replacement pattern —
  PCL returned `'p:q:r:s'` expected `'p1q1r1s'`.

- **Unicode whitespace separator** (tests 136–138): `split(' ', $str)` with Unicode
  whitespace (e.g. U+2000) — PCL splits on ASCII whitespace only.

- **Split to specific array** (tests 149–151): `@pkg::ary = split(...)` — assignment
  to a package-qualified or stacked array. Got `'a b c'` instead of `'1 2 3'`.

- **`/e` re-eval count** (tests 153, 155): `split(/(?{ $n++ })/, ...)` — inline code
  in split pattern only evaluated once instead of per-split.

---

### do.t (already listed above)

---

### time.t ✅ FULLY PASSING (session 207)

---

### chop.t (6 failures, 94/100 passing)

- **`chop(@stuff = @stuff)`** (test 35): `chop` on a freshly-assigned array — returns
  `''` instead of last char `'f'`. `chop` doesn't see the updated array value after
  the assignment.

- **`chop` as lvalue error** (tests 48–51): `chop($x) = 1` / `chomp($x,$y) = (1,2)` —
  should die "Can't modify chop in assignment". PCL doesn't detect this.

- **`chomp` on hash keys** (test 100): `$b = chomp @a when $b eq $/ eq 0 and \$a[0] == \$b`
  — lvalue aliasing condition.

---

### args.t (5 failures, 18/23 passing)

- **`splice(@_, 0, 0, 'x')`** (tests 3–4): `splice` on `@_` inside a function — modifies
  `@_` but changes aren't visible through lvalue args. Got `'4'` instead of `'a b c x'`.

- **`goto &sub` with multiple @_ elements** (tests 7–8): `goto &sub` with a modified `@_`
  containing multiple elements — same @_ aliasing issue.

- **`local @_`** ✅ FIXED (session 207, tests 12, 15, 18): `_find_symbols_and_undefs_in_list` now handles `PPI::Token::Magic` (`@_` is Magic not Symbol). Single array local with init emits var as default return value.

- **`delete $_[0]`** (test 23): `delete $_[0]` outside a block should set element to undef.

---

### chdir.t ✅ FULLY PASSING (session 207)

---

### concat2.t (2 failures, 2/4 passing)

- **UTF-8 concatenation changes flag** (tests 1–2): `.=` concatenation changes the UTF-8
  flag of the string in ways that affect subsequent operations. `"abc" .= $utf8_str`
  should upgrade the LHS to UTF-8; PCL doesn't track the UTF-8 flag.

---

### closure.t (2 failures, 48/? passing)

- **Nested closure with `my $i = $i`** (tests 11, 13): `sub bizz { my $i=7; sub { my $i=$i; sub{...} } }`.
  Inner closure captures `my $i = $i` (shadow). When `bizz()` is called twice,
  the two inner subs should have independent `$i` values (both 7). PCL returns `''` —
  the inner variable doesn't properly capture the outer `$i` before shadowing it.
  Root cause: `_vars_referenced_in_closures` renames `$i__lex__N` but the `my $i = $i`
  RHS is parsed against the wrong scope (new name already in effect for RHS).

---

### or.t (3 failures, 11/14 passing)

- **Tied variable as `||` operand** (tests 8–10): `$tied || $var` — lvalue context
  propagated through `||` to RHS and LHS. Tied variable fetch semantics in lvalue context
  not supported. Also `||` propagating lvalue context to its lhs.

---

### push.t (4 failures, 28/32 passing)

- **Push onto invalid target** (tests 4–6): `push $int, ...` / `push $hashref, ...` /
  `push $blessed_arrayref, ...` — should die "Type of arg 1 to push must be array or
  list-ref". PCL doesn't validate push target type.
- **Croak on readonly array** (test 32): same as unshift below.

---

### repeat.t (3 failures, 45/48 passing)

- **Void context list repeat** (test 43): `(...)x... in void context in list (via scalar
  comma)` — context classification edge case.
- **Lvalue aliasing in repetition** (tests 46–47): `\$_[0] == \$_[1]` when @_ aliases
  elems repeated by x — documented not-supported (@_ aliasing).

---

### delete.t (3 failures, 53/56 passing)

- **`\delete $h{key}` address equality** (tests 26, 54): `\(values %a)` == `\$a{bar}` ==
  `\delete $a{bar}` — all three should give the same address. PCL's delete returns a
  copy, not the same slot.
- **DESTROY on deleted element** (test 56): GC-based DESTROY — documented not-supported.

---

### hashassign.t (4 failures, 305/309 passing)

- **Remaining 4 failures** (tests 304, 307–309): lvalue aliasing into RHS list —
  documented not-supported.

---

### flip.t (1 failure, 13/14 passing)

- **String flip-flop warning count** (test 10): `"foo".."bar"` in scalar context should
  generate 2 "isn't numeric" warnings (one for "foo", one for "bar"). PCL generates 0.
  Fix: `p-..` / `p-...` flip-flop code should warn when comparing non-numeric endpoints.

---

### concat.t (1 failure, 233/225 passing — note: extra tests exist)

- **`($a = expr) .= 'c'`** (test 220): `($a = 'A'.$b) .= 'c'` — assignment expression
  used as lvalue for `.=`. Expected `"Abc"`, got `"ab"`. PCL doesn't treat the result
  of `=` as an lvalue. Fix: `p-.=` / assignment chain to detect `(var = expr) .= rhs` form.

---

### undef.t (3 failures, 32/35 passing)

- **Modification of read-only value** (tests 16–17): `undef $constant` should die
  "Modification of a read-only value attempted". PCL gives `''` (no error).
- **`undef &tcp_proto`** (test 18): `undef &Socket::tcp_proto` — undefining a function
  slot. PCL generates "function undefined" error instead of silently setting it to undef.
  Fix: `p-undef-func` or similar in runtime.

---

### unshift.t (1 failure, 18/19 passing)

- **Croak on readonly array** (test 19): `unshift @readonly, ...` should die "Modification
  of a read-only value attempted". PCL doesn't mark arrays as readonly.

---

### wantarray.t (1 failure, 27/28 passing)

- **Void context through `||`** (test 11): `sub f { $false || context(shift) }; f('V')` —
  should see void context at `||` RHS. PCL propagates scalar context instead.
  Do NOT fix — wantarray regression area. See `docs/wantarray-context.md`.

---

### vec.t (2 failures, 76/78 passing)

- **`my $foo` hoisted across `eval {}`** (tests 25–26): `eval { my $foo = vec($foo,...) }`
  — PCL hoists `my $foo` to the outer block, shadowing file-level `$foo`. Documented in
  session-log §189. Fix: don't hoist declarations across `eval {}` boundaries.

---

### grent.t (1 failure, 2/3 passing)

- **Parallel test ordering** (test 2): "not necessarily serious: run t/op/grent.t by itself".
  Race condition in parallel sweep only.

---

### Hexfp.t (21 failures, 104/125 passing)

- PPI can't parse `0x1.8p-1` hex float literals — documented not-supported.
  Remaining failures all involve hex float literals in source code.

---

### sort.t ✅ FULLY PASSING
### join.t ✅ FULLY PASSING
### loopctl.t ✅ FULLY PASSING
### for.t ✅ FULLY PASSING
### my.t ✅ FULLY PASSING
### chr.t ✅ FULLY PASSING
### ord.t ✅ FULLY PASSING
### do.t — listed above
### splice.t ✅ FULLY PASSING
### reset.t ✅ FULLY PASSING
### vec.t — 2 failures listed above
### flip.t — 1 failure listed above
### wantarray.t — 1 failure listed above
### auto.t ✅ FULLY PASSING
### pos.t — listed above
### qr.t — listed above
### readline.t — listed above

---

### Zero-passing files

- **crypt.t**: requires `crypt()` XS function — not implemented.
- **lfs.t**: large file support — not tested.
- **signatures.t**: Perl 5.36+ subroutine signatures — partial implementation.
- **test-pack-new.t**: new pack tests in progress.
- **test_ref_pass.t**: pass-by-reference tests in progress.

---

## Documented not-supported (no fix needed)

- `?pat?` one-match regex (reset.t)
- `@_` aliasing (pos.t defelem, aassign.t alias tests, repeat.t 46–47)
- `lvalue substr` (state.t, substr.t, bless.t)
- DESTROY via GC (ref.t, grep.t, bless.t, delete.t test 56, hash.t)
- `use bytes` (chr.t, pos.t 26/29/30, length.t 7–23)
- Error detection for invalid Perl (for.t 131–138, my.t 53–59) — per principle 9, comment out
- Hash bucket internals (`Hash::Util` bucket_ratio etc.) — not implementable in CL
- Hex float literals `0x1p-2` — PPI misparse

---

## Priority ranking (open items, session 207)

Ordered by estimated fixable test count, excluding pack.t (separate plan).

| # | Bug | Files affected | Est. impact |
|---|-----|----------------|-------------|
| 1 | `%a` hex float format in sprintf | sprintf2.t, infnan.t | ~170 tests |
| 2 | `infnan.t` SBCL arithmetic signal in eval-block | infnan.t | ~126 tests |
| 3 | `Hash::Util` bucket stats — document/skip | hash.t | ~225 tests (all not-supported) |
| 4 | `gmtime` large/negative timestamps | time.t 47–70 | ✅ FIXED (time.t fully passing) |
| 5 | NUL character search in `index`/`rindex` | index.t 63–72 | ✅ FIXED (already passing) |
| 6 | `index` in utf8::encode octet mode | index.t 49–58 | ~10 tests |
| 7 | AASSIGN_COMMON: `@a = @a` snapshot RHS | array.t 33–62 | ~27 tests |
| 8 | `pack c/C/s/...` Inf/NaN error messages | infnan.t 56–167 | ~112 tests |
| 9 | `do {}` scalar context at sub return | do.t 17–38 | ~14 tests (wantarray-adjacent) |
| 10 | method AUTOLOAD chain + goto &$AUTOLOAD | method.t 40–58 | ~15 tests |
| 11 | `ref($qr)` returns "REGEXP" | qr.t | ✅ PARTIALLY FIXED (session 207, +3 tests) |
| 12 | `gmtime(0)` scalar year format | time.t 43,45 | ✅ FIXED (time.t fully passing) |
| 13 | `local @_` in sub not properly scoped | args.t 12/15/18 | ✅ FIXED (session 207) |
| 14 | `~.` string complement on number | bop.t 172, 186 | ~2 tests (PPI parses wrong, hard) |
| 15 | `\(list_expr)` → ARRAY not SCALAR | bless.t, ref.t | ~3 tests |
| 16 | `state $x` in map/grep block | state.t 74–75 | ~2 tests |
| 17 | Glob bitwise ops (`*STDOUT \| "str"`) | bop.t 320–328 | ~9 tests |
| 18 | `chdir('')` not setting `$!` | chdir.t 28, 34 | ✅ FIXED (session 207) |

### All previously listed cross-cutting bugs — FIXED or CLOSED

| Bug | Resolution |
|-----|-----------|
| `state ++$var` parser | ✅ FIXED session 172 |
| `scalar(%hash)` key count | ✅ FIXED session 175 |
| `%hash = (...)` list-context return | ✅ FIXED sessions 183–185 |
| `substr` out-of-bounds warning | ✅ FIXED session 174 |
| `p-/` CL ratio not float | ✅ FIXED session 172 |
| `chr(Inf/NaN)` error message | ✅ FIXED session 172 |
| `join(undef, ...)` test 18 | ✅ FIXED session 175 |
| `p-sort` inplace sort | ✅ FIXED session 184 |
| `while (my ($k,$v) = each %h)` | ✅ CLOSED (each.t now fully passing) |
| Dynamic loop labels `last $var` | ✅ CLOSED (loopctl.t now fully passing) |
| `p-list-=` list-context return | ✅ FIXED sessions 183–185 |
| `tail_position` leaked in `gen_funcall` | ✅ FIXED session 184 |
| Sort comparator wrong context | ✅ FIXED session 184 |
| splice.t flip-flop LIST_CTX | ✅ FIXED sessions 182–183 |
| eval-block list context (pack.t Group A) | ✅ FIXED session 199 |
| time.t — `(EXPR)[N]` subscript LIST_CTX, `times` bareword, regex modifiers in `pl-like` | ✅ FIXED session 207 |
| chdir.t — `$!` not set on failed `chdir('')` | ✅ FIXED session 207 |
| args.t — `local(@_)` not localized (Magic token not found) | ✅ FIXED session 207 |
| qr.t — qr// object numeric address was 0 | ✅ FIXED session 207 |

### Session 200 fix: `(pcl:p-defpackage ...)` pre-declaration

**Bug:** `_assemble_output()` in `Pl/Parser.pm` emitted `(defpackage :PKG (:use :cl :pcl))`
for cross-package symbol pre-declarations, triggering SBCL "MAIN also shadows" warnings.

**Fix:** Changed to `(pcl:p-defpackage $cl_pkg)` (warning-suppressed, idempotent).
Updated `Pl/t/decl-ordering-01.t` test 23 regex accordingly.
