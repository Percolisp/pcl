# pack.t Remaining Failure Groups

**Status as of session 194:** 5174 pass, 778 fail, 8770 skip (14722 total).

**Session 193 baseline:** 5118 pass, 834 fail, 8770 skip (14722 total).

Skipped blocks (not counted here):
- 8748 tests: `(?{code})` regex blocks used in `%val` setup — CL-PPCRE rejects this syntax
- 10 tests: `p`/`P` pointer format — raw C memory address, no CL equivalent
- 7 tests: Unicode section p/P tests — same reason
- 1 test: `P*` error message check — same reason

The 834 failures fall into the groups below, ordered by test count.

---

## Group A — `eval { }` list context not propagated (~276 tests)

**Tests:** 297–443, 3511–3912 (pairs), 3916–3982

**Symptom:**
```
is(scalar @t, 2)        → got: '1'   expected: '2'
is($t[1], 34)           → got: undef expected: '34'
is($x[0], $x[1])        → got: '-32768.0'  expected: undef
```

**Root cause:** `my @t = eval { unpack(...) }` — PCL's `p-eval-block` does not propagate
list context from the enclosing array assignment into the block. `unpack()` runs in scalar
context inside the eval, returning only 1 element. `@t` gets 1 element; tests that check
`$t[1]` or `scalar @t == 2` fail.

Affects three loops:
- **Tests 297–443** (74 fails): `my @t = eval { unpack("$t*", pack("$t*", 12, 34)) }` for
  each format in `@templates` (c, C, W, i, I, s, S, l, L, n, N, v, V, f, d, q, Q).
- **Tests 3511–3912** (163 fails, every 5th test): byteorder loop —
  `my @x = eval { unpack "$format$format>$format<", $nat.$be.$le }`, then `is($x[0], $x[1])`.
- **Tests 3916–3982** (39 fails): same byteorder loop, `f`/`F`/`d`/`D` formats.
  Also reveals float formatting bug: $x[0] prints as `'0.0f0'` instead of `'0'` (see Group B).

**Fix area:** `Parser.pm` — `_process_eval_block_statement`; `pcl-runtime.lisp` — `p-eval-block`.
The block must detect the enclosing wantarray and propagate it into the block body.

**Note:** Fixing Group A also eliminates the `'expected: undef'` pattern in tests 3511–3982,
because once `@x` has 3 elements, `is($x[0], $x[1])` and `is($x[0], $x[2])` both compare
equal values and pass (even if those values have float format issues).

---

## Group G — `U` Unicode codepoint format (~152 tests)

**Tests:** 14045–14288

**Symptom:**
```
unpack("aC/UU", "b\0$bytes")  → got: 'b,225'   expected: 'b,8188'
```
225 = 0xE1 = first byte of UTF-8 for U+1FFC (8188).

**Root cause:** PCL's `U` format handler returns the first raw byte of the UTF-8 byte sequence
instead of decoding the full multi-byte sequence into its Unicode codepoint. `unpack 'U'`
should return the integer codepoint (e.g., 8188 = U+1FFC); PCL returns the first byte (0xE1 = 225).

Also affects:
- `U0` mode scoping (`U0` switches encoding mode for subsequent format chars)
- Counted-length strings with `U` format (`C/U`, `a/U`)
- Round-trips: `pack("U", N)` / `unpack("U", str)`

**Fix area:** `cl/pack-impl.pl` — `U` format pack and unpack handlers.
Unpack: read UTF-8 bytes, decode codepoint. Pack: encode integer N as UTF-8 byte sequence.

---

## Group C — Sign extension in `l!`, `s!`, `i!`, `j` formats ✅ FIXED (session 194)

**Tests:** 2293–2454, 13189–13350 — **now 0 failures**

**Root cause (confirmed):** `p-**` in pcl-runtime.lisp always coerces to double-float:
`(expt 2.0d0 64.0d0)` = `1.8446744073709552d19`. For 64-bit sign extension, subtracting
this float from the bignum `18446744073709551615` gives 0.0 (both round to the same double).

**Fix:** Added `pl-_unpack_read_int` CL override at the bottom of `cl/pcl-pack.lisp`
that uses `(ash 1 bits)` for exact integer powers of 2. The override redefines the
`p-sub`-generated transpiled function with direct CL code. No changes to pack-impl.pl.

---

## Group I — UTF-8 `@`/`@!`/`W` byte-vs-character counting (~106 tests)

**Tests:** 13857–14038 (48), 14297–14495 (sparse, ~28), 14564–14703 (58 — but overlaps with H)

**Symptom:**
```
# @! alignment with UTF-8 string
got: '́̂'   expected: '́'          # wrong number of characters positioned

# Pack neutrality test for j
not ok - Pack j undoes unpack j
got: '        '  expected: 'þÿÿÿÿÿÿÿ'   # sign extension issue + encoding
```

**Root cause (two sub-issues):**

1. **`@` / `@!` positioning with multi-byte UTF-8 strings**: `@N` means "seek to byte offset N"
   in pack, but "seek to character offset N" for UTF-8 strings. PCL counts bytes/characters
   incorrectly for one of these modes.

2. **Upgrade/downgrade neutrality**: Various formats (`s`, `j`, `D`) should produce the same
   packed bytes regardless of whether the input string is UTF-8-flagged. PCL sometimes produces
   different byte sequences for upgraded vs downgraded strings, or loses the sign of negative
   values when encoding.

**Fix area:** `cl/pack-impl.pl` — `@` and `@!` format handlers (check UTF-8 character vs byte
offset); `W` format and format neutrality for signed types.

---

## Group H — `F<`/`D` long double and checksum precision (~16 tests)

**Tests:** 13681–13801

**Symptom:**
```
unpack '%65F<' gave 0, expected 16
unpack pack D> -17179869184  → got: undef
```

**Root cause:**
- `F<` (little-endian native float, likely 80-bit long double on x86): checksum `%65F<` gives 0
  instead of 16. Likely a bit-counting issue in the checksum accumulation for the `F` (long
  double) format.
- `D>` (big-endian long double): `unpack pack D> $val` returns undef, meaning packing or
  unpacking fails silently. `D` format may not be implemented or may behave differently from
  Perl's x87 80-bit extended-precision float.

**Fix area:** `cl/pack-impl.pl` — `F` and `D` format size and checksum handling.

---

## Group D — `%N` checksum arithmetic overflow (~32 tests)

**Tests:** 3393–3499 (sparse, every ~3rd test)

**Symptom:**
```
# For list (0,1,...,18446744073709551615) packed with Q
unpack '%65Q' gave 18446744073709551617,  expected 36893488147419103231
```

**Root cause:** The `%N` format computes a checksum: sum all (bit-)values, then take the
result modulo 2^N. For large values like `Q` (unsigned 64-bit), the sum overflows into
numbers larger than 2^64. Perl uses arbitrary-precision arithmetic for the accumulator
then truncates; PCL may use a native CL integer (which doesn't overflow) but applies the
modulus at the wrong point, or uses a narrower intermediate type.

**Fix area:** `cl/pack-impl.pl` — `%` format accumulator: use `logand result (1- (expt 2 N))`
after summing all values (not per-value). Ensure accumulator is a bignum throughout.

---

## Group E — `f` (single-precision float) round-trip precision (~33 tests)

**Tests:** 3069–3211 (sparse, every ~3rd test)

**Symptom:**
```
unpack('f', pack('f', 17179869184))  → got: '17179870000'  expected: '17179869184'
```

**Root cause:** IEEE 754 single-precision (`f`) has 23-bit mantissa (~7 decimal digits).
`17179869184 = 2^34` is exactly representable in float, but PCL's round-trip loses precision.
The pattern is every third test (the third value in each set of 5, corresponding to larger
magnitudes where single-precision rounding matters).

**Fix area:** `cl/pack-impl.pl` — `f` format: use `single-float` explicitly in CL (not
`double-float`) so that the pack/unpack round-trips through 32-bit IEEE 754 as Perl does.

---

## Group B — Error message text mismatches (~32 tests)

**Tests:** 38, 4128–4131, 4273–4278, 4395–4420

**Symptom:**
```
# Test 38
got: ''    expected to match: qr/^Can only compress unsigned integers/

# Test 4128
got: "Invalid type '/' in unpack"
     expected to match: '/' must follow a numeric type

# Test 4273
got: ''    expected to match: Can't use '[<>]' in a group with different byte-order

# Tests 4395, 4404, 4414
got: ''    expected to match: length/code after end of string / Invalid type / Code missing after '/'
```

**Root cause:** PCL's pack error messages differ from Perl's exact wording, or some errors
are not thrown at all (empty `$@` when Perl would die).

**Fix area:** `cl/pack-impl.pl` — error message strings in validation functions. Match Perl's
exact wording for: `'/' must follow a numeric type`, `Can't use '[<>]' in a group`,
`length/code after end of string`, `Code missing after '/'`, etc.

---

## Group F — `/` counted-length format issues (~25 tests)

**Tests:** 4136–4167, 4235–4265, 4370–4387

**Symptom (three sub-issues):**

1. **`/` format returning wrong value in list context** (4136, 4156): unpack returns undef
   instead of the expected string for `a/a*/b*` and similar patterns.

2. **`x` format in list context returns `""` instead of `()`** (4235):
   `list unpack ('x', "N") gave "" expected ()` — the `x` skip format should contribute
   nothing (empty list) but returns an empty string.

3. **Pack `n/a*` not prepending count** (4370–4387):
   `got: 'ABCABC...*', expected: '30.ABCABC...*'` — the count prefix is missing from output.

**Fix area:** `cl/pack-impl.pl` — `/` format logic; `x`/`X` return value in list context.

---

## Group K — IV/NV precision at 64-bit boundary (~5 tests)

**Tests:** 26–30

**Symptom:**
```
pack 'Q', ~0   (= 18446744073709551615 = 2^64-1)  → gives 0
~0 - 1  → got: '18446744073709535232'  expected: '18446744073709551614'
```

**Root cause:** `~0` = 2^64−1 cannot be represented exactly as a double-float
(max exact integer in double is 2^53). Perl stores it as an IV (native 64-bit integer);
PCL converts it to double first, losing the low bits. `pack 'Q'` then encodes the rounded
value (which may be 0 if overflow).

**Fix area:** `cl/pack-impl.pl` — promote integers to `(unsigned-byte 64)` before packing
`Q`, `L`, `N`, `V` formats. Use CL's bignum path when the value exceeds 2^53.

---

## Group L — UTF-8 encoding in `pack W` / `pack U` output (~18 tests)

**Tests:** 4175–4192

**Symptom:**
```
got: '@Ã'   expected: '@Ê'   (pack with W or U of a high character)
not ok - pack doesn't return malformed UTF-8
```

**Root cause:** `pack 'W', N` for N > 127 should produce raw bytes (not UTF-8 encoded).
PCL may be encoding the value as UTF-8 instead of a raw byte (double-encoding), or
producing a different byte sequence due to encoding mode confusion.

**Fix area:** `cl/pack-impl.pl` — `W` format pack: write raw byte (character code N),
do not UTF-8-encode. Check encoding mode flags (`U0`/`C0`) for context.

---

## Group M — Small isolated failures (~7 tests)

| Test | Count | Issue |
|------|-------|-------|
| 24 | 1 | Transpiler error: `PPI::Structure::Condition` not handled — fires inside `eval { pack ... }` where PPI wraps the condition in a Condition node |
| 38 | 1 | Error message: `Can only compress unsigned integers` — PCL may not generate this error for `pack 'w', -1` |
| 447 | 1 | `u` uuencode: last chars of decoded output differ by 2 bytes (`¨  ` vs `   `) — trailing padding in uuencoded block |
| 4165 | 1 | `A` format: got `' stringetc'`, expected `' stringetc'` — invisible whitespace difference (likely tab vs newline in trailing whitespace handling) |
| 4196–4212 | 5 | compress_template loop: `is(join('!',@u1), join('!',@u2))` gives `'1'` vs `'42'` — possibly eval-list-context in a `my @u1 = eval { unpack ... }` call (related to Group A) |

---

## Summary Table

| Group | Tests | Root Cause | Fix Difficulty |
|-------|-------|-----------|----------------|
| A — eval BLOCK list ctx | ~276 | `p-eval-block` ignores enclosing wantarray | Medium — needs context propagation through eval |
| G — `U` codepoint format | ~152 | Returns raw byte instead of decoded codepoint | Medium — UTF-8 decode in pack-impl.pl |
| I — UTF-8 @/W positioning | ~106 | Byte vs character count in `@`/`@!` with multibyte | Hard — UTF-8 mode interaction |
| C — sign extension `!`/`j` | ~96 | Wrong native size for `l!`, `j` (4 bytes not 8) | Easy — fix size table in pack-impl.pl |
| D — `%N` checksum overflow | ~32 | Wrong modulus accumulation for large values | Easy — fix accumulator arithmetic |
| B — error messages | ~32 | Message strings don't match Perl's exactly | Easy — text changes in pack-impl.pl |
| E — `f` float precision | ~33 | Double used where single-float required | Medium — use `single-float` in CL |
| H — `F<`/`D` long double | ~16 | Long double size/format not handled correctly | Hard — platform-specific |
| L — `pack W` UTF-8 output | ~18 | Raw byte vs UTF-8 encoded output | Medium |
| F — `/` counted length | ~25 | `/` format bugs, `x` returns `""` not `()` | Medium |
| K — 64-bit IV/NV | ~5 | Double can't hold 2^64−1 exactly | Medium — use bignum path |
| M — isolated | ~7 | Various (see table above) | Varies |

**Easiest wins:** Group C (96 tests, fix size table), Group D (32 tests, fix accumulator),
Group B (32 tests, fix message strings). Together ~160 tests for straightforward changes.

**Biggest payoff:** Group A (276 tests) — but context propagation through eval is complex and
touches wantarray semantics (see `docs/wantarray-context.md`).
