# pack.t Remaining Failure Groups

**Status as of session 205:** 5634 pass, 91 fail, 8997 skip (14722 total)

Pack.t breakdown:
- **5634 tests actually pass** (not skip)
- **91 tests fail** (detailed below)
- **8997 tests skip** (see "Skipped Test Sections" below)

---

## Remaining Failures (91 tests)

### Group: U0/C0 Mode Switching Mid-Template (~18 tests)

**Tests:** 14045–14046, 14075, 14077, 14080, 14082, 14144–14147, 14195, 14197, 14200, 14202, 14264–14267

**Symptom:**
```
unpack("aU0C/UU", "b\0$U_1FFC_bytes")  → got: 'b,8188'  expected: 'b,225'
pack H5 C0 W returns expected value     → got: 'øù û'    expected: 'øùðû'
pack U0U C0 W should give 1+1 chars    → got: '3'        expected: '2'
```

**Root cause:** `U0` or `C0` appearing in the middle of a template should switch the encoding
mode (byte vs character) for subsequent format chars. PCL only handles `U0` as a top-level
prefix transformation on the whole input string (in `p_unpack`). When `U0` appears mid-template
(e.g. `aU0C/UU`), PCL doesn't switch mode — it keeps reading Unicode codepoints rather than raw
bytes.

**Fix area:** `_unpack_tmpl` / `_pack_tmpl` in `cl/pack-impl.pl` — threading a `$mode` flag
(`:byte` vs `:unicode`) through the loop, updated when `U0` or `C0` is encountered as a type char.
The 'U' format handler reads differently depending on mode.

**Difficulty:** Medium.

---

### Group: UTF-8 Upgrade Neutrality (~23 tests)

**Tests:** 14288, 14297, 14306, 14315, 14324, 14333, 14342, 14351, 14360, 14369, 14378, 14396,
14405, 14414, 14423, 14432, 14441, 14450, 14459, 14468, 14477, 14486, 14495

**Symptom:**
```
Simple s pack doesn't get upgraded    → got: something-upgraded  expected: same-bytes
Simple Q pack doesn't get upgraded    → ...
```
For formats s, S, i, I, l, L, j, J, f, d, F, q, Q, s!, S!, i!, I!, l!, L!, n!, N!, v!, V!.

**Root cause:** Perl maintains a per-string UTF-8 upgrade flag. When `utf8::upgrade($str)` is
called, the bytes change but the semantic value doesn't — pack should produce identical output
regardless of whether the source string is UTF-8-flagged. PCL strings are always Unicode (CL
has no byte/unicode duality), so there is no upgrade flag to track. Fixing requires either
emulating the flag (significant work) or always normalizing to bytes before packing.

**Difficulty:** Hard — requires architectural UTF-8 flag tracking.

---

### Group: `.` Format and `@!` with Multibyte Strings (~19 tests)

**Tests:** 14656, 14657, 14658, 14660, 14661, 14669, 14673, 14674, 14681, 14682, 14683, 14687,
14691, 14692, 14693, 14694, 14698, 14702, 14703

**Symptom:**
```
utf8 offset is relative to inner group  → got: '2'   expected: '6'
. relative to counted group, extend     → got: wrong  expected: correct
Test basic utf8 @!                      → fails
Proper error message ("'.' outside of string")  → got: ''
```

**Root cause:** `.` (position relative to group start) and `@!` (absolute byte offset) both
need byte-offset arithmetic when operating on multibyte UTF-8 strings. PCL does character-level
counting. The error message for `'.'` outside of string bounds is also missing.

**Difficulty:** Hard — requires byte vs char counting awareness with UTF-8 strings.

---

### Group: UTF-8 Byte Encoding (11 tests)

**Tests:** 14600, 14601, 14603, 14604, 14605, 14606, 14607, 14608, 14609, 14610, 14611

**Symptom:**
```
got: 'Ã¾'   expected: 'þ'    (UTF-8 double-encoding of high bytes)
```

**Root cause:** After `utf8::upgrade`, PCL double-encodes high bytes. Packing a downgraded
string vs an upgraded string should produce identical raw bytes, but PCL UTF-8-encodes the
output when the string is internally Unicode.

**Difficulty:** Hard — same root cause as upgrade neutrality group.

---

### Group: A\* Unicode Whitespace Stripping (5 tests)

**Tests:** 14628, 14629, 14630, 14631, 14632

**Symptom:**
```
normal A* strip leaves \xa0  → got: 'ab \nÂ '  expected: 'ab \n'
upgraded strings A* removes \xa0         → ...
upgraded strings A* removes all unicode whitespace → ...
```

**Root cause:** The `A` format strips trailing ASCII spaces and NUL bytes. For UTF-8-upgraded
strings, Perl also strips Unicode whitespace (U+00A0 non-breaking space, etc.). PCL's `A` format
only strips ASCII space (0x20) and NUL. Handling upgraded-string stripping requires UTF-8 flag
awareness.

**Difficulty:** Medium for \xa0 specifically; hard for full Unicode whitespace (upgrade-flag needed).

---

### Group: Slash/W/u Format and Error Message Mismatches (~10 tests)

**Tests:** 4131, 4166, 4167, 4170, 4171, 4175, 4176, 4264, 4265, 4391

**Symptom (several distinct sub-issues):**

- **Test 4131** (`got: '1'  expected: '0'`): Byte-order conflict check — PCL may be raising an
  error where Perl doesn't (or vice versa) for `(s<)>` style constructs.

- **Tests 4166, 4167** (`%vd` sprintf — `got: '1.20.300.4000'  expected: '%vd'`):
  The `%vd` vector-flag format in `sprintf` is not supported. PCL's sprintf doesn't handle
  the `v` flag, so the template is returned as-is with a "Redundant argument" warning.

- **Tests 4170, 4171** (`got: '196'  expected: '300'`; `got: undef  expected: '300'`):
  Slash-format unpack returning wrong element — likely the data-format count being applied
  wrong in `_unpack_tmpl`'s slash handler.

- **Tests 4175, 4176** (`pack 'W', 202` — `got: '@Ã'  expected: '@Ê'`; malformed UTF-8 error):
  `W` format should write a raw octet (byte). PCL UTF-8-encodes high bytes (202 → 0xC3 0x8A),
  producing 2 chars instead of 1 raw byte. Perl's `W` format writes the raw byte value.
  Test 4176 expects a "Malformed UTF-8" error which PCL doesn't raise.

- **Tests 4264, 4265** (`got: ''  expected: 'xyzzy'`; `got: undef  expected: 'ab'`):
  Byte-order conflict detection: some `(x>)` or `(x<)` constructs not raising the right error,
  causing the eval to succeed with wrong output instead of failing.

- **Test 4391** (`got: 'b'  expected: 'badc'`): Unpack `a/a*` or similar slash format returns
  only the first element instead of the full unpacked sequence.

**Difficulty:** Medium — mostly string-level fixes in `pack-impl.pl`. `%vd` requires new sprintf
handling; `W` raw-byte fix is straightforward; `@Ê` vs `@Ã` is a character-code issue.

---

### Group: `w` Format with Large Float (2 tests)

**Tests:** 238, 240

**Symptom:**
```
Should be able to pack 'w', 8.98e307  → got: 'Can only compress unsigned integers'
Round trip pack/unpack 'w' of 2**1023 → got: '-Inf%' precision difference
```

**Root cause:** PCL's `w` format (BER-compressed integer) rejects float arguments with "Can only
compress unsigned integers". But Perl converts float arguments to integer first if the value is
non-negative and representable. `8.98e307 = 2**1023` is a large but exact integer value;
Perl calls `IV_cast(NV)` which converts it. PCL's check fires before this conversion.

**Fix area:** `_pack_str_one` or `_pack_tmpl` `w` handler in `cl/pack-impl.pl`: call `int()` on
the argument before rejecting it; only die if `int($arg) != $arg` (fractional) or `$arg < 0`.

**Difficulty:** Easy (2 tests).

---

### Isolated / Miscellaneous (3 tests)

| Test | Description | Root Cause | Difficulty |
|------|-------------|-----------|------------|
| 24   | `pack` transpiler crash | PPI wraps `eval { pack ... }` condition in a `PPI::Structure::Condition` node; codegen doesn't handle it | Medium |
| 447  | `u` uuencode trailing chars | `¨  ` vs `   ` — trailing padding in the last uuencoded block is 2 chars wrong | Medium |
| 14616 | `pack N/S13 works` | `"@array[0..12]"` string interpolation generates `(p-aref @array (p-.. 0 12))` (scalar element) instead of array slice | Medium |

---

## Previously Fixed Groups

| Group | Tests Fixed | Session |
|-------|-------------|---------|
| A — `eval { }` list context not propagated | ~276 | 199 |
| B — Error message mismatches | ~32 | 196–198 |
| C — Sign extension `l!`/`s!`/`i!`/`j` formats | ~96 | 194 |
| D — `%N` checksum arithmetic overflow | ~32 | 196 |
| E — `f` single-precision float round-trip | ~33 | (via float overrides) |
| F — `/` counted-length format bugs | ~25 | 197–198 |
| G — `U` Unicode codepoint format | ~152 | 198–199 (partial) |
| H — `F<`/`D` long double checksum | ~16 | (skipped — D not in use) |
| I — `@`/`@!`/`W` byte-vs-char counting | ~106 | (partial — some remain) |
| K — 64-bit IV/NV precision (`~0`) | ~5 | 194 |
| L — `pack W` UTF-8 output | ~18 | (partial) |
| M — Small isolated | ~7 | Various |

---

## Easy Wins (Recommended Next Fixes)

| Tests | Fix | Effort |
|-------|-----|--------|
| 238, 240 | `w` format: call `int($arg)` before rejecting as non-integer | ~5 lines in `_pack_str_one` |
| 4175 | `W` raw-byte: `chr($val)` should write a byte, not UTF-8 encode it | Check `_pack_str_one` W handler |
| 4391 | Slash `a/a*` unpack wrong element | Debug `_unpack_tmpl` slash chain |
| 14616 | `"@arr[0..12]"` slice interpolation | Fix `StringInterpolation.pm` or `ExprToCL.pm` |
| 4264, 4265 | Byte-order conflict — eval succeeds with wrong value | `_pack_parse_mods` die logic |
| 14045–14046 | `U0` mid-template byte mode | Thread `$utf8_mode` flag through `_unpack_tmpl` |

---

## Skipped Test Sections (8997 tests)

These tests are skipped entirely and represent features PCL cannot test today:

### 1. `(?{code})` Regex Code Blocks — 8748 tests

The largest skip block in pack.t. The test setup uses `/(PATTERN)(?{$var{TYPE}=$^R})/`
to populate a `%val` hash mapping pack-format letters to numeric test values. CL-PPCRE
rejects `(?{code})` syntax, and `$^R` is never set, so all `%val` entries are undef.
Pack receives undef arguments → the x[TEMPLATE] count expressions compute junk byte counts.

**What it covers:** The 8748 tests exercise `x[SUBEXPR]` repeat-count notation for every
format letter (A, Z, a, c, C, W, B, b, H, h, s, v, n, S, i, I, l, V, N, L, p, P, f, F, d,
D, w, u, U, q, Q, j, J). These tests verify that `x[sizeof(TYPE)]` produces the correct
number of pad bytes for alignment.

**To fix:** Implement `(?{code})` regex code blocks (hard — requires mid-match side effects)
OR replace `%val` setup with a Perl pre-computation that doesn't use `(?{...})`.

### 2. `D` Format (Long Double) — 208 tests

`D` format is 80-bit extended-precision long double (x87 format). SBCL uses IEEE 64-bit
doubles with no 80-bit support. The skip fires when `pack("D", 12.34)` throws "Invalid type".

The skipped section (`skip "Long doubles not in use", 166`) tests:
- `pack("D", N)` / `unpack("D", str)` round-trips for values -(2^34) to 2^34
- `D>` big-endian and `D<` little-endian variants
- `(DcCD)>` and `(DcCD)<` groupings
- Byte-size `length(pack("D", 0)) == $Config{longdblsize}`

**Status:** Unfixable without XS/C FFI to do 80-bit float I/O. Correctly skipped.

### 3. `p`/`P` Pointer Format — 28 tests

`p` packs a C `char *` pointer (address of a string); `P` packs a `char *` with fixed length.
These store raw virtual memory addresses — no equivalent in garbage-collected CL.

The 28 skips cover:
- Basic `p`/`P` round-trips
- Error check for `P*` (no star allowed)
- [perl #131844] pointer overflow on 32-bit builds (4 tests, separately skipped)

**Status:** Correctly skipped. See `docs/not-supported.md`.

### 4. 32-bit Build Tests — 4 tests

`[perl #131844]` — pointer addition overflow test that requires a 32-bit pointer size.
PCL runs on a 64-bit platform, so `$Config{ptrsize} == 4` is false.

**Status:** Correctly skipped (platform constraint, not a PCL limitation).

---

## Summary Table (Current State)

| Category | Tests | Root Cause | Fix Difficulty |
|----------|-------|-----------|----------------|
| U0/C0 mode switching mid-template | 18 | Mode flag not threaded through template loop | Medium |
| UTF-8 upgrade neutrality | 23 | No UTF-8 flag in CL strings | Hard |
| `.`/`@!` with multibyte strings | 19 | Byte vs char counting in position formats | Hard |
| UTF-8 byte encoding | 11 | Double-encoding high bytes after upgrade | Hard |
| A\* Unicode whitespace | 5 | Only strips ASCII space/NUL, not Unicode WS | Medium–Hard |
| Slash/W/error-msg misc | 10 | Mixed: `%vd`, W raw-byte, slash chain, die logic | Easy–Medium |
| `w` format with float | 2 | Rejects float before int-conversion | Easy |
| Isolated (24, 447, 14616) | 3 | Transpiler node, uuencode padding, slice interp | Medium |
| **TOTAL** | **91** | | |
