# Pack.t Attack Plan — Next Session

**Current state** (session 199): pack.t — **117 failures**, 14605 pass, 8771 skip, 14722 total.

## Failure Groups and How to Attack Them

### Group A — `eval { }` list context ✅ FIXED (session 199)
**Tests:** 297–443 (step 2, 74 tests), 3511–3981 (pairs every 5, ~142 tests), plus some in 4235–4259 range  
**Fix:** `Pl/ExprToCL.pm` eval-block codegen. The three eval-block paths (`anon_sub`, `inline_lambda`, `func_ref`) now wrap `(p-eval-block ...)` with `(let ((*wantarray* $wa)) ...)` based on the node's context annotation. This is identical to the pattern already used for `do { }` func_ref blocks.  
**Root cause (fixed):** The eval block codegen emitted bare `(p-eval-block ...)` without binding `*wantarray*`. When in `(p-array-= @t ...)` context, `*wantarray*` was nil (scalar). `pl-p_unpack` saved `*pcl-caller-wantarray* = nil` at entry and returned only the first value.  
**Investigation result:** `do { }` blocks did NOT have this problem — they already used `(let ((*wantarray* $wa)) (funcall func-ref))` for the func_ref path.  
**Impact:** 623 → 117 failures (506 tests fixed).

### Group B — Error message mismatches (estimated ~32 tests)
**Tests:** Various in the 4130–4410 range.  
**Symptom:** Our error messages don't exactly match Perl's. For example:
- "Repeat count is negative" vs our variant
- "Invalid type ..." format differences
**Fix:** Compare each failing message against Perl's actual message and update the `die` strings in `pack-impl.pl`. Most are simple string changes.

### Group C — Already fixed (sign extension, session 194) ✅

### Group D — 32-bit checksum (investigated but not yet fixed)
**Tests:** ~32 tests  
**Symptom:** `unpack('%32c', ...)` gives wrong result.  
**Root cause:** 32-bit checksums use unsigned 32-bit arithmetic in Perl (truncates at 2^32) but our bignum arithmetic gives full-precision results.  
**Fix:** After checksum computation, mask to 32 bits: `return $r & 0xFFFFFFFF` for 32-bit checksums, or more generally cap at `2**$checksum_width` for widths ≥ 32.  
Wait — for widths ≤ 31, modulo already handles it. For widths ≥ 32, Perl uses native UV arithmetic which wraps at 2^32 (or 2^64 for 64-bit). The floor-division formula already handles modulo correctly, so this may already work. Needs investigation.

### Group E — Float checksum (fractional) — FIXED in session 196 ✅
**Fix:** `$checksum - int($checksum/$mod) * $mod` with `$q--` adjustment for negative. Handles both fractional floats and negative integers correctly.

### Group F — 'w' format with Math::BigInt (test 24)
**Tests:** 1–2 tests  
**Symptom:** `eval q{ use Math::BigInt; ... }` — PCL's string eval likely fails to load Math::BigInt, leaving `$y = ''`.  
**Root cause:** String eval subprocess may not find Math::BigInt in the Perl path, or the generated code doesn't handle BigInt objects as BER integers.  
**Investigation:** Check if Math::BigInt is installed in the PCL Perl environment. If so, check if `p-eval` handles `use` statements with class loading. This is a low-priority single test.

### Group G — U format (Unicode codepoints, ~152 tests)
**Tests:** Scattered, testing 'U' format pack/unpack.  
**Symptom:** 'U' format packs Unicode codepoints to UTF-8 bytes. Our implementation may return raw byte values instead of Unicode codepoints, or vice versa.  
**Fix location:** `_pack_utf8_char` and the 'U' unpack handler in `pack-impl.pl`.

### Group H — Long double 'F'/'D' format (~16 tests)
**Tests:** Tests with 'D' format (long double).  
**Root cause:** SBCL doesn't support 80-bit extended precision (long doubles). These are inherently non-fixable without C FFI support.  
**Action:** These tests will remain failing unless an XS bridge is built. Document as known limitation.

### Group I — UTF-8 string length counting (~106 tests)
**Tests:** Tests involving strings with non-ASCII chars where length()/byte count differs.  
**Root cause:** PCL's `length()` returns character count, but pack needs BYTE count for some operations. Requires `use bytes` semantics which is not yet supported.  
**Action:** These require `use bytes` or `Encode::encode_utf8` semantics. Significant work.

## Quick Wins — Recommended Order

1. **Group B (error messages, ~20 remaining tests)** — mechanical, low risk  
   - Run `./runt pack 2>/dev/null | grep "not ok"` then look at the diagnostic output  
   - Find each message in `pack-impl.pl` and align with Perl's exact string

2. **Group D (32-bit checksum, ~32 tests)** — verify if already fixed  
   - Run `perl -e 'printf "%d\n", unpack("%32c", pack("c5", -128,-1,0,1,127))'` and compare with PCL  
   - If the floor-division fix (session 196) already handles this, these tests may now pass

3. **Group G (U format, ~244 tests)** — needs `_pack_utf8_char` and unpack U handler fixes

## Debugging Tools

```bash
# Quick single test investigation
./runt pack 2>/dev/null | grep -A3 "not ok 297"

# Check specific pattern
perl -ne 'print if /^not ok/' /tmp/pack-output.txt | head -50

# Compare a specific formula in Perl vs PCL
echo 'my $r = (-1) % 65536; print "$r\n"' | perl       # should be 65535
echo 'my $r = (-1) % 65536; print "$r\n"' | ./pl2cl | sbcl --noinform --load cl/pcl-runtime.lisp 2>/dev/null | tail -3
```

## Session 199 Changes

- **Group A (eval list context) FIXED**: `Pl/ExprToCL.pm` — eval-block codegen now wraps `(p-eval-block ...)` with `(let ((*wantarray* $wa)) ...)` based on node context annotation. All three paths (`anon_sub`, `inline_lambda`, `func_ref`) fixed. **623 → 117 failures** (506 tests fixed).

## Session 198 Changes

- **POSIX char classes**: `perl-regex-to-ppcre` now translates `[:alpha:]`, `[:print:]` etc. to Unicode hex ranges. CL-PPCRE 2.1.2 doesn't support POSIX class syntax. Fixes 26+ tests.
- **`do-regex-subst` fix**: Was bypassing `perl-regex-to-ppcre` — now calls it on the pattern. Fixes test 4370.
- **Byte-order conflict** in `_pack_parse_mods`: `(s<)>` now dies "Can't use '>' in a group with different byte-order". Fixes tests 4273-4278.
- **Slash depth die/last** (`_unpack_tmpl`): Outer slash count overflow at top-level → `last` (silent). Nested → `die`. Controlled by `$depth` param. Also fixed recursive `S/(...)` call to pass `$depth + 1`. Fixes 4130/4132 regression, keeps 4395/4396 passing.
- **Slash final-field B/b/H/h/U/u**: Added these to the dispatch in the slash final-field handler. Fixes `a/a*/b*` format (tests 4156/4158).
- **Override section now at line 4248** of `cl/pcl-pack.lisp`.
- **`pcl-pack.lisp` rebuilt**.

### DONE: Group A — eval block context propagation ✅ (session 199)

Fixed in `Pl/ExprToCL.pm`. The eval-block codegen now wraps `(p-eval-block ...)` with the correct `*wantarray*` binding based on the node's context annotation — same pattern as `do { }` func_ref blocks.

**Investigation**: `do { }` did NOT have this problem (already uses wantarray wrapper). Only `eval { }` was missing it.

## Session 197 Changes

- **Whitespace rule**: Removed `_pack_skip_ws` between type char and count in `_pack_parse_count`, `_pack_tmpl`, `_unpack_tmpl`, `_pack_template_size`. `'A *'`/`'A 4'`/`'A ![4]'` now die "Invalid type".
- **Comma warning**: `$pcl_pack_comma_warned` flag, reset per-call via `local` in `p_pack`. `_pack_skip_ws` warns once.
- **Z*/A* slash**: Z case in slash count position (`_pack_tmpl` around line 402). Z case in unpack slash count reader (`_unpack_tmpl`). Tests 2/3/4 of `test_pack_z.t` pass.
- **Z*/A* test 1 root cause found**: Pack produces CORRECT bytes. Failure is `[[:print:]]` treating NUL (chr 0) as printable → `s/[^[:print:]]/./g` leaves NUL in string → prints as space. **Fix needed**: `[[:print:]]` must exclude chr(0)–chr(31). POSIX: printable = 0x20–0x7e.
- **PExpr.pm line 904**: `Use of uninitialized value in string eq` — investigate next session.

## Session 196 Changes

- **Checksum formula**: Changed from `$checksum % $mod` to floor-division formula that handles both negative integers AND float checksums with fractional parts.
- **Slash format validation**: Check for count AFTER `/` (not count of preceding type).
- **'w' format eE check**: Reject string notation ("1e21") only when value ≥ 2^64.
- **Copyright header**: Added to `cl/pack-impl.pl`.

## Rebuild Procedure (after editing pack-impl.pl)

Override section start: find with `perl -ne 'print "$.: $_" if /^\(defun pl-_pack_float32/' cl/pcl-pack.lisp`
(was line 4248 after session 198).

```bash
cd /home/bernt/pcl
./pl2cl < cl/pack-impl.pl > /tmp/pack-generated.lisp
# Save overrides — find start line first (see above)
sed -n 'LINE,$p' cl/pcl-pack.lisp > /tmp/pack-overrides.lisp
# Assemble
head -12 /tmp/pack-generated.lisp > /tmp/pack-new.lisp
sed -n '15,$p' /tmp/pack-generated.lisp | head -n -2 >> /tmp/pack-new.lisp
cat /tmp/pack-overrides.lisp >> /tmp/pack-new.lisp
# Check parens
perl .claude/hooks/split-lisp.pl /tmp/pack-new.lisp 2>/dev/null || \
  perl -e '...' /tmp/pack-new.lisp  # use the checker from CLAUDE.md
# Install
cp /tmp/pack-new.lisp cl/pcl-pack.lisp
# Verify
perl sweep-perl-tests.pl --jobs 1 perl-tests/pack.t 2>/dev/null | grep "pack"
```
