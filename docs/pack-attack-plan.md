# Pack.t Attack Plan — Next Session

**Current state** (session 196): pack.t — **623 failures**, 14099 pass, 8771 skip, 14722 total.

## Failure Groups and How to Attack Them

### Group A — `eval { }` list context (estimated ~216 tests)
**Tests:** 297–443 (step 2, 74 tests), 3511–3981 (pairs every 5, ~142 tests)  
**Symptom:** `my @t = eval { unpack(...) }` — `@t` gets only 1 element instead of N.  
**Root cause:** `p-eval-block` in `pcl-runtime.lisp` does not propagate list context into the block. `unpack` runs in scalar context and returns only the first unpacked value.  
**Fix location:** `p-eval-block` (around line 5263 of `pcl-runtime.lisp`). Need to propagate `*wantarray*` binding through the `handler-case` body.  
**Warning:** This interacts with the wantarray system. Read `docs/wantarray-context.md` before touching it. The fix needs to be carefully scoped to not break the existing VOID_CTX behavior.

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

1. **Group B (error messages, ~32 tests)** — mechanical, low risk  
   - Run `./runt pack 2>/dev/null | grep "not ok"` then look at the diagnostic output  
   - Find each message in `pack-impl.pl` and align with Perl's exact string

2. **Group A (eval list context, ~216 tests)** — high impact  
   - Carefully modify `p-eval-block` to propagate `*wantarray*`  
   - Test thoroughly since this touches many code paths

3. **Group D (32-bit checksum)** — verify if already fixed, ~32 tests  
   - Run `perl -e 'printf "%d\n", unpack("%32c", pack("c5", -128,-1,0,1,127))'` and compare with PCL  
   - If the floor-division fix (session 196) already handles this, these tests may now pass

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

## Session 196 Changes

- **Checksum formula**: Changed from `$checksum % $mod` to floor-division formula that handles both negative integers AND float checksums with fractional parts.
- **Slash format validation**: Check for count AFTER `/` (not count of preceding type).
- **'w' format eE check**: Reject string notation ("1e21") only when value ≥ 2^64.
- **Copyright header**: Added to `cl/pack-impl.pl`.

## Rebuild Procedure (after editing pack-impl.pl)

```bash
cd /home/bernt/pcl
./pl2cl < cl/pack-impl.pl > /tmp/pack-generated.lisp
# Save overrides (check line number — search for "MANUAL OVERRIDES" or first "(defun pl-_pack_float32")
grep -n "^;; The transpiled stubs" cl/pcl-pack.lisp  # find line N
sed -n 'N,$p' cl/pcl-pack.lisp > /tmp/pack-overrides.lisp
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
