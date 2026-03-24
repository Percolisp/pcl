# bop.t Analysis (updated session 97, 2026-03-24)

510 tests. **22 pass** (up from 13 before crash fix). No longer crashes.

## Status by section

### Section 1: Basic integer bitwise (tests 1–15)
Tests 1–6, 8–12, 15 pass. Failures:
- Test 7: `~0 > 0 && do { use integer; ~0 } == -1` — `use integer` ~0 should be -1 (signed)
- Tests 13–14: `use integer` with large negative shifts should fill with sign bit
- Tests 16–19: unknown — need investigation

### Section 2: String bitwise — char-by-char (tests 21–32) — NOT YET FIXED
`"AAAAA" & "zzzzz"` must AND each character position, returning a same-length string.
PCL's `p-band`/`p-bor`/`p-bxor` coerce to number, return 0. Fix: detect string operands
and do char-by-char CL ops. User confirmed this should be supported.

Tests 21–32 all fail. Tests 27–32 use `_and`/`_oar`/`_xor` subs with `($)` prototype
— prototype now correctly limits args (fix below), but string bitwise still wrong.

### Section 3: User-sub prototype crash — ✅ FIXED (session 97)
`sub _and($) { ... }` — prototype `($)` means one scalar arg.
`is _and 0, '0', 'str'` now correctly parses as `is(_and(0), '0', 'str')`.

**Root cause**: `handle_subcalls` in `Pl/PExpr.pm` wasn't consulting prototype `min_params`
when deciding how many arguments to consume for a no-paren user sub call.

**Fix**: Added `_proto_max_args` helper and arg-limiting code in `handle_subcalls`.
- `_proto_max_args` returns the fixed arg count for user prototypes with no `@`/`%`/`*` params
- Returns `undef` for built-in prototypes (they lack `min_params`) — excludes `open`, `close`, etc.
- Scans forward counting commas to find the Nth arg boundary, sets `$end_pars` accordingly
- Tests: `Pl/t/bop-01.t` (7 tests, all passing)

### Section 4–5: COW numeric bitwise (tests 36–46)
Tests 36–38 pass. Tests 39–46 fail. Need investigation.

### Section 6: tie / double-magic (~120 tests)
`tie $x, "main", 1` — PCL has no tie. FETCH/STORE call counting tests.
Major feature, not worth implementing for v1.

### Section 7: UTF-8 flag (~10 tests)
`utf8::is_utf8()` — not-supported (PCL has no per-scalar UTF-8 flag).

### Section 8: `use feature "bitwise"` + `&.`/`|.`/`^.`/`~.` (~45 tests)
New string-force operators from Perl 5.22. Not implemented in PCL.
`use feature "bitwise"` also changes `&`/`|`/`^` on strings to force numeric.

### Section 9: Ref/object bitwise + `$SIG{__WARN__}` counting (~120 tests)
Bitwise ops on undef, refs, globs, objects. `$SIG{__WARN__}` call counting.
Depends on string bitwise + $SIG{__WARN__} call protocol being correct.

### Section 10: Overload + block eval
`use overload q/|/ => sub { "y" }` — bitwise overloading. Block eval supported,
but error message matching fails.

### Section 11: Negative shifts + use integer arithmetic right shift (~25 tests)
`0x7b << -4` should equal `0x7b >> 4` (negative = reverse direction).
`use integer; -1 >> 1` should be -1 (arithmetic fill), not 0.

### Section 12: String eval for error testing (~15 tests)
`eval '$_ = "\xFF" & "\x{100}"'` — tests fatalization of >0xFF codepoints in string bitwise.

### Sections 13–20: fresh_perl_is, pack "P", version objects, Config
Mostly untestable without those features.

## Priority fixes for next session
1. **Section 2 string bitwise** — `p-band`/`p-bor`/`p-bxor` need string-context detection;
   when both operands are strings, do char-by-char CL bitwise ops (logand/logior/logxor
   on `char-code` per position, pad shorter operand with NUL, result length = shorter)
2. **Section 1 tests 16–19** — investigate what these test
3. **Section 4–5 tests 39–46** — investigate COW numeric bitwise failures
4. **Section 11 negative shifts** — `(ash val (- shift))` when shift < 0
5. **Section 11 use integer arithmetic right shift** — `(ash val shift)` for negative vals
   is already arithmetic in CL (fills with sign bit), so this may already work
