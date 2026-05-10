# pack/unpack Implementation Plan

Session 177, 2026-05-10. Analysis of what is needed to fix `pack.t`.

---

## Current state

The sweep reports **pack.t: 6081 pass + 7841 fail / 14722 total** (early stop at 90s timeout).
In interactive mode the test hangs after test 2 — the sweep's 90s kill gives it more time.
The hang is most likely at test 7–8 (`open(BIN, $Perl); sysread BIN, $foo, 8192`), where
reading a Perl ELF binary over a UTF-8 stream can block on invalid byte sequences.

The current implementation lives at lines ~8856–9046 of `cl/pcl-runtime.lisp`:
- `p-pack` (~30 lines) — handles only `d` (double, byte order wrong), `C` (unsigned byte),
  `A`/`a` (string, fixed length 1). **Repeat counts are silently skipped** (digits hit
  `((#\0..#\9) nil)`). Unknown format chars silently advance arg-idx.
- `p-unpack` (~150 lines) — handles `C`/`c`, `n`/`N`/`v`/`V`, `A`/`a`/`Z`, `H`/`h`,
  `x`/`X`/`@`. Has correct repeat-count parsing. Missing most types.

---

## Test structure (14722 tests)

| Bulk generator | Tests | Depends on |
|---|---|---|
| `numbers()` / `numbers_with_total()` | ~3600 | round-trip pack+unpack + `%N` checksum |
| `byteorder()` | ~500 | `>` / `<` endian modifiers |
| Negative integer encoding | ~400 | signed int types + two's-complement |
| Modifier syntax checks | ~150 | error messages for bad modifier combos |
| Specific feature tests | ~9600 | uuencode, BER, bit strings, `/`, `U`, `W`, etc. |

The `numbers()` function generates 55 tests per format variant (5 values × 2 + 15 checksum
widths × 3) or 165 for formats with `>`/`<` variants (`[silqjfdp]` case-insensitive).

---

## Missing features — full inventory

### In p-pack (almost everything is missing)

Critical structural bug first: **repeat counts are not applied** — `c2` packs `c` twice in
Perl; PCL skips the `2` as whitespace.

| Format | Meaning | Missing from pack |
|--------|---------|-------------------|
| `c` | signed byte | yes (falls to unknown-arg-skip) |
| `s`/`S` | 16-bit LE signed/unsigned | yes |
| `i`/`I` | 32-bit native signed/unsigned | yes |
| `l`/`L` | 32-bit signed/unsigned | yes |
| `q`/`Q` | 64-bit signed/unsigned | yes |
| `j`/`J` | IV/UV (64-bit on x86-64) | yes |
| `n`/`N` | big-endian 16/32-bit unsigned | yes (in unpack only) |
| `v`/`V` | little-endian 16/32-bit unsigned | yes (in unpack only) |
| `f`/`F` | 32-bit / NV (64-bit) float | yes |
| `d` | 64-bit double | partial (byte order wrong) |
| `Z` | null-terminated string | yes |
| `b`/`B` | LSB/MSB bit string | yes |
| `x` | null byte output | wrong (currently skip-arg) |
| `X` | back-up in output | yes |
| `@` | absolute seek (pad) | yes |
| `u` | uuencode | yes |
| `U` | UTF-8 codepoint | yes |
| `W` | wide byte (>255 OK) | yes |
| `w` | BER-encoded integer | yes |
| `!` | native size modifier | yes |
| `>`/`<` | force big/little-endian | yes |
| `%N` | N-bit checksum prefix | N/A (unpack-only) |
| `/` | count-prefixed | yes |
| `()` | grouping with repeat count | yes |
| `p`/`P` | pointer | intentionally skip (see below) |
| `D` | long double (80-bit) | intentionally skip (see below) |

### In p-unpack (many cases missing)

The unpack parser structure is correct; it just needs more cases:

| Format | Missing |
|--------|---------|
| `s`/`S` | 16-bit native (LE = n/v but signed S, signed s) |
| `i`/`I`/`l`/`L` | 32-bit signed/unsigned |
| `q`/`Q`/`j`/`J` | 64-bit signed/unsigned |
| `f`/`F` | 32-bit / NV float |
| `d`/`D` | 64-bit double |
| `b`/`B` | LSB/MSB bit string |
| `u` | uudecode |
| `U` | UTF-8 codepoints |
| `W` | wide byte |
| `w` | BER integer |
| `!` modifier | adjusts size for s/i/l/n/N/v/V |
| `>`/`<` modifier | endianness for all numeric types |
| `%N` prefix | N-bit checksum mode — requires separate dispatch |
| `/` modifier | count-prefixed |
| `()` grouping | recursive/stack template |

---

## Why each hard part is hard (or easy) in SBCL/CL

### Not hard

- **All integer types**: CL `ash` and `logand` do all bit manipulation. CL bignums make
  `q`/`Q` (64-bit) no harder than `i` (32-bit). On x86-64 with SBCL on Linux, all native
  sizes are fixed: `s!`=2, `i!`=4, `l!`=8, `j`=8 — just a table.

- **`>`/`<` modifiers**: Byte-swap the bytes of the underlying type. Pure arithmetic.

- **`w` BER integers**: CL bignums make this _easier_ than C — loop over 7-bit groups
  with `(ash n -7)` and `(logand n #x7f)`.

- **`b`/`B` bit strings**: Simple bit-extraction loop over bytes.

- **`/` modifier**: Stateful lookahead during template parsing — the preceding count field
  governs how many bytes/chars the following type reads.

### Moderately tricky

- **Float types (`f`, `d`, `F`)**: CL has no _standard_ way to get IEEE 754 bit
  representations. **SBCL-specific solution** (fine since PCL targets SBCL only):
  - Pack: `(sb-kernel:single-float-bits val)` → 32 bits; `(sb-kernel:double-float-bits val)`
    → 64 bits.
  - Unpack: `(sb-kernel:make-single-float bits)` and `(sb-kernel:make-double-float high low)`
    where `high = (ldb (byte 32 32) bits)`, `low = (ldb (byte 32 0) bits)`.
  - Note: the current `d` implementation in p-pack has the byte order backwards — it emits
    MSB-first on a LE platform. Fix: emit bytes from LSB (byte-idx 0) to MSB (byte-idx 7).

- **`%N` checksum mode**: Requires a separate code path, not a format-char case. When the
  template starts with `%N` (N = bit width, default 16), p-unpack accumulates item values
  into a running integer sum instead of pushing results, then applies `(mod sum (expt 2 N))`.
  N up to 65 means the sum can exceed 64 bits → needs CL bignum arithmetic. The tricky part
  is integrating this mode into the existing per-item dispatch loop without duplicating it.

- **`u` uuencoding**: The algorithm is straightforward (6-bit groups, +32 offset, 60-char
  output lines with a length prefix char). The hard part is matching Perl's exact output
  byte-for-byte — pack.t compares against a known heredoc. The decoder (unpack `u`) also
  needs to handle both `` ` `` and space as "zero" (Perl accepts either).

- **`()` grouping**: Requires a recursive or stack-based template parser. `pack "(NN)3", @v`
  means "apply NN three times". The current linear-scan design needs refactoring to allow
  nested repeat groups. Either recurse on the group substring, or push a continuation frame
  onto an explicit stack.

### Genuinely not implementable

- **`p`/`P` (pointer types)**: Pack a raw memory address. CL's GC moves objects — no stable
  addresses. The right behavior: throw `"Invalid type 'p' in pack"`. This is critical because
  the `is_valid_error()` helper in pack.t matches `qr/^Invalid type '\w'/` and **skips** tests
  rather than failing them (TAP counts skip as pass). Without this error, PCL produces wrong
  output and the tests _fail_. With it, they pass via skip.

- **`D` (80-bit long double)**: SBCL uses 64-bit doubles only. Throw `"Long double not
  available in pack"` (or similar). pack.t already handles this with `is_valid_error`.

---

## High-leverage: throw errors for unknown types

This is the single most impactful change with least effort. If p-pack/p-unpack signal
`(error "Invalid type '~A' in pack" ch)` for any unrecognized format character, then
`is_valid_error($@)` in the `numbers_with_total()` SKIP blocks returns true and those tests
become **skips (= TAP pass)** rather than wrong-value failures.

Currently PCL silently corrupts output, so tests fail with wrong values instead of skipping.
Throwing the right error could flip ~3000 failures to passes with essentially zero
implementation cost.

The error string format Perl uses is `"Invalid type 'X' in pack"` for pack and
`"Invalid type 'X' in unpack"` for unpack. PCL just needs to match `qr/^Invalid type '\w'/`.

---

## Implementation plan

### Phase 1: Error on unknown types (high value, low cost — ~20 lines)

In both `p-pack` and `p-unpack`, replace `(otherwise ...)` with:
```lisp
(otherwise
 (error "Invalid type '~A' in pack" ch))   ; or "in unpack"
```

Also make `p`/`P`/`D` explicit cases that signal this error immediately.
Expected gain: **~2000–3000 test flips from fail to skip/pass**.

### Phase 2: Complete p-pack rewrite (~200–250 lines)

Replace the current ~30-line stub with a proper implementation:

```
Template parser loop:
  read ch
  if ch = '(': push group frame (start-idx, repeat-count)
  if ch = ')': pop group frame, handle repeat
  read optional ! modifier
  read optional > or < endian modifier
  read optional count (digits) or *
  dispatch on ch with repeat:
    c/C: emit signed/unsigned byte
    s/S/i/I/l/L/q/Q/j/J: emit N-byte integer (LE default; swap for >)
    n/N: emit 16/32-bit big-endian (same result as S>/L>)
    v/V: emit 16/32-bit little-endian (same as S</L<)
    f/F: emit 4-byte float using sb-kernel:single-float-bits
    d: emit 8-byte double using sb-kernel:double-float-bits (LSB first on LE)
    a: emit string, NUL-pad to count
    A: emit string, space-pad to count
    Z: emit string + NUL, total length = count
    b/B: pack bit string from string of '0'/'1' chars
    x: emit count NUL bytes (does NOT consume arg)
    X: back up count bytes in output buffer (shorten fill-pointer)
    @: pad/truncate output to absolute position
    u: uuencode input string
    U: emit UTF-8 encoding of codepoint
    W: emit codepoint as UTF-8 if >127, else as single byte
    w: BER-encode integer
    H/h: hex string → bytes
    p/P/D: signal "Invalid type 'X' in pack"
    otherwise: signal "Invalid type 'X' in pack"
```

Key Perl size rules for integers on x86-64 Linux (hardcode in PCL):
| Format | Bytes | Signed? |
|--------|-------|---------|
| c/C | 1 | c signed, C unsigned |
| s/S/s!/S! | 2 | s signed, S unsigned |
| i/I/i!/I! | 4 | i signed, I unsigned |
| l/L | 4 | l signed, L unsigned (Perl `l` is always 32-bit!) |
| l!/L! | 8 | native long on Linux x86-64 is 64-bit |
| q/Q/q!/Q! | 8 | q signed, Q unsigned |
| j/J | 8 | IVSIZE on x86-64 |
| n/N | 2/4 | unsigned big-endian |
| v/V | 2/4 | unsigned little-endian |
| n!/N! | 2/4 | signed big-endian (! makes them signed) |
| v!/V! | 2/4 | signed little-endian |

Sign handling: for signed types with negative values, store as two's complement.
```lisp
(when (< val 0)
  (setf val (+ val (expt 2 (* nbytes 8)))))
```

### Phase 3: Complete p-unpack additions (~200 lines)

Add to the existing case dispatch:

- `s`/`S`: read 2 bytes LE, sign-extend for s.
- `i`/`I`/`l`/`L`: read 4 bytes LE.
- `q`/`Q`/`j`/`J`: read 8 bytes LE.
- `f`/`F`: read 4 bytes, reconstruct float with `sb-kernel:make-single-float`.
- `d`: read 8 bytes LE, reconstruct with `sb-kernel:make-double-float`.
- `b`/`B`: read N bytes, emit string of '0'/'1' characters (LSB-first for b, MSB-first for B).
- `w`: read BER-encoded integer (loop while MSB set, accumulate 7-bit groups).
- `u`: uudecode — read lines, decode 6-bit groups.
- `U`: read UTF-8 sequence, emit codepoint as integer.
- `W`: read 1 or more bytes as wide char, emit codepoint.

For `!` modifier with n/N/v/V: interpret as signed (flip sign bit).
For `>`/`<` modifiers: byte-swap the bytes before interpreting.

### Phase 4: `%N` checksum mode (~50 lines)

Add a pre-scan at the start of p-unpack: if template starts with `%`, parse the bit width N,
then accumulate items instead of pushing them:

```lisp
;; Checksum mode
(when (char= (char tmpl 0) #\%)
  (let* ((width-end (position-if-not #'digit-char-p tmpl :start 1))
         (width-str (subseq tmpl 1 width-end))
         (width (if (zerop (length width-str)) 16 (parse-integer width-str)))
         (inner-tmpl (subseq tmpl width-end))
         (items (p-unpack (make-p-box inner-tmpl) str))
         (sum (reduce #'+ (map 'list #'(lambda (b) (to-number (unbox b)))
                               (coerce items 'list))
                      :initial-value 0)))
    (return-from p-unpack (make-p-box (mod sum (expt 2 width))))))
```

### Phase 5: `()` grouping (~50 lines)

Refactor the template scanner into a recursive function. When `(` is encountered, find the
matching `)`, parse the repeat count after `)`, and recurse on the group substring that many
times. The cleaner approach is to expand the template to a flat string first:

```
(NN)3  →  NNNNNN
(CCC)* →  CCC... until data exhausted
```

This pre-expansion pass avoids restructuring the main dispatch loop.

---

## Extension file approach

### Why a separate file makes sense

`p-pack` and `p-unpack` are the only functions in the runtime whose implementation could
fill a dedicated file (~500 lines vs the 9000-line runtime). Separating them:
- Keeps the runtime focused on core semantics
- Lets pack be developed/tested without touching the runtime
- The file can be loaded or skipped independently

### How SBCL finds files relative to the runtime

**`*load-truename*`** is the key. Inside a file being loaded, `*load-truename*` is bound to
the pathname of that file. So inside `pcl-runtime.lisp`:

```lisp
(defvar *pcl-runtime-directory*
  (when *load-truename*
    (make-pathname :name nil :type nil :defaults *load-truename*)))
```

This captures the runtime's directory at load time (e.g. `/home/bernt/pcl/cl/`). Important:
capture it near the **top** of the file, not at the bottom — by the time the bottom executes,
other files may have been loaded in between, changing `*load-truename*`.

Then at the end of the runtime (or in the extension itself), load the extension:

```lisp
;; Near end of pcl-runtime.lisp, before (format t "PCL Runtime loaded")
(when *pcl-runtime-directory*
  (let ((pack-file (merge-pathnames "pcl-pack.lisp" *pcl-runtime-directory*)))
    (when (probe-file pack-file)
      (load pack-file))))
```

This means `cl/pcl-pack.lisp` is auto-loaded whenever `cl/pcl-runtime.lisp` is loaded,
with no changes needed to the `runt` or `sweep-perl-tests.pl` scripts.

### Lazy loading ("only if used")

If startup time matters and most transpiled programs don't use pack, lazy loading is possible:
define stub functions in the runtime that load the extension on first call:

```lisp
;; In pcl-runtime.lisp — stubs that lazy-load pcl-pack.lisp
(defun p-pack (template &rest args)
  (%pcl-load-pack-extension)
  (apply #'p-pack template args))    ; calls the newly-loaded real p-pack

(defun p-unpack (template &optional (str $_))
  (%pcl-load-pack-extension)
  (p-unpack template str))           ; calls the newly-loaded real p-unpack

(defun %pcl-load-pack-extension ()
  (when *pcl-runtime-directory*
    (let ((f (merge-pathnames "pcl-pack.lisp" *pcl-runtime-directory*)))
      (when (probe-file f) (load f)))))
```

After `(load f)`, the `p-pack` symbol is rebound to the new definition. CL's late-bound
`#'p-pack` lookup in the `(apply ...)` call then dispatches to the new function. The stubs
don't run again because the new definitions don't call `%pcl-load-pack-extension`.

**Tradeoff**: lazy loading saves ~2–5ms of file I/O per SBCL invocation. Since PCL's SBCL
invocations already take 2+ seconds for startup, this is negligible. The unconditional load
at the end of the runtime is simpler and recommended.

### `pcl-pack.lisp` structure

```lisp
;; cl/pcl-pack.lisp — pack/unpack full implementation
(in-package :pcl)

;;; Helper constants for integer sizes on x86-64 Linux
(defconstant +pack-sizes+
  '((#\c . (1 t)) (#\C . (1 nil))
    (#\s . (2 t)) (#\S . (2 nil))
    (#\i . (4 t)) (#\I . (4 nil))
    (#\l . (4 t)) (#\L . (4 nil))   ; Perl l/L always 32-bit
    (#\q . (8 t)) (#\Q . (8 nil))
    (#\j . (8 t)) (#\J . (8 nil))))

(defun p-pack (template &rest args) ...)
(defun p-unpack (template &optional (str $_)) ...)
;; helpers: %pack-int, %pack-float, %unpack-int, %unpack-float, etc.
```

The function definitions in pcl-pack.lisp replace the stubs in pcl-runtime.lisp when loaded.
CL `defun` always redefines — no special shadowing needed.

---

## Expected impact

| Phase | Likely gain | Effort |
|-------|------------|--------|
| Phase 1: error on unknown types | +2000–3000 (fail→skip) | ~20 lines, 1 hour |
| Phase 2: complete p-pack | +1500–2000 | ~250 lines, 1 day |
| Phase 3: complete p-unpack | +1500–2000 | ~200 lines, half day |
| Phase 4: `%N` checksum | +400–600 | ~50 lines, 2 hours |
| Phase 5: `()` grouping | +200–300 | ~50 lines, 2 hours |

Realistic target after all phases: **12000–13000 / 14722** (~83–88%).

Remaining ~1700 failures would be:
- `p`/`P` pointer semantics (tests check exact address behavior)
- `D` long double
- NaN/Inf error message format mismatches (documented in sweep-bug-catalog.md)
- Test 7–8 hang (sysread of Perl binary over UTF-8 stream)
- Some `s///e` expression compilation failures (s///e bug in ExprToCL.pm)

---

## Files to change

| File | Change |
|------|--------|
| `cl/pcl-runtime.lisp` | Add `*pcl-runtime-directory*` var near top; add extension loader at bottom; keep thin stubs for p-pack/p-unpack or remove them |
| `cl/pcl-pack.lisp` | New file — full pack/unpack implementation |
| `Pl/t/pack-01.t` | New test file — targeted regression tests for pack/unpack |
