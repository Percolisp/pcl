# PCL: Features Deliberately Not Supported

This document lists Perl behaviours that PCL intentionally does not emulate,
along with the rationale.  Tests covering these features are commented out in
the `perl-tests/` files.

---

## Interned boolean constants (`!0` / `!1` identity)

**Perl behaviour:** `!0` and `!1` return a single globally-shared, read-only
scalar each time.  Because the same scalar is returned on every call, taking a
reference (`\!0`) gives the same address, and trying to modify the value
through `for (!0) { $_ = 43 }` throws "Modification of a read-only value".

**PCL behaviour:** `pl-!` and `pl-not` return a fresh CL value (`1` or `""`)
on every call.  References to successive calls are distinct, and the values are
mutable.

**Rationale:** This is a CPython-style implementation detail of the Perl
interpreter.  Real application code never relies on the reference identity of
boolean results; the feature exists only so that Perl can avoid allocating a
new scalar for every negation.  Replicating it in CL would require a global
constant table and read-only box semantics, with no practical benefit for
running CPAN modules.

**Affected tests:** `perl-tests/not.t` tests 21–24 (commented out).

---

## Read-only constants via `\undef` stash tricks

**Perl behaviour:** `BEGIN { $::{z} = \undef }` creates a constant slot in the
symbol table pointing to the undef scalar, making `z` a read-only constant.
Attempts to assign to it throw "Modification of a read-only value".

**PCL behaviour:** Stash manipulation (`%::`) is not fully supported; the
read-only flag on scalars is not emulated.

**Affected tests:** `perl-tests/undef.t` tests 16–18 (need `$SIG{__WARN__}`
and read-only scalars).

---

## Error messages: no "at FILE line N" location info

**Perl behaviour:** Die/warn/croak messages append `" at FILE line N.\n"` when
the message does not already end with a newline.  `die if $@` propagates with
`"... propagated at FILE line N."`.

**PCL behaviour:** PCL does not track source file/line number at runtime.  Error
messages have the same text content, but the `" at FILE line N."` suffix is
never appended.

**Rationale:** CL does not naturally expose Perl-compatible source location
metadata at runtime.  Application code adds `\n` to its own messages anyway;
only internal test machinery needs the exact suffix.

**Affected tests:** Many `perl-tests/` files comment out tests that check the
exact `" at FILE line N"` format (e.g. `warn.t`, `die.t`, `chop.t`).

---

## Unicode semantics differences

PCL uses SBCL's native Unicode strings and CL-PPCRE's regex engine.  These
diverge from Perl in several respects:

- **`utf8::encode` / `utf8::decode`**: Perl has an internal UTF-8 flag per
  scalar that can be toggled.  CL strings are always Unicode; the flag does not
  exist.  Tests that call `utf8::encode` and then compare the *byte* encoding to
  the *character* string are not meaningful in PCL.

- **Multi-character case mappings**: `uc("\x{DF}")` in Perl returns `"SS"` (two
  characters).  SBCL's `string-upcase` returns `"SS"` too, but `uc("\x{587}")`
  (Armenian ligature) titlecase expansion differs.  CL-PPCRE may not apply
  context-sensitive sigma lowercasing (`\x{3C2}` vs `\x{3C3}`).

- **`\p{IsWord}` and Unicode properties in CL-PPCRE**: `\p{IsWord}` does not
  reliably match non-ASCII word characters in cl-ppcre, whereas Perl's regex
  engine handles the full Unicode word-character set.

- **`use bytes`**: Perl's `use bytes` pragma forces byte-level string operations.
  PCL does not implement `use bytes`.

- **`pack 'U', N` / `chr` as UTF-8 flag**: `pack('U', 0)` produces a
  UTF-8-flagged empty string in Perl; PCL's `pl-pack` does not replicate the
  flag.

**Affected tests:** Unicode-sensitive tests in `perl-tests/lc.t` (tests 57, 59,
61, 63, 66, 71, 73, 74, 89–121, 127–131) and `perl-tests/chop.t` (the
`@chars` loop generating `start=N end=N` tests, the `pack('U',0)` utf8-NUL
tests) are commented out.

---

## `$SIG{__DIE__}` and `$SIG{__WARN__}` handler invocation

**Perl behaviour:** `die` invokes any `$SIG{__DIE__}` handler; `warn` invokes
any `$SIG{__WARN__}` handler.

**PCL behaviour:** `pl-warn` invokes `$SIG{__WARN__}` correctly.  `pl-die`
does NOT invoke `$SIG{__DIE__}` — implementing this requires CL condition
restarts and is deferred.

**Affected tests:** `perl-tests/die.t` tests using `$SIG{__DIE__}` are
commented out.

---

## `given`/`when` / smart match (`~~`)

**Perl behaviour:** `given`/`when` (the "switch" statement) and the `~~`
smart-match operator were introduced experimentally in Perl 5.10
(`use feature 'switch'`), deprecated in Perl 5.34, and **removed entirely
in Perl 5.38**.

**PCL behaviour:** Not implemented.

**Rationale:** The feature no longer exists in modern Perl (≥ 5.38).  No
maintained CPAN module targets it.  PCL's goal is to run real CPAN code, so
implementing a deleted construct would add complexity for zero practical gain.

**Affected tests:** `perl-tests/switch.t` — entire file skipped.  The
`~~` operator is also excluded from `perl-tests/cmpchain.t` (the few tests
that used it are commented out).
