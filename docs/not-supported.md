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
