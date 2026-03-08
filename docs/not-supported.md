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

---

## `@_` argument aliasing

**Perl behaviour:** Elements of `@_` are aliases to the caller's actual
arguments.  `$_[0] = 42` modifies the caller's variable in place.  This
is how many Perl idioms pass by reference without explicit `\`.

**PCL behaviour:** PCL copies arguments into `@_` via `pl-flatten-args`.
Modifications to `$_[0]` do not propagate back to the caller.

**Rationale:** CL function arguments are values, not aliases.  Implementing
Perl-style pass-by-alias would require wrapping every argument in a
mutable cell and teaching the caller to read it back — a pervasive change
with high cost and low gain for CPAN code (most modules don't use this
trick).

**Affected tests:** `perl-tests/args.t` (all 4 tests).

---

## Regex code blocks: `(?{code})` and `(??{code})`

**Perl behaviour:** `(?{code})` runs arbitrary Perl code during pattern
matching; `(??{code})` evaluates code and uses the result as a sub-pattern.
Both allow the regex engine to call back into Perl mid-match.

**PCL behaviour:** Not implemented.  CL-PPCRE does not expose a hook for
embedded Perl-side callbacks during scanning.

**Rationale:** These features require deep integration between the regex
engine and the Perl interpreter.  They are rarely used in CPAN modules and
have no clean mapping to CL-PPCRE's interface.

**Affected tests:** `perl-tests/study.t` (tests using `(?{...})`).

---

## `format` / `write` report formatting

**Perl behaviour:** The `format` keyword defines a report template;
`write()` fills it in and sends it to a filehandle.  `$^A` accumulates
the formatted text.

**PCL behaviour:** Not implemented.  `format` blocks are not parsed or
emitted.

**Rationale:** Perl's report-formatting system is essentially unused in
modern CPAN code.  No maintained module targets it.

**Affected tests:** None in `perl-tests/` (no format.t in the suite).

---

## `Internals::*` C-level introspection

**Perl behaviour:** `Internals::SvREADONLY($ref, 1)` marks a scalar
read-only; `Internals::SvREFCNT($ref)` returns the reference count of an
SV.  These are direct windows into Perl's C-level runtime.

**PCL behaviour:** Not implemented.  CL's garbage collector and box model
have no equivalent reference-count or read-only flag concept.

**Rationale:** Application code does not use `Internals::*`; only Perl's
own test suite and low-level XS modules call it.  Faking it would require
a per-box read-only flag and reference counting, with no benefit for
running CPAN modules.

**Affected tests:** `perl-tests/undef.t` (read-only checks),
`perl-tests/unshift.t` (read-only constant checks).

---

## `local` on hash/array elements and typeglobs

**Perl behaviour:** `local $hash{key}` and `local @arr[1,2]` temporarily
localize individual hash/array slots and restore them on scope exit.
`local *FOO` temporarily replaces an entire symbol-table entry (all of
`$FOO`, `@FOO`, `%FOO`, `&FOO`, `*FOO`) and restores it on scope exit.

**PCL behaviour:**
- `local $scalar` and `local @array` and `local %hash` — **supported**.
- `local $hash{key}`, `local @arr[N]`, `local @hash{@keys}` — **not supported**.
- `local *GLOB` — **not supported**.

**Rationale:** Element-level localization requires wrapping each slot
access in a save/restore protocol on top of CL dynamic binding, which
doesn't compose cleanly.  Typeglob localization similarly needs snapshot /
restore of all five slots in a PCL glob struct.  Both are deferred (see
`docs/todo-features.md`).

**Affected tests:** `perl-tests/local.t` (Tie::Array dependency causes a
separate hang that also blocks this file).

---

## Hex floating-point literals (`0x1.8p+1`)

**Perl behaviour:** Perl 5.22+ accepts C99-style hex float literals:
`0x1.8p+1` means `1.5 * 2^1 = 3.0`.

**PCL behaviour:** PPI does not tokenize hex floats as a single literal;
the source reaches PCL in a broken form.

**Rationale:** Hex floats are a niche syntax used almost exclusively in
low-level numerical code that also depends on XS.  Not worth a custom PPI
workaround.

**Affected tests:** `perl-tests/hexfp.t` — entire file skipped (PPI parse
error).

---

## DynaLoader / XS binary extensions

**Perl behaviour:** `DynaLoader` loads compiled `.so`/`.dll` extension
modules at runtime.  Most POSIX, POSIX-adjacent, and performance-sensitive
CPAN modules use XS.

**PCL behaviour:** PCL can only load pure-Perl modules.  Any `require` or
`use` that ultimately calls `DynaLoader::bootstrap` fails or is silently
skipped by the stub.

**Rationale:** XS bridge support is a planned future phase (see
`XS_BRIDGE_DESIGN.md`).  Until then, POSIX and other XS modules must be
stubbed in `lib/` by hand.

**Affected tests:** `perl-tests/chdir.t` (uses POSIX).

---

## Regex encoding modifiers (`/a`, `/d`, `/l`, `/u`)

**Perl behaviour:** Perl 5.14+ added regex modifiers that select which
Unicode semantics to use:
- `/a` — ASCII-only matching for `\d`, `\s`, `\w`, `\b`.
- `/d` — "default" Unicode-degraded behaviour (Perl 5.6 compat).
- `/l` — locale-dependent character class matching.
- `/u` — full Unicode semantics (the default on `use utf8` source).

**PCL behaviour:** These modifiers are accepted in the source but silently
ignored.  CL-PPCRE always uses Unicode semantics (roughly equivalent to
`/u`).

**Rationale:** The difference between `/a` and `/u` matters for
`\d`/`\s`/`\w` on non-ASCII text, which is uncommon in real CPAN code.
Emulating `/l` (locale) would require calling SBCL locale-aware functions,
which is not worth the complexity.

**Affected tests:** Various regex tests that use `/a` for strict ASCII
matching.

---

## `reset()` for one-match `?pattern?` and named captures

**Perl behaviour:** `reset()` clears all `?pattern?` patterns so they can
match again, and optionally resets package variables whose names start with
a given letter.  `reset` with no argument inside a `while` loop is
sometimes used to clear `%+` named-capture variables.

**PCL behaviour:** `pl-reset` is not implemented.  `?pattern?` (one-match
patterns) are also not implemented — they are parsed identically to
`/pattern/`.

**Rationale:** `?pattern?` was deprecated in Perl 5.22 and removed in
Perl 5.38.  `reset()` for variables is an obscure feature not used in
modern CPAN code.

**Affected tests:** `perl-tests/reset.t` (most tests pass via other means;
the `reset` / `?pat?` tests fail).

---

## `__SUB__` (current sub reference)

**Perl behaviour:** `use feature 'current_sub'; __SUB__` returns a
reference to the currently executing subroutine, enabling anonymous subs
to recurse without a named variable.

**PCL behaviour:** Not implemented.  `__SUB__` is not recognized as a
keyword.

**Rationale:** The same effect can be achieved with a named sub or by
capturing `$self = \&{__SUB__}` before Perl 5.16.  Uncommon in CPAN code;
implementing it would need a dynamic `*current-sub*` variable threaded
through every function call.

**Affected tests:** None in `perl-tests/` (no dedicated test file).

---

## `use integer` — large shift / overflow edge cases

**Perl behaviour:** Under `use integer`, very large shift amounts (e.g.
`4 << 2147483648`) yield `0`; right-shifting a negative number yields `-1`
(arithmetic shift fill).  These are defined by C's signed-integer
semantics.

**PCL behaviour:** `use integer` arithmetic is partially implemented, but
extreme shift counts and the exact overflow behaviour for
`IV_MIN << 0`-style corner cases differ between SBCL and Perl's C runtime.

**Rationale:** These are C-ABI details of the Perl interpreter, not
semantics that CPAN code relies on.  The common integer arithmetic cases
(`+`, `-`, `*`, `int(/)`) work correctly.

**Affected tests:** `perl-tests/bop.t` (large-shift and `use integer`
edge-case tests); the file also hangs for an unrelated reason (see
`docs/todo-features.md`).

---

## Lvalue subroutines

**Perl behaviour:** A sub marked `: lvalue` can appear on the left-hand
side of an assignment.  The sub must return a reference to a writable
location; Perl then stores the assigned value there.

```perl
sub field : lvalue { $obj->{field} }
field() = 42;           # modifies $obj->{field}
```

The built-in `substr` also acts as an lvalue: `substr($s, 0, 4) = "new"`.

**PCL behaviour:** The `: lvalue` attribute is not implemented.  `substr`
on the left-hand side of an assignment is also not supported.

**Rationale:** Implementing lvalue subs requires an "lvalue context"
that propagates through the call, returns a settable location, and then
performs the store — a fundamentally different calling convention from
normal subs.  No maintained CPAN module in scope requires custom lvalue
subs.  The `substr`-as-lvalue form can always be rewritten as
`substr($s, 0, 4, "new")` (four-argument form), which PCL does support.

**Affected tests:** `perl-tests/aassign.t` (a few tests use lvalue subs).
