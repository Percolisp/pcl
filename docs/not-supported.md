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

## `caller()` filename and line number

**Perl behaviour:** `caller()` in list context returns `($package, $filename, $line)`.
`caller(N)` returns the Nth frame's package, file, and line.  Many modules use
this for error reporting (`Carp`, `Exporter`, `Test::More`).

**PCL behaviour:** `pl-caller` returns the package name correctly.  Filename is
always `"(unknown)"` and line number is always `0`.  `caller(N)` for N > 0 is
unreliable.

**Rationale:** CL does not naturally expose Perl-compatible source location
metadata at runtime.  Embedding per-form source locations would require either
reader macros at transpile time or a side-table mapping CL function names to
source positions.  `caller.t` also depends on string eval (36 tests) and
`%::` stash manipulation — together these make the file essentially impossible
to pass without implementing those larger features first.  No CPAN module in
scope depends on exact `caller()` filename/line values; they use it only for
error message strings (where "at (unknown) line 0" is acceptable) or in test
infrastructure.

**Affected tests:** `perl-tests/caller.t` — only ~3/112 tests pass even after
fixing the crash bugs found in session 90.

---

## Error message text and format

**Perl behaviour:** Perl's compile-time and runtime errors have specific,
documented text (e.g. `"Can't find string terminator"`, `"syntax error at
FILE line N"`, `"Undefined subroutine &foo called"`).  Many test files use
`fresh_perl_like` / `eval { }; like($@, qr/.../)` to match exact error
wording.

**PCL behaviour:** PCL does not guarantee that error messages match Perl's
wording.  A parse error in PCL produces a different message than Perl's; a
runtime error (SBCL condition) has a completely different format.  Some
errors that Perl detects at compile time are silently ignored by PCL (see
"Error compatibility for invalid Perl input" below).

**Rationale:** PCL is a transpiler targeting correct execution of valid
CPAN code.  Replicating Perl's exact error vocabulary would require
duplicating Perl's full error-reporting infrastructure.  No CPAN module
depends on the exact wording of error messages it never triggers in normal
operation.

**Affected tests:** Any `perl-tests/` file that uses `like($@, qr/text/)`
on error strings, `fresh_perl_like(..., qr/error pattern/)`, or similar
error-message pattern matching (e.g. `heredoc.t` tests 17–43, parts of
`die.t`, `chop.t`, `anonsub.t`).

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

**PCL behaviour (all now supported):**
- `local $scalar`, `local @array`, `local %hash` — supported.
- `local $hash{key}`, `local @arr[N]`, `local @hash{@keys}` — supported (sessions 85–86, via `p-local-hash-elem` / `p-local-array-elem` macros).
- `local *GLOB` — supported (sessions 75–79, via `p-local-glob`).

**Affected tests:** `perl-tests/local.t` still fails due to `Tie::Array` dependency which causes a hang — not a `local` issue.

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

---

## `prototype()` does not return prototype strings

**Perl behaviour:** `prototype(\&foo)` returns the prototype string for `&foo`
(e.g. `"\$a"` for `sub foo ($a) { }` without `use feature 'signatures'`), or
`undef` if the sub has no prototype.

**PCL behaviour:** `prototype()` always returns `undef`.  The behavioral
distinction is handled correctly: without `use feature 'signatures'`, `($a)` in
a sub definition is ignored as a parameter binding (the body's `$a` refers to
the outer scope, matching Perl's prototype semantics); with the pragma, `$a` is
bound as a signature parameter.  Only the `prototype()` introspection value is
missing.

**Rationale:** Storing prototype strings requires tracking them at parse time and
threading them through to a runtime lookup table.  No maintained CPAN module
calls `prototype()` on its own functions — it's used only for introspection
tools and test infrastructure.

**Affected tests:** `perl-tests/signatures.t` test checking `prototype(\&t000) eq '"\$a"'`
(commented out).

---

## Bare `if` with empty true branch

**Perl behaviour:** `sub f { if(1) {} }` returns `undef`.  When the condition is
true but the branch body is empty, the last expression evaluated is the empty block,
which produces `undef`.

**PCL behaviour:** PCL returns the condition value (`1`).  The tail-if transform
saves the condition into `--pcl-if-ret--N` before testing it; when the branch body
is empty, nothing overwrites that variable, so the condition value is returned.

**Rationale:** This is an obscure corner case (an empty true branch whose return
value the caller inspects) that is essentially never written intentionally in CPAN
code.  Fixing it would require detecting the empty-branch case and emitting an
explicit `(setf ret_var nil)` inside the then-block — extra complexity for zero
practical gain.

**Affected tests:** None in `perl-tests/` (no test exercises this combination).

---

## Error compatibility for invalid Perl input

**Perl behaviour:** Perl validates code at compile time and produces specific error
messages (e.g., "Illegal declaration of anonymous subroutine", "syntax error") for
certain invalid constructs.

**PCL behaviour:** PCL's goal is to run valid CPAN code, not to validate or reject
invalid Perl.  When given invalid Perl, PCL may silently accept it, produce different
output, or simply ignore the erroneous construct — but it will not produce the same
error message that Perl would.

**Rationale:** PCL is a transpiler for functioning Perl code, not a linter or
Perl-compatible compiler.  Implementing Perl's full error-detection logic would
require replicating large parts of Perl's parser and semantic analysis, with no
benefit for running CPAN modules (which are valid Perl by definition).

**Affected tests:** `perl-tests/anonsub.t` tests 1–5 (invalid anonymous sub syntax);
`perl-tests/signatures.t` tests for syntax errors when `use feature 'signatures'`
is not in effect (commented out).

---

## `pack`/`unpack` — pointer types (`p`/`P`) and 80-bit long double (`D`)

**Perl behaviour:**
- `pack "p", \$str` / `pack "P", \$str` pack a raw memory address (pointer) into the
  binary string.  `unpack "p"` / `unpack "P"` recover the pointer and dereference it.
- `pack "D", $val` / `unpack "D", $str` use 80-bit extended-precision floating point
  (x87 `long double`).  Perl uses the platform C `long double` type.

**PCL behaviour:**
- `p`/`P` in a pack/unpack template signals `"Invalid type 'p' in pack"` (or `"in unpack"`).
- `D` signals `"Invalid type 'D' in pack"` (or `"in unpack"`).

These errors match `qr/^Invalid type '\w'/`, so the `is_valid_error()` helper in
`pack.t` counts them as **skipped** (TAP pass) rather than failures.

**Rationale:**
- **`p`/`P`**: CL's garbage collector moves objects; there are no stable raw memory
  addresses.  Providing a fake address would be dangerously wrong.  No CPAN module
  that runs on PCL would use raw pointer pack/unpack.
- **`D`**: SBCL uses 64-bit IEEE 754 doubles only (`double-float`).  There is no
  SBCL type for 80-bit extended precision.  `sb-kernel:single-float-bits` and
  `sb-kernel:double-float-bits` only cover 32- and 64-bit; there is no 80-bit
  equivalent.

**Affected tests:** `perl-tests/pack.t` — the `p`/`P` and `D` format tests are
handled via `is_valid_error()` skip, so they count as passes once the error is thrown.

---

## `DESTROY` called by garbage collector

**Perl behaviour:** When a blessed object goes out of scope and its reference count drops
to zero, Perl calls the `DESTROY` method (if defined).  Code that relies on deterministic
destruction (e.g. releasing a file lock, closing a handle, decrementing a counter) puts
this logic in `DESTROY`.

**PCL behaviour:** `DESTROY` is never called automatically.  SBCL's garbage collector
runs asynchronously and PCL has no finalizer hook wired to blessed objects.  A `DESTROY`
method can be defined and called explicitly (`$obj->DESTROY()`), but the implicit GC-driven
call does not happen.

**Rationale:** CL's GC does not guarantee finalizer order or timing.  Wiring DESTROY would
require `trivial-garbage` or SBCL-specific finalizer APIs with non-deterministic execution.
Most CPAN modules that use `DESTROY` do so for resource cleanup that is irrelevant when the
whole CL image exits anyway.  CPAN code that depends on DESTROY running while the program is
still running (e.g. `Scope::Guard`, `File::Temp`) is out of scope for now.

**Affected tests:** `perl-tests/ref.t` (tests 63–64), `perl-tests/grep.t` (tests 69–76),
`perl-tests/bless.t` (a few object-lifetime tests).  Do not attempt to fix these —
the only real fix is a GC finalizer integration.

---

## Lazy argument evaluation / `$SIG{__WARN__}` side effects during argument build

**Perl behaviour:** In Perl, function arguments are evaluated left-to-right.  If evaluating
one argument triggers a `$SIG{__WARN__}` handler that modifies a variable that appears in a
later argument, the later argument sees the modified value.

Example from `join.t` tests 9–10:
```perl
my $s = ':';
$SIG{__WARN__} = sub { $s = '-' };
# Each undef element warns; $s changes after the first warn;
# subsequent elements are joined with '-', not ':'.
is join($s, undef, undef, undef), "-";
```

**PCL behaviour:** CL evaluates all arguments before calling the function.  `p-join` receives
a snapshot of `$s` taken before any warn handlers fire, so it always uses the original `':'`.

**Rationale:** CL function calls are strict (not lazy).  Replicating Perl's left-to-right
side-effect semantics would require thunk-wrapping every argument and forcing them one at a
time inside the function — a pervasive change to calling convention with essentially zero
benefit for real CPAN code (no module intentionally modifies its own separator mid-join).

**Affected tests:** `perl-tests/join.t` tests 9–10.  Do not attempt to fix these —
the fix requires a fundamentally different argument-passing model.

---

## Ref aliasing (`use feature 'refaliasing'`)

**Perl behaviour:** `use feature 'refaliasing'` enables assignment to references
as lvalues: `\$x = \$y` makes `$x` an alias for `$y`.  It was experimental in
Perl 5.22–5.38 and removed in Perl 5.40 without graduating to stable.

**PCL behaviour:** Not implemented.  The `use feature 'refaliasing'` pragma is
silently accepted as a no-op, but the lvalue-ref assignment `\$h{foo} = \$var`
does not create an alias.

**Rationale:** The feature was removed from Perl itself.  No stable CPAN modules
depend on it.  Implementing lvalue-ref aliasing in the PCL box/unbox model would
require significant runtime changes for a removed, never-stable feature.

**Affected tests:**
- `perl-tests/substr.t` — last block (`{ # [perl #132527] ... }`) commented out (1 test)
- `perl-tests/aassign.t` — blocks at lines 124–175 and 284 use refaliasing (multiple tests)
- `perl-tests/each.t` — block at lines 319–320 uses refaliasing

---

## Context propagation into string eval

**Perl behaviour:** `eval "code"` inherits the calling context.  Code inside the string
eval can call `wantarray()` and get the correct answer; built-ins such as `%hash{keys}`
emit a warning when the eval is called in scalar context (e.g. `scalar eval '...'` or
`my $v = eval '...'`).

**PCL behaviour:** `p-eval` calls the `pl2cl` subprocess to transpile the string, then
calls `(load ...)` in the same SBCL process.  Because `*wantarray*` is a CL dynamic
variable, `(load ...)` inherits whatever dynamic binding is in scope at the call site.
However, PCL does not currently emit a `(let ((*wantarray* ctx)) (p-eval ...))` wrapper
because the calling context is not known at code-generation time: determining it would
require working AST-level context annotations (`docs/ast-annotation-plan.md`), which are
deferred.  Without that wrapper, code inside string eval cannot reliably detect its calling
context via `wantarray()`, and context-sensitive behaviour (e.g. the scalar-context warning
from `%hash{keys}`) does not fire.

**Path to fix:** Once `docs/ast-annotation-plan.md` is implemented, `gen_funcall` can
detect `eval "string"` calls annotated with a context and emit:

```lisp
(let ((*wantarray* nil))   ; or t / :void
  (p-eval ...))
```

This would propagate context through `(load ...)` correctly, since dynamic bindings cross
`load` boundaries within the same process.

**Affected tests:** `perl-tests/kvhslice.t` tests 9–12 and 25–28 (scalar-context warning
via `scalar eval '...'`); any test in other files that relies on `wantarray()` returning a
meaningful value inside `eval "string"`.
