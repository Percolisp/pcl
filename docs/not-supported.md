# PCL: Features Deliberately Not Supported

This document lists Perl behaviours that PCL intentionally does not emulate,
along with the rationale.  Tests covering these features are commented out in
the `perl-tests/` files.

> **Deferred vs. permanent.** Most items here are *permanent* design decisions
> (they replicate a Perl interpreter implementation detail with no payoff for
> real CPAN code).  A few, however, are merely **deferred until after the
> compatibility phase** — implementable, planned, and tracked in the README
> roadmap ("Deferred language features — planned, not rejected").  These are
> marked **[DEFERRED — see roadmap]** below: live symbol-table hashes
> (`%main::`), `__SUB__` outside string `eval`, and richer `caller()`
> (package/sub-name depth).

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

**Affected tests:** `perl-tests/not.t` tests 21–24.  They were **commented out
of the .t file** until s337 (#150 part 2); the file is now byte-identical to
`t/op/not.t` and the four assertions run, matched by the skip registry
(`cl/skip-registry.lisp`, `:read-only`).  Note task #159 gave read-only
*arrays* a real representation — the storage itself is fixed-size — but a
read-only **scalar** has nowhere to carry the flag, and interning would need a
global constant table besides.

---

## A LITERAL in a `foreach` list is writable, not read-only

**Perl behaviour:** a constant in a foreach list is a read-only scalar, and the
loop variable aliases it — so `for (3) { $_++ }` and `for ($x, 3) { $_++ }`
both die *"Modification of a read-only value attempted"* (after the writable
elements before the literal have already been modified).

**PCL behaviour:** the literal becomes an ordinary box like any other element,
so the write silently lands on a copy and the loop runs to completion.  Every
non-literal element in the same list is written correctly.

**Rationale:** same missing mechanism as the two entries around this one — a
read-only **scalar** has nowhere to carry the flag in PCL's box model (task
#159 gave read-only *arrays* a real representation because there the storage
itself is fixed-size).  This is not new to #267's multi-element aliasing: the
single-element spelling `for (3) { $_++ }` has always behaved this way, probed
live against perl in s370.  Nothing consumes a wrong value — the program keeps
running where perl stops — so it is an accepted divergence, not a silent-wrong
in the rule-12 sense.

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

## Live symbol-table hashes (`%main::`, `%Foo::`)  [DEFERRED — see roadmap]

**Perl behaviour:** `%main::` (and any `%Pkg::`) is a live view of the package
symbol table: its keys are symbol names, its values are typeglobs, and writing
to it (`$main::{foo} = \&bar`) installs into the package.  Code that walks or
mutates the stash (`Exporter`, plugin loaders, `*{...}` glob tricks) relies on
this being live.

**PCL behaviour:** The syntax (`$Pkg::`, `%Pkg::`, `$::`, `%::`, `*Pkg::`) parses
and routes to `(p-stash "Pkg")`, but `p-stash` returns a **snapshot containing
only subs** (a fresh hash of `name → code-ref` built from the CL package on
each call).  Reads of existing subs (`exists`/lookup) work.  **`delete` now
writes through** (session 247): each snapshot is registered in a weak
side-table (`*p-stash-pkg-table*`), and `p-delete` on a stash also
`fmakunbound`s the sub in the CL package, so `*{Pkg::name}{CODE}` and method
dispatch stop seeing it (Moo's Method::Generate::Constructor bootstrap relies
on this).  Other writes (`$Pkg::{foo} = \&bar`) are still lost;
scalar/array/hash/IO glob slots are absent; `local` on a stash element is
skipped.

**Why deferred, not permanent:** PCL package vars and subs already *are* CL
symbols in a CL package, so the stash is introspectable via `do-symbols`.  The
plan (README roadmap) is to make `p-stash` return a **live proxy** over the CL
package and teach the hash primitives (`p-hash-get`/`-set`, `p-exists`,
`p-delete`, `p-keys`, …) to dispatch to package introspection — a pure runtime
change needing no new compiler analysis, and cheap because normal `$Foo::bar`
access compiles to a direct symbol reference and never touches the stash.  Full
typeglob slots are the fiddly remainder.

**Affected tests:** `perl-tests/caller.t` (needs `%::`), `perl-tests/undef.t`
(stash-slot tricks), and any stash-walking module code.

---

## `caller()` filename and line number  [DEFERRED — see roadmap]

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

**Re-confirmed by the user 2026-07-28 (s316e)** — also covers the perl-suite
fatal-death rows (`run/fresh_perl.t` expected-tsv row).  If a CPAN module
pattern-matches a specific message it actually triggers, fix that one message
at the point that raises it.

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

> **Supported (session 2026-06-28):** `use utf8` now decodes the *source* as
> UTF-8 before parsing (`_maybe_decode_utf8` in `Pl/Parser.pm`), so multi-byte
> sequences in string literals — and UTF-8 *identifiers* (`my $café`, `*ワルド`)
> — are single characters: `use utf8; length("café")` is `4`, `substr`/`index`
> are character-based.  Without the pragma, high bytes stay Latin-1 (byte
> semantics), matching Perl.  The items below are the *remaining* divergences.
>
> **Caveat — `use utf8` is whole-file, not lexically scoped.**  Perl's `use
> utf8` is a *lexical* pragma: only literals/identifiers inside its block are
> UTF-8.  PCL decodes the **entire source** as UTF-8 whenever `use utf8` appears
> anywhere in the file.  The reason is the *input shape*, not PPI: PCL passes
> the source to PPI as a single Perl scalar, and a scalar's UTF-8 flag is
> all-or-nothing (per-scalar), so `utf8::decode` on it is necessarily
> whole-string.  Honoring lexical scope would mean splitting the source into its
> `use utf8` regions and decoding each independently — but locating those
> regions requires parsing the source first, which needs the encoding decided
> first (circular).  The observable divergence is a file that mixes a `use utf8`
> block with raw high-byte literals *outside* any such block:
> `{ use utf8; length("café") }` is `4` in both, but a later unscoped
> `length("café")` is `5` in Perl (bytes) and `4` in PCL (still decoded).  Real
> code puts `use utf8` at the top and treats the whole file as UTF-8, so this
> never bites in practice.

- **`utf8::encode` / `utf8::decode`**: Perl has an internal UTF-8 flag per
  scalar that can be toggled.  CL strings are always Unicode; the flag does not
  exist.  `utf8::encode` is therefore a **no-op** — measured s337:
  `utf8::encode("\x{100}")` leaves a 1-character string of ord 256, where perl
  produces the 2 bytes 196,128.  Tests that call `utf8::encode` and then compare
  the *byte* encoding to the *character* string are not meaningful in PCL.
  **Second-order consequence (s337, #150 part 2):** a test file can *skip its own
  rows* over this.  `t/op/chop.t` guards a 4-assertion block with
  `next if $end_utf8 eq $end` after encoding; under PCL the two are always equal,
  so the block never runs and the file emits **100 of perl's 148 rows** —
  `perl-tests/chop.t` is PARTIAL/INCOMPLETE for this reason and not because
  anything failed.  (Until s337 that copy had its plan hand-lowered to 100, which
  made the shortfall read as a clean pass.)  Nothing can be skip-registered here:
  the rows are never emitted at all.

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

### `use bytes` — byte view of upgraded strings

Perl's `use bytes` makes string ops (including `sprintf %vd`) see the UTF-8
*bytes* of characters above 255 (`sprintf "%vd", v1.22.333.4444` under the
pragma yields `1.22.197.141.225.133.156` — the 333 and 4444 expand to their
UTF-8 byte sequences).  PCL strings are SBCL character vectors with no
byte-view; the pragma is a no-op, so such characters format as their code
points.  Affects `op/ver.t` tests 21/23/25 and `op/chr.t` tests 10-13 (both
the `use bytes` block; chr.t asks for `chr(-1)` to be the single byte `\xFF`
under the pragma, where PCL keeps the un-pragma'd `\x{FFFD}`).

### Code points above U+10FFFF (perl's extended UTF-8)

**Perl behaviour:** perl can hold and encode code points *beyond* Unicode's
maximum using its own extended UTF-8 — `chr(0x110000)` encodes as
`f4 90 80 80`, `chr(0x1FFFFF)` as `f7 bf bf bf`, and `chr(0x200000)` as the
five-byte `f8 88 80 80 80`.

**PCL behaviour:** the character is unrepresentable, and stringifies as
`\x{FFFD}` (the Unicode replacement character) — which is already what perl
itself gives for other unrepresentable arguments such as `chr(-1)`.

**Why:** Perl strings are SBCL character vectors, and SBCL's `char-code-limit`
is 1114112 (`#x110000`) — measured, s319/s320 — so the largest character that
can exist in a CL string is `#x10FFFF` and `(code-char #x110000)` yields no
character at all.  Supporting these would mean changing the representation of
every Perl string away from a CL string, which is a data-model change of the
same scale as boxed aggregates, for a perl-private extension: no CPAN module
emits code points past U+10FFFF, and Unicode itself defines none.  Ruled a
blessed gap in `docs/fable-answers-s318.md` §11.

Affects `op/chr.t` tests 40-42.  (`ord` still round-trips the number — PCL
carries the numeric value in the box — so only the *character/encoded* form
is lost.)

### `use vN` does not toggle default warnings

Perl ≥ 5.35 as a `use VERSION` target enables warnings by default in that
lexical scope (`op/ver.t` test 54).  PCL treats `use v5.x` purely as a
version check.

### Control-character glob names ("Mordor" v-strings)

`*{"\3"} = *DATA; readline v3` — aliasing a glob under a control-character
name and reading through a v-string handle designator (`op/ver.t` test 52)
exercises symbolic-glob machinery PCL does not model for non-identifier
names.

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

## `@_` argument aliasing — PARTIAL (plain `my` lexicals only)

**Perl behaviour:** Elements of `@_` are aliases to the caller's actual
arguments.  `$_[0] = 42` modifies the caller's variable in place.  This
is how many Perl idioms pass by reference without explicit `\`.

**PCL behaviour (since task #131, 2026-08-01):** most argument shapes DO
alias — the box already in play is passed through:

- package globals, `our` vars, `$_` and other magic globals;
- array/hash spreads (`f(@a)` — element boxes, holes as lazy defelem);
- named-container ELEMENTS (`f($h{k})`, `f($a[i])`): the live slot box
  when the element exists, a lazy defelem cell when it does not (a
  read-only callee never vivifies; the first write through `$_[N]`
  creates the key / extends the array).  Named subs, coderef calls
  (`$c->(...)`, `&$c(...)`), and method calls.

**Plain `my` lexicals: CLOSED for KNOWN callees (task #189, s330).**  A sub
whose body writes through `@_` is detected where the fact lives — the callee's
body (`Parser2::_sub_writes_args`) — the fact rides `sub_info` as
`writes_args`, and VarAnnotator turns it into an `arg-to-writer` boxing event
at that sub's call sites.  So `sub setit { $_[0] = "x" } setit($lexical)` now
writes the caller's variable, and only files containing such a sub pay for it.
The scan is deliberately conservative: an `@_`/`$_[N]` occurrence it cannot
prove is a read (including the escapes `\$_[N]`, `\@_`, `&callee;`,
`goto &sub`, handing `@_` to an unknown callee) sets the flag.

What still COPIES:

- calls the scan cannot see the callee of — **coderef calls (`$c->(…)`),
  method dispatch, and cross-file callees** (the fact is same-file today).
  The runtime's "Cannot modify non-boxed value" warning is the loud backstop
  for exactly these, and must not be removed.
- deref elements (`f($ref->{k})`) and prototype-`$`-imposed element args
  (extendable via the same argbox accessors if real code needs them).
- `substr($_[0],…)` as an lvalue and 4-arg `substr` — the ARGUMENT arrives
  boxed, but the callee lowers substr's target as a value (task #209).

**Affected tests:** `perl-tests/args.t` rows touching coderef/method-dispatch
argument aliasing.

**What it used to cost (s323 → closed s330):** perl's own `File::Basename`
uses the in-place idiom — `sub _strip_trailing_sep { $_[0] =~ s{(.)/*\z}{$1}s }`
called as `_strip_trailing_sep($dirname)` — so `dirname("/a/b/c")` answered
`/a/b/`, wrong for every path, and PCL shipped `lib/File/Basename.pm` to work
around it.  **That shim is deleted**: core's File::Basename now runs correctly,
guarded by `Pl/t/writes-args-01.t`.

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

**PCL behaviour:** Not implemented.  A `format NAME = … .` block is **stripped at
the source level** (`_preprocess_source`, session 2026-06-25) so it does nothing
but, crucially, does not corrupt the surrounding code — PPI does not recognise the
`format` keyword and otherwise swallows the picture lines *and the following
statement* into one bogus statement, surfacing the `.` terminator as an unknown
operator (a PARSE ERROR that loses the next real statement).  `write()` is a
no-op runtime stub returning `1` (Perl's success value) rather than an
undefined-function crash.  The format-control specials exist as writable globals
with Perl-like defaults (`$~`→`"STDOUT"`, `$^`→`"STDOUT_TOP"`, `$=`→60, `$-`/`$%`
→0) so reading/setting them never crashes — but since `write()` does nothing,
values that Perl updates *as a side effect of writing* (e.g. `$-` lines-left)
stay at their defaults.

**Rationale:** Perl's report-formatting system is essentially unused in
modern CPAN code.  No maintained module targets it.

**Affected tests:** None in `perl-tests/`.  `t/io/defout.t` reaches 21/22 (the
one fail is `$-`, which only changes once a real `write()` has run).

---

## `Internals::*` C-level introspection

**Perl behaviour:** `Internals::SvREADONLY($ref, 1)` marks a scalar
read-only; `Internals::SvREFCNT($ref)` returns the reference count of an
SV.  These are direct windows into Perl's C-level runtime.

**PCL behaviour:** split, since task #159 (s337):

- **`Internals::SvREADONLY(@array, FLAG)` — IMPLEMENTED.**  Perl's read-only
  AV is a *fixed-size* array whose elements stay writable, and PCL gets that
  by swapping the variable's storage for a simple CL vector (no fill pointer,
  not adjustable) over the same element boxes.  Every size change then fails
  by construction and reports perl's `Modification of a read-only value
  attempted`; in-bounds element writes and foreach-alias writes still work.
  Clearing the flag restores an adjustable copy.  Guard: `Pl/t/readonly-array-01.t`.
- **`Internals::SvREADONLY($scalar, FLAG)` and `(%hash, FLAG)` — still not
  implemented**, and now say so: one deduped stderr line per kind
  (`PCL: Internals::SvREADONLY on a SCALAR is not implemented (task #159) —
  ignored`).  A read-only *scalar* needs a per-box flag; a read-only *hash* is
  a different perl feature entirely (a **restricted hash**, whose error is
  `Attempt to access disallowed key '…' in a restricted hash`), not the same
  fixed-size rule.
- `Internals::SvREFCNT` still returns 1 — a GC runtime has no refcount.

**Three known divergences of the array implementation**, all announced or
strictly better than the old silent no-op:

1. `$#ro = N` where N would *shrink* the array: perl does not guard this — it
   truncates.  PCL announces and does nothing, because a simple vector cannot
   be truncated in place and the runtime does not have the variable's cell at
   that point.  (Growing correctly dies, as in perl.)
2. A reference taken **before** the flag was set still points at the old
   adjustable storage, so `push @$r, 1` through it succeeds where perl dies.
   CL cannot un-adjust an existing array, so storage identity cannot be
   preserved across the swap.
3. `Internals::SvREADONLY(@$ref, 1)` — through a reference rather than a named
   array — cannot reach the storage cell, so it announces and no-ops.

**Affected tests:** `perl-tests/undef.t` 16–18 (read-only **scalars**, still
registered).  `push.t`, `unshift.t`, `splice.t` and `sort.t`'s in-place-sort
row now pass and are no longer in the skip registry.

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

**Rationale:** XS bridge support is a designed future phase (see
`docs/xs-shim-design.md` — source-recompile against `libperlshim` shim
headers; `XS_BRIDGE_DESIGN.md` is the superseded sketch).  Until the bridge
ships, POSIX and other XS modules must be stubbed in `lib/` by hand.

Note (session 223c): individual XS *functions* can be bridged directly via
`sb-alien` when there is a stable libc entry point.  `crypt()` is now
implemented (`p-crypt` → system `crypt(3)` in `libcrypt.so.1`), so it is no
longer in this not-supported bucket.

**Affected tests:** `perl-tests/chdir.t` (uses POSIX).

---

## Source filters (`Filter::Util::Call`, `Filter::Simple`, `use Switch`, …)

**Perl behaviour:** A source filter is code installed by a module's `import`
(via `filter_add` from `Filter::Util::Call`, or the `Filter::Simple` sugar on
top of it) that intercepts and rewrites the *source text* of the file being
compiled, from the line after the `use` to end-of-file (or until
`filter_del`).  The filter hooks perl's parser reader: as the interpreter
reads each source line, the filter gets to transform it before compilation.
Classic users: `Switch` (removed from core in 5.14), `Filter::cpp`,
obfuscators/decryptors, and legacy syntax-extension modules.

**PCL behaviour:** Not supported.  A filter module's `import` runs, but
`filter_add` does not exist (Filter::Util::Call is XS), and there is no
parser reader to hook: PCL parses the whole file with PPI at transpile time.
Code that relies on a filter rewriting subsequent source will be transpiled
from the *unfiltered* text and misbehave or fail to parse.

**Could it be done?**  Two distinct routes, one dead and one live:

- **At runtime — no, and the planned XS bridge does not change that.**
  `Filter::Util::Call`'s XS talks to perl parser internals (the
  `PL_parser`/`PL_rsfp_filters` filter stack), which are exactly the
  internals `docs/xs-shim-design.md` classifies as Tier X (§8.6): the shim
  exposes the documented value API, not the parser, and PCL's "parser" is
  PPI in a separate process — there is nothing at runtime for a filter to
  hook.

- **At transpile time — yes, faithfully, if ever needed.**  Source filters
  run at *compile* time in perl too, so applying the filter to the remaining
  source text during transpilation and then transpiling the result is
  semantically correct.  The transpiler runs under real perl, so the
  mechanism is: on `use Foo ARGS` where `Foo` installs a filter, have the
  system perl execute the real filter over the rest of the file (build a
  temp program of `use Foo ARGS;` + remaining text and capture the
  post-filter source via a `filter_read` loop — the `Filter::ExtractSource`
  technique), splice the filtered text back, and continue the normal PPI
  parse.  Caveats: the filter module itself runs under the *system* perl
  (it must be installed there, and its XS runs natively — which is fine,
  that is where it ran for real perl too); filters that consult program
  compile-time state beyond their import arguments (variables set by
  earlier `BEGIN` blocks of the *user* program) would not see it; and
  detecting "is `Foo` a filter module?" requires actually running its
  import under real perl, so it would be driven by a known-filter-modules
  list rather than general detection.

**Rationale for not implementing now:** source filters are rare, fragile,
and actively discouraged in modern Perl (`Switch` was expelled from core;
`Filter::Simple`'s own docs warn against filters; the ecosystem moved to
keyword plugins, which are a different, also-unsupported mechanism).  No
CPAN module in PCL's scope uses one.  The transpile-time route above is
recorded so that if a real module ever needs it, the feasible design is
already on file — revisit then, not before.

**Affected tests:** `perl-tests/closure.t` — the `[perl #114888]` block
(a perl-internals pad-name regression test that builds a filter inside
`fresh_perl_is`; it is skipped territory anyway since `fresh_perl_*` spawns
a separate perl binary).  No other test or in-scope module uses a filter.

---

## `fork` — SUPPORTED (with two narrow caveats), not a general gap

`fork` **is implemented** via `sb-posix:fork` (`p-fork` in
`cl/pcl-runtime.lisp`), together with `wait`, `waitpid` (sets `$?`), `getppid`,
`kill SIGNAL, LIST` (numeric or named signals), and `exec LIST`.  The parent
gets the child PID, the child gets `0`, both continue running the program, and
`$Config{d_fork}` reports true.  The classic patterns work and match perl:

- `fork; exec PROG` (spawn a program in the child) — verified byte-for-byte.
- `fork; …Perl…; exit` + `waitpid`/`wait` reaping with the right `$? >> 8`.
- `kill 'TERM', $pid` / `kill 9, @pids` (returns the count signalled).

Output streams are flushed before the fork so buffered text is not duplicated
into the child.

**Caveat 1 — no fork after CL threads.**  A raw `fork(2)` keeps only the
*calling* thread in the child.  If the program has spawned CL/Perl threads
before forking, the child is missing them and fork-then-continue is undefined.
Ordinary single-threaded Perl (the overwhelming majority) is fine.

**Caveat 2 — a fork-then-*continue* child is still an SBCL process.**  Such a
child inherits SBCL's signal handlers, so a signal that would *kill* a plain
perl child (e.g. `kill 'TERM'`) may instead be caught and turned into a clean
exit — so `$? & 127` (death-by-signal) can read `0` where perl shows the signal
number.  This does **not** affect fork-then-`exec` children (the exec'd program
has default handlers) nor the fork/wait/exit-status path; it is a narrow
artifact of running Lisp in the post-fork child.

**Affected tests:** `t/io/pipe.t`, `t/io/openpid.t` and similar now *run* rather
than crashing.  (`t/io/socket.t`/`socketpair.t` still need the socket-server
plumbing, and Perl's `runperl`/`fresh_perl_*` helpers spawn a *separate*
`./perl` binary — those are different, unrelated gaps.)

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

## Regex `/n` modifier — non-capturing groups

**Perl behaviour:** Perl 5.22+ added the `/n` modifier, which makes the grouping
metacharacters `( )` *not* capture: `"hello" =~ /(hi|hello)/n` matches but leaves
`$1` undef.  It has three non-trivial wrinkles:
- **Named captures are exempt**: `(?<a>.)` under `/n` *still* captures (sets `$1`
  and `%+`), so `/n` cannot be a blanket "make every group non-capturing".
- **Scoped overrides**: `(?n:…)` turns nocapture *on* for a sub-pattern even
  without the flag, and `(?-n:…)` turns it *off* inside an otherwise-`/n` pattern.
  Correctly honouring these needs a real modifier-scope stack while scanning the
  pattern.
- **Stringification preserves the original**: `qr/(what)/n` stringifies as
  `(?^n:(what))` — the literal `(what)` is kept; only *matching* must skip the
  capture.  So any rewrite has to be match-time only, not alter the stored
  pattern text.

**PCL behaviour:** `/n` is accepted in the source but silently ignored — groups
capture as usual.  CL-PPCRE has no `/n` equivalent.

**Rationale:** Faithful `/n` would require a mini regex-pattern rewriter that
(a) converts only *unescaped, non-special, non-named* `(` to `(?:` while skipping
character classes and `\(`, (b) tracks `(?n:)`/`(?-n:)` scope, and (c) compiles
the rewritten pattern for matching while keeping the original for stringification
— a sizeable, self-contained feature.  `/n` is rare in real CPAN code (the common
way to avoid a capture is to just write `(?:…)`), so the payoff does not justify
the machinery.  Revisit if a real module is shown to depend on it.

**Affected tests:** `t/re/reg_nocapture.t` (the `/n`, `(?n:)`, `(?-n:)` rows),
parts of `t/re/rxcode.t`.

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

## `__SUB__` (current sub reference)  [PARTIAL — named subs work, anon subs do not]

**Perl behaviour:** `use feature 'current_sub'; __SUB__` returns a
reference to the currently executing subroutine, enabling anonymous subs
to recurse without a named variable.

**PCL behaviour (since s316o):** Inside a NAMED sub — body or signature
default — `__SUB__` is rewritten at the shared PPI entry to `(\&name)`
(`_rewrite_current_sub` in `Pl/Parser.pm`): zero runtime cost, correct
recursion (op/signatures.t t122), late-bound so redefinition is honored.
Inside an ANONYMOUS sub it still resolves to the runtime stub
`pl-__SUB__`, which returns a no-op lambda.

**Rationale for the anon gap:** an anonymous closure has no name to
rewrite to; a correct version needs a per-closure self-binding
(`my $self; $self = sub {…}` shape or a dynamic `*current-sub*`), which
either changes closure shape or taxes every call.  Anon `__SUB__`
recursion is rare in CPAN code.

**Affected tests:** anon-`__SUB__` uses only.

---

## Scalar copy does not preserve reference/SV identity

**Perl behaviour:** A Perl scalar variable holds an SV with a stable identity.
Copying a *reference* into another scalar (`my $b1 = $b`) makes both point at the
same referent, so `bless $b, 'Pie'` is visible through `$b1`, and `\state $x`
returns the *same* address on every call (the state SV persists).

**PCL behaviour:** A PCL scalar is a `p-box`; copying it (`my $b1 = $b`) unboxes
and re-boxes the value, so the two scalars do not share a single underlying SV.
Re-blessing through one copy is not seen through another, and `\(state $x)` yields a
fresh address each call.  (Blessed *hash*/*array* refs DO share identity — the class
lives in the referent — so this only affects blessed/aliased **scalar** refs.)

**Rationale:** Faithful scalar SV-identity would require every scalar to be a shared
mutable cell threaded by reference everywhere, a pervasive change to the box model
for a corner real CPAN code does not depend on (it shows up only in identity/aliasing
torture tests).  Related: "`@_` argument aliasing" and "Sparse arrays … SV identity".

**Affected tests:** `perl-tests/qr.t` test 6 (`my $b1=$b; bless $b` — `$b1` should
also be blessed), `perl-tests/state.t` "Reference to state variable", and the C3
rebless-in-place rows in `perl-tests/bless.t` (`my $c1 = bless $c1, "C3"` not seen
through the outer `$c1`).

---

## Computed goto (`goto EXPR`)

**Perl behaviour:** `goto EXPR` where EXPR evaluates to a label name (or `&sub`)
transfers control to that label/sub.  `state.t` uses `goto state $flower = $f` to
jump to a label held in a state variable.

**PCL behaviour:** Not implemented for the label form.  CL has no first-class labels,
so a label name computed at runtime cannot be resolved to a `tagbody` tag.
`p-goto-computed` now NAMES the label on stderr and falls through
(`%p-goto-target`); it used to be a silent no-op returning undef.  Announced,
not fatal (the #155 tie shape): a die aborts a whole file over one rare
construct — measured, `state.t` runs 157 of 166 rows with the warning and 69
with a die.  Fixed s328.  **Only the label form is unsupported**: `goto &sub`
AND `goto EXPR` where EXPR evaluates to a **code ref** (`goto \&NAME`,
`goto $coderef`) are both real tail calls and both supported — `goto \&NAME`
was also silently doing nothing before s328, which is what hid Capture::Tiny's
entire public API (task #199).

**Scope note (s295):** this entry covers only the *computed-name* form.  `goto
LABEL` with a **literal** label is fully supported — backward gotos as lexical
`(go)` jumps, forward gotos (including from inside a `map`/`grep` lambda) via
the throw-based `:pcl-goto-<label>` catch-wrap — see `ir-spec.md` §6.4.  That
mechanism does not extend to computed names: backward targets are still lexical
`tagbody` tags (not first-class), and the forward catch-wrap is placed at
compile time per label, which requires knowing the target label at the goto
site.

**Rationale:** Runtime-computed `goto LABEL` has no clean CL target (tags are lexical,
not first-class).  It is rare and discouraged in modern Perl.

**Depth note (s329):** `goto &sub` replaces the frame in perl, so an unbounded
goto CHAIN (a goto-based trampoline) runs at constant stack.  PCL's frame
replacement is throw-based and still nests dynamic bindings, so a chain is
BOUNDED — ~10^5 chained gotos exhaust SBCL's binding stack.  Every CPAN use
seen so far is a bounded chain (usually depth 1: `unshift @_, …; goto \&impl`),
which is fully supported; a true trampoline loop is not.

**Affected tests:** `perl-tests/state.t` "computed goto" rows (70–73).

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

**PCL behaviour:** The user-defined `: lvalue` attribute is not implemented.

Note (session 219): the *built-in* magic lvalues `substr`, `pos` and `vec` **are**
supported, both as direct assignment targets and as live references:
- `substr($s, 0, 4) = "new"` works (rewritten to the four-argument form).
- `\substr($s, $off, $len)`, `\pos($s)` and `\vec($s, $o, $b)` now produce live
  write-through references (`my $r = \substr($s,0,1); $$r = "J"` mutates `$s`),
  via the `p-magic-cell` mechanism (`p-substr-ref` / `p-pos-ref` / `p-vec-ref` in
  `cl/pcl-runtime.lisp`).  `ref`/`reftype`/stringification report `LVALUE`.

Only user-defined `: lvalue` subs remain unsupported.

**Rationale:** Implementing user lvalue subs requires an "lvalue context"
that propagates through the call, returns a settable location, and then
performs the store — a fundamentally different calling convention from
normal subs.  No maintained CPAN module in scope requires custom lvalue subs.

**Affected tests:** `perl-tests/aassign.t` (a few tests use user `: lvalue` subs).
The `\substr`/`\pos`/`\vec` lvalue-ref rows in `perl-tests/ref.t` now pass.

---

## `prototype()` — returns only registered prototypes (attribute / Sub::Util)

**Perl behaviour:** `prototype(\&foo)` returns the prototype string for `&foo`
(e.g. `"\$a"` for `sub foo ($a) { }` without `use feature 'signatures'`), or
`undef` if the sub has no prototype.

**PCL behaviour (since s316o):** a runtime registry (`%pcl-sub-prototypes`,
keyed on the function object) backs `prototype()`.  It is populated by the
`:prototype(...)` attribute (named and anonymous subs — desugared at the
shared PPI entry into `__pcl_set_prototype` calls) and by
`Sub::Util::set_prototype`, and `prototype()` accepts coderefs, blessed
coderefs and symbolic names.  CLASSIC prototypes (`sub foo ($$) {…}`) are
still consumed at transpile time only — `prototype()` reports `undef` for
them, where perl reports the string.  (For how PCL reads a `($a)`-shaped
parameter list at all, see *Signature syntax is read as a signature even
with the feature off*, below.)

**Rationale for the classic-prototype gap:** registering every prototyped
sub is easy but changes `defined prototype(...)` guards in code that works
today; do it deliberately (with a sweep) or not at all.  No maintained CPAN
module reads its own classic prototypes back at runtime.

**Affected tests:** `perl-tests/signatures.t` rows reading classic
prototypes back.

---

## Signature syntax is read as a signature even with the feature off

**Perl behaviour:** a parameter list is a *signature* only where
`use feature 'signatures'` (or `use v5.36`, `use experimental 'signatures'`) is
lexically in scope.  Without it, `sub f ($a) { $a }` declares the *prototype*
`($a)` — perl warns `Illegal character in prototype` and binds nothing, so the
body's `$a` is the outer `$a`.

**PCL behaviour:** any parameter list containing a NAMED parameter (a sigil
immediately followed by a word character: `($a)`, `($x, $y = 1)`) is compiled as
a signature, regardless of the pragma.  Real prototypes — `()`, `($$)`, `(&@)`,
`(\@\@)`, `($;$)` — contain no named parameter, are unaffected, and keep their
prototype meaning.  Named and anonymous subs share this rule, and the same
textual test (`parse_prototype_or_signature`) implements both.

**Rationale:** PCL's compiler is not lexically pragma-aware, and neither is its
only source of that information.  PPI *does* track the feature and hands back a
`PPI::Structure` instead of a `PPI::Token::Prototype` when it is enabled — that
signal is honoured when present — but it is not sufficient: PPI's pragma
tracking only takes effect on the NEXT LINE (so the one-liner
`use feature "signatures"; my $c = sub ($x) {…}` yields a Prototype), and a
string `eval` is compiled on its own, with no view of the enclosing scope that
enabled the feature.  Treating a named parameter as a signature is right in
every case where the code was written to run at all; the alternative silently
binds nothing, which is the failure mode that is expensive to debug.  Signature
syntax with the feature genuinely off is a construct perl itself warns about.

**Affected tests:** `perl-tests/signatures.t` t1
(`is &t000(456), 123, "(\$a) not signature when not enabled"`) — PCL returns
456.  Guard for the intended behaviour: `Pl/t/transpile-test-07.t`.

---

## Bare `if` with empty true branch  [RESOLVED on the default v2 pipeline]

**Perl behaviour:** `sub f { if(1) {} }` returns `undef`.  When the condition is
true but the branch body is empty, the last expression evaluated is the empty block,
which produces `undef`.

**PCL behaviour:** The default (v2) pipeline matches perl: its `--pcl-if-ret--N`
ret-var transform overwrites the saved condition with the branch body's value
(`(progn)` = `nil` = undef for an empty branch), so `f()` is `undef`.  Only the
legacy `PCL_V1=1` pipeline still returns the condition value (`1`) — its tail-if
transform saves the condition into the ret-var and an empty branch never
overwrites it.

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

## Ref aliasing (`use feature 'refaliasing'`)  — IMPLEMENTED s396 (#325)

> **NO LONGER A LIMITATION for the assignment forms.**  `\$x = \$y`,
> `\my $x = \$y` (and the `@`/`%` spellings), `our \$T = \$::TODO`,
> `\$h{k} = \$v` / `\$a[i] = \$v`, `\&c = \&d`, the list spellings
> `(\$x) = @_` / `\($x) = @_` / `\(my $p) = @_` / `\my($s) = @_`, and the
> FOREACH spellings `for \my %e (@list)` / `for \%::a (@list)` all alias,
> probed row-for-row against perl 5.40.3.  Guard: `Pl/t/refaliasing-01.t`.
>
> The mechanism is one arm in `p-setf`'s place dispatch: a `\`-cast lvalue
> REBINDS THE NAME'S STORAGE to the right-hand referent, which in PCL's model
> is exactly "both names now hold the same box / vector / hash-table".  Before
> it, four of the six spellings were **silent wrong** (the write landed in a
> throwaway ref box) and two were hard refusals.

**What is still missing** (each its own cause, none of them the alias itself):

- **`my \$x` in RVALUE position** (`is \$x, my \$y, …`) — the declarator as an
  expression whose value is a ref.  `t/op/decl-refs.t` is mostly this plus
  diagnostics.
- **the feature-gating diagnostics** — `Experimental aliasing via reference not
  enabled` when the feature is off, and the `Aliasing via reference is
  experimental` warning when it is on.  PCL accepts the syntax unconditionally
  and warns for neither; 135 of `t/op/decl-refs.t`'s rows are exactly these.
  Same family as the general `use warnings` model (task #221).
- **`\state @a = [1..3]`** — blocked by the pre-existing non-scalar `state`
  outside a named sub refusal (#314 family F-F), not by aliasing.  Blocks
  `t/op/lvref.t`.

**Affected tests:**
- `perl-tests/substr.t` — last block (`{ # [perl #132527] ... }`) commented out (1 test)
- `perl-tests/aassign.t` — blocks at lines 124–175 and 284 use refaliasing
- `perl-tests/each.t` — block at lines 319–320 uses refaliasing

---

## Readouts of perl's own internals: `B::` optree inspection, `re::optimization`, `XS::APItest`

**What this covers:** test code that inspects perl's IMPLEMENTATION rather than
the language perl implements — the optree its compiler built and the decisions
it made while building it, the regex engine's optimizer state, the SV-level
bookkeeping XS sees.  These are not observable Perl SEMANTICS: two conforming
implementations of the language can disagree on every one of them and both be
right.  PCL compiles to Common Lisp and matches with cl-ppcre, so there is no
answer it could give that would be *perl's* answer.

**What would lift it:** nothing.  PCL has no perl optree, no perl SV, and a
different regex optimizer, so the only way to report these numbers would be to
reimplement perl's compiler and matcher alongside PCL's own purely so a test
file could inspect them.  This section is where a registration goes when it is
unfixable by construction rather than merely unimplemented — a module PCL could
one day shim (IPC::SysV, XS::APItest's *language* surface) is ordinary
not-implemented and gets its own entry instead.

**Counting the class:** every suite registration that cites *this section name*
is a member, so `grep -c 'Readouts of perl.s own internals'
docs/perl-suite-expected.tsv` IS the population (ruled s397,
`docs/fable-answers-s396.md` §4).

### `re::optimization` — the regex engine's internal optimizer report

`re::optimization($qr)` (XS, in perl's `re` module) returns a hashref
describing what perl's regex COMPILER decided about a pattern: `minlen`,
`minlenret`, the `anchored` and `floating` substrings with their offsets and
utf8-ness, `noscan`, `isall`, `skip`, `implicit`, the anchor flags
(`SBOL`/`MBOL`/`GPOS`) and the start-class (`stclass`).  PCL: absent —
`t/re/opt.t` dies `undef-fn: re::optimization`.  cl-ppcre's optimizer is a
different program making different decisions.

**Affected tests:** `t/re/opt.t` (639 rows).  Registered XDIFF.  Found s396
while implementing refaliasing: the file's ONLY blocker had been the
`our \$TODO = \$::TODO` on line 46, and task #325 sized its 639 rows as
reachable on that basis — with the declaration lowering fixed, the file
transpiles and runs, and this is what it hits instead.

### Constant-folding / inlinability and the `:method` attribute readout

`t/op/const-optree.t` asks, for ~40 spellings of `sub () { … }`, whether
*perl's compiler* turned the sub into an inlinable constant, and whether the
`:method` attribute was recorded.  Neither is asked directly: inlinability is
read out of the exact wording of a redefinition warning (`Constant subroutine
… redefined` vs `Subroutine … redefined`), and `:method` out of whether
`use warnings 'ambiguous'` produced `Ambiguous call resolved as CORE::time()`.
Both are diagnostics ABOUT an optree PCL does not build.

**That file is NOT registered, and this subsection is why it is worth reading
before someone registers it (s399).**  The s397 ruling authorised registering
it on the premise that *every* diverging row is such a readout.  The per-row
read the bar demands says otherwise — of its 62 diverging rows:

* **53** are the internals readouts above (28 `… is/is not inlinable`,
  25 `… has (no) :method attribute`) — this section;
* **5** are `… now throws exception (RT 134138)`: perl REJECTS
  `sub () { $x }` when `$x` is modified elsewhere ("Constants from lexical
  variables potentially modified elsewhere are no longer permitted"), PCL
  compiles it and closes over `$x` — §Error compatibility for invalid Perl
  input (CLAUDE.md §9), a different blessed class;
* **4** are `retval of my sub …` — a REAL FIX TARGET, not a readout: PCL
  compiles `my sub x () { 8 }` as a PACKAGE sub, so two lexical subs of the
  same name in different scopes clobber each other and every `\&x` resolves to
  the last one (probed: `sub { my sub x () {8} \&x }` and
  `sub { my sub x () {3} \&x }` give perl `8 3`, PCL `3 3` — silent wrong).
  Task #337.

All-or-nothing (the `docs/perl-suite-expected.tsv` header rule): those last
four keep the file UNEXPLAINED, because a registration would silence them.
Re-register it when #337 lands — with the reason citing this section *and*
§Error compatibility for invalid Perl input, and with a fresh per-row read.

---

## Triple (and higher) dereference without braces: `$$$ref`  — RESOLVED s390 (#305)

> **NO LONGER A LIMITATION.**  `$$$ref`, `$$$$ref`, the arrow forms
> (`$$$rrr->{k}`), the no-arrow element forms (`$$$rr{k}`) and the
> mixed-sigil runs (`@$$arr[0,1]`, `%$$hrr{"a"}`) all work and are probed
> byte-identical to perl.  The PPI mis-lex is repaired by ONE token pre-pass,
> `Pl::PExpr::_split_pid_magic_cast_run`; see `docs/ppi-upstream-bugs.md` §1.
> Bare `$$` is still the PID (guarded).  The rationale below is kept as the
> record of why it was deferred — note it proposed a fragile SOURCE rewrite in
> `_preprocess_source`; the fix that worked is a TOKEN-stream repair instead.

**Perl behaviour:** `$$$ref` is a triple dereference: Perl parses it as
`${$$ref}` — dereference `$$ref` (which is itself a scalar ref), then
dereference the result.

**PCL behaviour:** The `$$$ref` syntax does not work.  PPI — the Perl parser
PCL relies on — tokenizes `$$$ref` as the two-token sequence `$$` (the special
PID variable) followed by `$ref`, rather than as a triple dereference.  PCL
therefore generates incorrect code and typically emits a PARSE ERROR.

**Workaround:** Write the explicit block form `${$$ref}`, which PPI tokenizes
correctly and PCL handles:

```perl
my $y = 42;
my $r  = \$y;
my $rr = \$r;
print ${$$rr};   # prints 42 — works in PCL
print $$$rr;     # PARSE ERROR in PCL — use block form above
```

**Rationale:** This is a PPI tokenizer limitation, not a PCL semantic gap.
Fixing it would require either patching PPI or adding a pre-processing pass
in `Pl/Parser.pm`'s `_preprocess_source()` to rewrite `$$$var` → `${$$var}`
before PPI sees the source.  That rewrite is fragile (e.g. `$$\$ref` is not
the same) and `$$$var` is extremely rare in real code; `${$$var}` is the
idiomatic form taught in Perl documentation.

**Affected tests:** `perl-tests/test_ref_pass.t` (removed; the `${$$ref}` form
is covered by `Pl/t/transpile-test-02.t`).

---

## Context propagation into string eval

> **Update (session 250): lexical *variable* capture is now IMPLEMENTED** — the
> eval'd code reads and writes the enclosing scope's `my` lexicals, and closures
> built inside the eval close over them (see
> [`docs/eval-lexical-capture.md`](eval-lexical-capture.md)). This section is now
> scoped to the remaining gap: propagation of the *calling context*
> (`wantarray()` / scalar-vs-list), which is still deferred.

**Perl behaviour:** `eval "code"` inherits the calling context.  Code inside the string
eval can call `wantarray()` and get the correct answer; built-ins such as `%hash{keys}`
emit a warning when the eval is called in scalar context (e.g. `scalar eval '...'` or
`my $v = eval '...'`).

**PCL behaviour (corrected s319 — MEASURED, the previous text overstated the gap):**
`p-eval` calls the `pl2cl` subprocess to transpile the string, then calls `(load ...)`
in the same SBCL process.  Because `*wantarray*` is a CL dynamic variable, `(load ...)`
inherits whatever dynamic binding is in scope at the call site — and that turns out to
be enough for two of the three contexts.  PCL still emits no explicit
`(let ((*wantarray* ctx)) (p-eval ...))` wrapper, so what actually happens is:

| call site | perl | PCL | |
|---|---|---|---|
| `eval $code;` (void) | `V` | **`S`** | **WRONG — the only real gap** |
| `my $s = eval $code;` (scalar) | `S` | `S` | correct (inherits the ambient scalar binding) |
| `my @l = eval $code;` (list) | `A` | `A` | correct (inherits the list binding) |

So `wantarray()` inside a string eval IS reliable in scalar and list context; the
remaining divergence is that a **void-context** eval reports scalar, because nothing
binds `*wantarray*` to `:void` at a void call site and the ambient binding is `nil`
(= scalar).  Context-sensitive behaviour keyed on void-ness (e.g. warnings that only
fire in non-void context) therefore still misfires.

Probe: `t/op/wantarray.t` (the `$qcontext` block, ~line 44) — one failing row out of 28.
Tracked as a fix target, NOT a permanent limitation; it is gated behind the VOID_CTX
sub-body-wrap regression named in CLAUDE.md §8, which must be fixed before further
wantarray work.

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

---

## Runtime `$ENV{TZ}` changes not reflected in `localtime`

**Perl behaviour:** Changing `$ENV{TZ}` at runtime causes subsequent `localtime` calls to use
the new timezone, because Perl calls `tzset()` (or equivalent) after each `$ENV{TZ}` assignment.

**PCL behaviour:** `p-localtime` uses CL's `decode-universal-time`, which reads the system timezone
at load time and does not re-query the C library's `TZ` environment variable on subsequent calls.
Setting `$ENV{TZ} = "GMT+5"` has no effect on results returned by subsequent `localtime` calls.

**Rationale:** CL's `decode-universal-time` is a portable abstraction that does not expose the
underlying `tzset()` call.  Implementing this correctly would require FFI to `tzset()` and
re-reading SBCL's timezone offset on every `localtime` call — not worth the complexity for code
that rarely changes TZ at runtime.

**Affected tests:** `perl-tests/time.t` test 7 — commented out and replaced with `ok(1, 'SKIP: ...')`.

## `Hash::Util` bucket statistics

**Perl behaviour:** `Hash::Util::bucket_ratio(\%h)` returns a string like `"3/8"` describing
how many hash buckets are used vs. available.  `Hash::Util::num_buckets`, `bucket_array`, and
`bucket_stats` expose similar internal CL-level metrics.  `perl-tests/hash.t` uses these in the
`torture_hash` / `validate_hash` routines to verify hash bucket behaviour across insertions and
deletions.

**PCL behaviour:** Not implemented.  CL hash tables have completely opaque internals — there is
no API for querying bucket count or fill ratio.  The `Hash::Util` module is not available in PCL's
`lib/` tree.

**Rationale:** No CPAN module in scope depends on `Hash::Util` bucket statistics; they exist only
for Perl's own internal test suite.  Replicating them would require either maintaining a parallel
bucket-metadata table alongside every CL hash table (high cost) or forking SBCL's hash
implementation (not feasible).

**Affected tests:** `perl-tests/hash.t` — the `validate_hash` / `torture_hash` calls (originally
~225 tests) are commented out.  Remaining failures in `hash.t` are DESTROY-via-GC and tie/weak-ref
tests, also documented as not-supported.

## `${^MAX_NESTED_EVAL_BEGIN_BLOCKS}`

**Perl behaviour:** This special variable (introduced in Perl 5.38) controls the maximum number
of nested `BEGIN` blocks allowed inside `eval "string"`.  Setting it to 0 prevents any `BEGIN`
blocks from running inside string eval; higher values limit nesting depth.

**PCL behaviour:** Not implemented.  PCL does not recognize `${^MAX_NESTED_EVAL_BEGIN_BLOCKS}`
as a special variable and will report a transpile error if it appears in source.

**Rationale:** This is a very new, rarely-used safety valve for deeply nested `eval` in
adversarial code.  No CPAN module in scope uses it.

**Affected tests:** `perl-tests/eval.t` — the block using `${^MAX_NESTED_EVAL_BEGIN_BLOCKS}`
is commented out (6 tests).

## `tie` on an ARRAY or HASH  [INTERIM — announced, not silent; scalar tie works]

**Perl behaviour:** `tie @a, 'Tie::StdArray'` / `tie %h, 'Tie::StdHash'` route
every element read and write through the tie object's `FETCH`/`STORE`/
`EXISTS`/`DELETE`/… methods.

**PCL behaviour:** the tie is **ignored**, and the program continues on the
untied aggregate.  As of s320 this is no longer silent: `p-tie` prints one
loud line to stderr —

```
PCL: tie: a HASH (class Tie::StdHash) is not implemented — the container is left untied (task #155)
```

— once per (kind, class) per process, and returns as before.  (The line comes
from the shared `%p-announce-unsupported` helper since s339, which is why the
class rides in the operand: that is what keeps the per-class dedup.)  **`tie` on a
SCALAR is fully implemented** (`p-tie-proxy`: `unbox` dispatches `FETCH`,
`box-set` dispatches `STORE`) and is unaffected.

**Why:** a scalar carries a `p-box` — a place with slots for `sv-ok`, `nv-ok`,
class, magic — and installing a tie proxy is just writing that box's value
slot.  An aggregate carries *nothing*: it arrives at `p-tie` as a raw CL
hash-table or vector, with nowhere to hang the proxy.  (Read-only *arrays*
escaped that limit — task #159 encodes the flag in the STORAGE rather than on
the container, which works because "read-only" is exactly "fixed size"; a tie
proxy has no such representation trick available.)  The fix here is the
boxed-aggregate data model, which changes the representation every array/hash
access compiles against.  That is an E5-era design item (Target A: it costs an indirection on
the hottest paths), deliberately **not** started pre-R1.

**Interim, not final.**  A `die` was considered and rejected for R1: it would
turn files that tie a container mid-run (`op/avhv.t`, 38/2 today) into crashes,
i.e. trade an announced wrong answer for an un-registrable one days before a
release.  Announced-wrong is the CLAUDE.md rule 12 minimum; TAP output is
unaffected by the stderr line.  Revisit die-vs-support when the boxed-aggregate
design lands.  Tracked as task #155, ruled in `docs/fable-answers-s318.md` §1.

## Sparse arrays (holes), element aliasing, and SV identity

**Perl behaviour:** A Perl array can have *holes* — index positions that have never
been assigned, which `exists $a[$i]` reports as false and which are distinct from an
element explicitly set to `undef`.  Holes survive being passed to a sub, sliced, or
iterated, and `map`/copy-assignment do not vivify them.  Separately, every array
element is an SV with its own identity: `\$_[0]` aliases the caller's element (so
`\$_[0] == \undef` holds for the shared `&PL_sv_undef`), and reading/refgen of a
not-yet-existing element can lazily create it as a *defelem* magical lvalue.

**PCL behaviour (updated s316e):** A PCL array is a CL adjustable vector of boxes
with `nil` for holes.  **Hole ALIASING is now implemented** as *lazy defelem-lite*
(`%p-defelem-box`, task #127): when foreach/grep/map/`@_` flattening visits a hole
slot it binds a box whose `p-magic-cell` (`:kind :defelem`) reads undef, stays
non-`exists`, and on first WRITE de-magics itself, stores itself into the source
slot, and re-dispatches through `box-set`.  So `for (@a) { $_ = 2 }`,
`grep { $_ = 5 } @a` and `$_[$i] = 5` vivify like perl, read-only iteration does
NOT vivify, and a hole passed to a sub keeps its position in `@_`.  Still
unsupported: per-element SV identity (`\$_[0] == \undef`, the shared read-only
`&PL_sv_undef`), hole-vs-real `exists` after a whole-array COPY (`my @b = @a`
keeps holes where perl vivifies on copy), general `@_` aliasing of NON-hole
elements, and position tracking through `unshift`/`splice` of a live alias.

**Rationale (remaining gaps):** Full defelem/SV-identity requires Perl's SV/refcount
lifecycle — a pervasive change to the box/vector model for behaviour real CPAN code
does not rely on.

> **History:** an eager hole-flagged-box sketch (s295b) was rejected 2026-07-19
> as a hot-path tax (a value sentinel costs every `unbox`, clear-on-write costs
> every `box-set`).  The s316e implementation avoids those placements: it rides
> the magic-cell dispatch that `unbox`/`box-set`/`box-nv`/`box-sv` already
> perform for arylen/`\substr`, creates boxes only for hole slots actually
> visited by a flattening, and never stores a still-magic box into an array
> (the setter de-magics before vivifying).  New hot-path cost is one `null`
> test per flattened element and one `p-magic-cell-p` test in
> `p-aref-unbox-elem`; the exec bench is unchanged (arrhash 0.268s before and
> after, s316e).  Beneficiary: run/fresh_perl.t cases 29/30 and the CPAN
> `for (@a) { $_ = … }` initialization idiom, which silently skipped holes.

**Affected tests:** `perl-tests/array.t` — `&PL_sv_undef` exists/identity, `undef
preserves identity`, `@_ alias to nonexistent elem`, and the holes-through-subs
position rows (registered in `cl/skip-registry.lisp`; the two `lazy element
creation` rows were DROPPED s316e — they pass under defelem-lite).  Also covers the non-creatable
negative-index error-detection cases (`$a[-1] = 0`), which fall under "Error
compatibility for invalid Perl input".

**NOT covered here (still fix targets):** arylen magic (`\$#array`, freed-array length,
`arylen_p`) and the `map +(LIST)` unary-plus parse bug — see `docs/sweep-bug-catalog.md`.

---

## Value of a block whose LAST statement is a `package` declaration

**Perl behaviour:** A `package NAME;` statement is a declaration, not an
expression — it contributes no value.  When it is the *last* statement of a
value-producing block, the block's value is that of the last *expression*
statement before it: `my $t = do { 42; package XT; }` assigns `42`.

**PCL behaviour:** The inline package switch compiles to a
`(p-set-current-package ...)` form; when it is the block's tail form, its
return value (the package name, `"XT"` above) becomes the block's value.
All *scoping* of the switch is correct (compile-time and runtime state
revert at block end — session 282); only the block's *value* diverges, and
only when the trailing statement is the package declaration itself.

**Rationale:** Matching Perl would require per-statement value tracking in
block lowering (save every expression statement's value so a trailing
declaration can be skipped) — the same cost class as the documented "bare
`if` with empty true branch" corner.  A trailing `package` in a
value-consumed `do`/`eval` block is essentially never written intentionally:
the switch has nothing left to apply to.  Decision confirmed 2026-07-11.

**Affected tests:** None in `perl-tests/` (found by the session-282 edge
probe battery, `Pl/t/transpile-test-04b.t` covers the supported shapes).

---

## `sort` comparator `$a`/`$b` re-homing after an inline `package` switch

**Perl behaviour:** `$a` and `$b` in a sort comparator are the globals of the
package the comparator block was *compiled* in.  A `package` statement at the
start of the block re-homes them: `sort { package XO; $b <=> $a } @list`
makes the comparator read `$XO::a`/`$XO::b`, which sort never sets — every
comparison warns "uninitialized value" and returns 0, so the list comes back
in its original order.

**PCL behaviour:** PCL's comparator lambda receives its two elements as
`$a`/`$b` parameters regardless of an inline package switch inside the block,
so the comparator actually works: the example above *sorts* the list
descending (and emits no warnings).

**Rationale:** Faithfully replicating this means making a working comparator
silently non-functional to honour a package-visibility artifact of Perl's
implementation — a pure foot-gun with no conceivable intentional use (code
that wants package-switched comparator helpers calls a named sub instead).
Decision confirmed 2026-07-11.

**Affected tests:** None in `perl-tests/` (found by the session-282 edge
probe battery).

---

## `mro` pragma — DFS default, ordering switch, and full API

**Perl behaviour:** `mro` selects a class's method resolution order. Perl's
**default is DFS** (depth-first); `use mro 'c3'` switches to C3. The pragma also
exposes an introspection/maintenance API: `mro::get_linear_isa($class[,$type])`,
`mro::get_mro`, `mro::set_mro`, `mro::get_isarev`, `mro::is_universal`,
`mro::invalidate_all_method_caches`, plus the `next::method` / `next::can` /
`maybe::next::method` family.

**PCL behaviour:** PCL **always uses C3** method resolution (CLOS
`class-precedence-list`), with no way to switch to DFS. The `mro` pragma's
DFS default, the ability to choose ordering, and most of the introspection API are
not emulated. For the diamond `D → (B,C) → A`, `mro::get_linear_isa('D')` would
yield C3 order `D B C A`, where stock Perl's *default* is DFS `D B A C`.

**Rationale:** PCL's whole object system is CLOS-backed and C3 is what CLOS gives
us; emulating Perl's DFS default (and a per-class switchable order) would mean
re-deriving linearization outside CLOS for no practical gain. Real consumers that
take `\&mro::get_linear_isa` typically don't depend on the exact order — e.g.
`Test2::Util::HashBase` comments *"these are not strictly equivalent, but for our
use we don't care about order."* This is **brute force and ignorance, by design**:
use C3 everywhere and revisit only if a real module is shown to depend on DFS
order or the wider API.

**What IS / will be provided (minimal, C3-only):** so that `require mro` /
`use mro` and `\&mro::get_linear_isa` don't crash at load (they currently do —
`require mro` even mis-parses), PCL ships a minimal C3-only provider. Anything
beyond `get_linear_isa` (DFS ordering, `set_mro`/`get_mro`, `next::method`, …) is
out of scope until needed. Full design + tiers: `docs/mro-plan.md`.

**Affected tests:** any module that `require mro` / `use mro` (Test2 stack via
`Test2::Util::HashBase`); a dedicated `perl-tests/mro.t` if present.

> **Revisit if it becomes a problem:** this is a deliberate, provisional
> simplification, not a closed decision. If C3-only (or the missing `mro` API /
> DFS ordering) is ever shown to actually break a real module's *behaviour* — not
> merely produce a different-looking order — we will reconsider and implement the
> needed pieces per `docs/mro-plan.md` (DFS ordering, `set_mro`/`get_mro`,
> `next::method`, …). The "brute force C3" stance holds only until a concrete case
> proves it insufficient.

---

## `split` implicit LHS-arity limit (`my ($a,$b) = split …` / `() = split …`)

**Perl behaviour:** When a `split` is *directly* the RHS of a list assignment,
Perl passes the number of LHS lvalues **+ 1** as an implicit `LIMIT` to `split`,
so it never produces more fields than the assignment can consume.  The visible
consequence shows up only when the *cardinality* of the split is observed:

```perl
my $n = () = split /,/, "a,b,c";   # Perl: 1  (LHS has 0 scalars → LIMIT 1 →
                                   #           the whole string is one field)
```

The ordinary value-producing forms are unaffected by the limit because the extra
fields are either discarded (`my ($a,$b) = split …` keeps `$a`,`$b` regardless)
or the LHS is an unbounded array (`my @a = split …` → LIMIT 0 = unlimited).

**PCL behaviour:** `p-split` always splits fully (effectively `LIMIT 0`) and does
not know the arity of the enclosing list assignment, so `() = split …` counts the
*actual* field count (`3` above).  Every common case still matches Perl:
`my ($a,$b) = split`, `my ($p,$q,@rest) = split`, `my @a = split`, and
`scalar(my @t = split)` all agree (verified vs perl 5.40).

**Rationale:** Implementing the optimization requires context-dependent `split`
codegen — the generator would have to thread the enclosing list-assignment's
lvalue count down to the `split` call as a `LIMIT`.  That is a pervasive
calling-convention change for a single observable idiom (`() = split`, used to
*count* fields) that real code writes as `scalar(@parts)` instead.  No CPAN
module in scope depends on the field-count of a directly-assigned `split`.

**Found by:** `tools/difftest-ops.pl` context axis (session 241) — `ctx-count
split /,/, $s` → perl `[1]`, PCL `[3]`.  Logged as a deliberate divergence.

---

## `**` returns an exact integer where Perl returns a float (NV)

**Perl behaviour:** `**` is always C `pow()`, so its result is an NV even when
both operands are integers.  Printing an NV goes through `%.15g`, and above
`2**53` the NV cannot represent consecutive integers at all:

```perl
print 2**53;      # perl: 9.00719925474099e+15   (display only — value exact)
print 2**53 + 1;  # perl: 9.00719925474099e+15   (value: precision LOST)
print 2 ** 3 ** 4;# perl: 2.41785163922926e+24
```

**PCL behaviour:** `p-**` (`cl/pcl-runtime.lisp`) returns an **exact** bignum
when base and exponent are non-negative integers within ~1000 bits, so the
three lines above print `9007199254740992`, `9007199254740993` and
`2417851639229258349412352`.  Results agree with perl up to ~`2**49` (≤ 15
significant digits); past that the divergence is *display* below `2**53` and
*value* (PCL is the more accurate one) above it.

**Rationale:** the exactness is load-bearing for our own transpiled code —
`cl/pack-impl.pl` needs an exact `2**($nbytes*8)` and `2**$checksum_width`, and
`lib/Math/BigInt/Calc.pm` builds its masks with `2**$AND_BITS`.  Making `**`
faithful means making it always return a double-float *and* giving those
callers an explicit integer-power helper; both files are ours and editable, so
the fix is possible, just not free (pack.t and bigint are the risk).  **The
user parked it 2026-06-26** ("put that number formatting on the stack").

**Found by:** `tools/difftest-ops.pl` (session ~241, still the standing
residual — s336 re-run: 3 of the 4 remaining mismatches are this one cause).
Memory: `project_power_op_float_divergence`.

---

## Perl 5.38 `class` / `field` / `method` syntax  [DEFERRED — future version]

**Perl behaviour:** Perl 5.38 introduced native object-oriented syntax as an
experimental core feature (`use feature 'class'`, stabilizing across 5.40):
`class Foo { … }` declares a class, `field $x [:param] [= DEFAULT]` declares
per-instance state, `method m { … }` declares a method with `$self`
implicitly bound, `ADJUST { … }` blocks run at construction, and `:isa(Base)`
declares inheritance.  Instances are opaque (not blessed hashes).

**PCL behaviour:** Not implemented.  The `class`/`field`/`method` keywords are
not recognized; a file using them mis-parses (PPI has only partial knowledge
of the syntax) and the transpile fails.

**Why deferred, not rejected:** this is the future of Perl OO and PCL should
support it in a **future version** — but it is a *self-contained surface
feature*, not a semantic blocker.  Its semantics map cleanly onto machinery
PCL already has: a `class` is a package + CLOS class (the existing
`p-defpackage`/`defclass` pair), `field` is per-instance storage (a slot in
the instance box, like today's blessed-hash fields), `method` is a `p-sub`
with an implicit `$self = shift`, `ADJUST` is constructor code (the Moo
`BUILD` shape PCL already exercises), and `:isa` is `@ISA`/C3 (already
CLOS-backed).  The work is mostly *parsing*: PPI's coverage of the new
keywords must be assessed (possibly a `_preprocess_source`-level lowering of
`class`→`package` + `field`/`method` desugaring, the same route `state` and
hex floats took).  Deliberately postponed until after the v2-pipeline
endgame (E4/E5) and the compatibility phase: almost no CPAN code targets the
syntax yet (it went stable only in recent perls), so it blocks nothing
today.

**Affected tests:** `t/class/*.t` (9 files) — registered as expected
divergences in `docs/perl-suite-expected.tsv`.  Revisit as a feature project
(parser desugaring + a `docs/class-feature-plan.md`) once the v2 endgame
lands.

## NUL bytes (and other control characters) in identifiers

**Perl behaviour:** perl's lexer accepts control characters — including NUL —
as the *first* character of an identifier (the old "control-character
variable" path that gives `$^W`-style names their storage): `$\0eq` is a real
variable whose name is the two-character string `"\0eq"`, `&\0eq` calls the
sub of that name, and so on for every sigil.

**PCL behaviour:** Not supported.  PPI does not tokenize a NUL inside an
identifier (the statement mis-parses), and PCL makes no attempt to repair it.
Related existing behaviour: a NUL in a *symbolic reference* string is a
silent no-op (`${"a\0b"}`).

**Rationale (decision confirmed 2026-07-24):** no human-written code names a
variable with a NUL byte; the construct exists only in perl-lexer torture
tests.  Faking it would mean teaching a PPI pre-pass a token class for names
nothing real ever uses.

**Affected tests:** `perl-tests/lex.t` — the five `<sigil> <null> ident`
rows (registered in `cl/skip-registry.lisp`).

---

## Pathological expression nesting depth (≥ ~10k)  [DEFERRED — revisit after Release 1]

**What:** expressions nested thousands of levels deep — the canonical case is
`t/op/cond.t`, which `eval`s a **20,000-deep right-nested ternary** (220 KB
string) as a regression comment on a historical perl SEGV.

**Behavior:** the transpile itself succeeds but needs quadratic memory and
time — PExpr's recursive descent copies each paren subexpression per nesting
level, so live memory ≈ n²/2 array slots (measured 2026-07-23: 335 MB /
785 MB / 2.1 GB / 6.75 GB at depths 2.5k / 5k / 10k / 20k, ~36 s at 20k) —
and SBCL then exhausts its control stack compiling the 20k-deep nested form.
PPI is linear and innocent (~10 KB/level); the generated CL is lean
(~12 B/level) and at depth 2,500 loads and runs correctly.

**Why deferred (user decision, s309):** no real code nests 20k deep — perl's
own compiler goes quadratic on this input (its comment says so), which is why
the test caps at 20k.  Genuine incompatibility families (the
`docs/perl-suite-triage.md` table) take priority.  The sweep runs op/cond.t
safely regardless: it is in `tools/run-perl-suite.pl`'s `%HEAVY` set (solo
phase, nothing running beside it) and the sweep is contained in a
`systemd-run` scope with `MemoryMax=10G`.

**Revisit AFTER RELEASE 1 (the noted discussion, s309):**
1. **Flat-width check first:** verify whether long *flat* expressions
   (`a + a + … ` ×10k, machine-generated-code territory — far more plausible
   in the wild than deep nesting) hit the same quadratic via the
   per-reduction operator scan.  If they do, the fix gains real-world value
   and should be scheduled; if not, deep nesting alone stays parked.
2. **The known fix** (moderate, not a rewrite): parse index ranges over a
   shared element array instead of copying `@$e[...]` slices per level
   (ternary arm `@condition/@true_expr/@false_expr` and the paren-structure
   path); also fixes the quadratic wall time.  SBCL side: larger
   `--control-stack-size` for generated-code compiles, or flatten deep
   right-nested ternary/if chains in codegen.
3. **Not covered by this verdict:** `op/utf8cache.t` and `re/speed.t` share
   the control-stack-exhausted *signature* but almost certainly not the
   cause (utf8cache dies after 2 tests on ordinary code; re/speed smells
   like cl-ppcre recursion on long strings).  Triage those separately as
   real bugs.

## Lexical compile-time hints (`$^H` / `%^H` scoping)

**Perl behavior:** `$^H` (hint bits) and `%^H` (the hints hash) are
*compile-time lexically scoped*: a BEGIN block inside a `{ … }` scope can set
`$^H |= bits` or `$^H{key} = val`, the values are visible while perl compiles
the rest of that scope (including to `eval ""` at run time via the captured
hints, and to `(caller)[8..10]`), and they are automatically RESTORED when
compilation leaves the scope.  This is the mechanism every lexical pragma
(`strict`, `warnings`, `feature`, user pragmas per perlpragma) is built on.
`comp/hints.t` tests exactly this: set-inside-BEGIN, visibility during
compilation, restoration at scope exit, `eval ""` capture, `%^H` not leaking
into runtime.

**PCL behavior:** `$^H` and `%^H` exist as inert always-bound GLOBALS (0 and
an empty hash) so reads, writes, `\%^H` and `keys %^H` never crash — but
there is no scoping: a write persists like any global write, nothing is
restored at scope exit, and `eval ""` sees whatever the globals currently
hold rather than a compile-time snapshot.

**Why (user decision, 2026-07-28 s316h):** PCL transpiles a whole file ahead
of time; it has no interleaved compile-run phase per scope, so honest `%^H`
semantics would mean modeling per-scope compile-time state in the transpiler
and threading captured hint snapshots into every sub, block and string-eval
— deep machinery whose only real consumers are hand-rolled lexical pragmas.
The pragmas real code uses (`strict`/`warnings`/`feature`) are handled by
PCL directly (stubs/semantics), not via `%^H`, so the mechanism's absence
does not block ordinary CPAN code.  Blessed as not-supported to keep the
incompatibility-fix budget on families that do block CPAN code.

**Effect:** `comp/hints.t` runs 17/31 (the passing rows are the unscoped
reads/writes); expected-tsv row cites this section.  A CPAN module
implementing its own lexical pragma via `%^H` + `(caller)[10]` would compile
but its pragma would behave file-globally rather than lexically.

**Revisit if:** a target CPAN module actually relies on `%^H` scoping
(autodie, indirect, namespace::clean are the known heavy users) — at that
point scope-tracked hint snapshots in the transpiler (a per-scope constant
alist captured at transpile time, restored via unwind-protect at runtime
scope exit) is the design sketch.

## String eval with multiple package sections

**What:** an `eval` string that changes package more than once, e.g.
`eval 'package A; …; package B; …'`, and — the same refusal, reached from
the other side — an eval whose leading statements before a `package X;` are
not the narrow initializer shapes the s353 whitelist accepts.

```perl
eval 'package A; our $x = 1; package B; our $y = 2; 1';   # PCL: unsupported
eval 'my $v = some_free_global(); package X; $v';         # PCL: unsupported
```

**Error (perl-shaped, trappable in `$@` exactly like any other eval error):**
`PCL: unsupported in string eval: multiple package sections`

**Why:** an eval body lowers as ONE thunk, and the thunk cannot change the
reader package part-way through. The single-switch shape (`package X;` plus
a body) is supported and common — it lowers AS section X through the D1-lite
qualified emission (#226, RULED s345 §2). A *second* switch would need the
thunk split into independently-packaged sections with values threaded
between them, which is whole-file section machinery inside a runtime eval.
Measured share: one event across the whole sweep + CPAN board, and it is a
shape no surveyed CPAN module uses.

**Owner:** task #242 (the refusal is deliberate; a real consumer re-opens it).

## String eval ending in an unconvertible declaration

**What:** an `eval` string whose last statement is a declaration whose VALUE
the lowering cannot produce. Every measured instance is input perl itself
rejects — `my $$x`, `my $$$x`, `my @$x`, `my($a,$b),$x,my($c,$d)`.

**Error:** `PCL: unsupported in string eval: trailing declaration has no value`

**Why:** an eval returns its last statement's value; for a trailing `my`/`our`
the lowering converts the declaration into a value-producing form, and a few
shapes have no such form. All five sweep events are perl-INVALID source from
lexer-torture rows that assert perl *rejects* them, so a non-empty `$@` is
the answer those rows want (CLAUDE.md principle 9 — PCL is a transpiler for
valid Perl, not a validator, but it must not silently accept nonsense).

**Owner:** task #242.

## A single generated top-level form above 64k characters

**What:** one emitted top-level runtime form whose whitespace-collapsed length
exceeds `$RUN_FORM_MAX` (64,000 chars).

**Error:** `PCL: unsupported: a single generated top-level form of N chars
exceeds the 64000-char limit (it would exhaust the SBCL compiler heap at
load): …`

**Why:** SBCL's register allocator grows superlinearly in form size; a
162k-char form OOMed a 1 GB heap outright, while the largest form in the
whole corpus that loads fine is ~55k. Refusing at transpile time with a
precise message beats emitting CL that crashes SBCL at load with no
attribution. The limit is never raised (RULED s346 §2). The one measured
event is torture-scale generated source; one honest loud row there is the
accepted outcome, and shrinking arbitrary generated forms is not required by
any target.

**Revisit if:** a real CPAN module produces such a form — then the fix is
splitting the run form (option (a): extend `_oversized_top_decls`), not
raising the cap.

**Owner:** tasks #230 / #242.

## An `our` alias whose requalified region contains a nested package statement or an inner-scope re-declaration

**What:** inside one block, an `our @x` declared after an in-block `package`
statement, where the region after a later `package` switch either contains a
*nested* `package` statement, or re-declares the same variable in an INNER
scope (a nested block, a sub body, or a `foreach my $x` head).

```perl
{ package tmp; our @c = (1,2);
  package main;
  { my @c = (9); }     # inner-scope re-declaration
  print "@c\n";        # PCL: unsupported (perl: tmp::c, i.e. "1 2")
}
```

**Error:** `PCL: unsupported: `our` alias for '@c' re-declared in an inner
scope of the same block` (or `… followed by a nested package statement …`).

**Why:** `our @x` binds the bare name to the *declaring* package's variable
for the rest of the enclosing scope, and a later `package` statement does not
re-home it — so the uses after the switch are requalified to the declaring
package. That rewrite is a flat one over the region. A block-level
re-declaration is fine (it simply ENDS the alias, #251/M7), but an
inner-scope one does not: the inner binding expires at its own scope's end
and the OUTER alias RESUMES, which a flat region rewrite cannot express.
Refusing beats requalifying the wrong half.

**Revisit if:** the requalification is rebuilt as a scope-walk rather than a
region rewrite — then both shapes fall out.

**Owner:** tasks #251 / #242.

## `:prototype(...)` on an anonymous sub at the START of an expression

**What:** the prototype attribute is DROPPED (loudly) when the anon sub
carrying it opens an expression — after `(`, `[`, `{` or a `,` that starts a
fresh expression statement:

```perl
my @a = (sub :prototype($$) { 1 }, 2);   # PCL: prototype dropped, announced
my $b = sub :prototype($) ($x) { $x };   # fine — the ordinary spelling works
```

**Message (stderr, at transpile time):** `PCL: attribute `:prototype($$)` on
an anonymous sub at the start of an expression is dropped (PPI lexes it as a
label; see docs/ppi-upstream-bugs.md §7)`

**Why:** PPI 1.291 lexes `sub :attr` at expression start as
`Label('sub :') Word('attr')` (upstream bug, `docs/ppi-upstream-bugs.md` §7).
`Pl::Parser::_extract_prototype_attributes` — which normally turns
`:prototype(…)` into a runtime `__pcl_set_prototype` wrap — keys on a
`PPI::Token::Attribute`, so it never sees this spelling, and it cannot be
re-run after the repair without a serialize+reparse that would just re-create
the mis-lex. The repair (`_normalize_anon_sub_attrs`) therefore drops the
attribute so the code RUNS, and says so.

The loss is effect-only: an anonymous sub has no name for the call-site
parser to consult, so even the correctly-lexed spelling only records the
prototype at runtime. Announcing rather than dying follows the s329 boundary
(`docs/fable-answers-s328.md` §1) — before the repair the whole statement was
silently replaced by a PARSE ERROR comment, which is the failure this entry
exists to prevent.

**Prototypes ending in `$` (s367, #270):** these are the same drop, but they
reach it by a second layer of the mis-lex — `prototype($)`'s closing paren is
tokenized as the magic variable `$)`, so PPI swallows the sub's block into
the attribute's parens (`docs/ppi-upstream-bugs.md` §7b). Every prototype
whose text ends in `$` is affected (`($)`, `(;$)`, `($;$)`, …). They are
repaired at source level before the §7 pass and land on this same
announce-and-drop path; the message is identical. Before s367 they did not
reach the announce at all — the §7 repair declined SILENTLY and the whole
statement vanished at exit 0. That silence is now a die: a `sub :` Label is
only ever produced by this mis-lex, so a run that does not end at a Block is
known-mangled input the repair does not cover, and it says so by name.

**Revisit if:** the mis-lexed spelling turns up in real code (it appears in
neither audit population today — s367 re-measured: 15 sources across both
populations carry a `sub :ATTR` spelling, none of them this one), or PPI
fixes the lexer — then the repair can produce proper `Attribute` tokens and
let the existing extractor run.

**Owner:** tasks #268 / #270.

---

## Attributes on a variable declaration (`my $x : shared`, `my @a : Foo(1)`)

**Perl behaviour:** an attribute list on a `my` / `our` / `state` declaration is
a compile-time call: perl looks up `MODIFY_SCALAR_ATTRIBUTES` /
`MODIFY_ARRAY_ATTRIBUTES` / `MODIFY_HASH_ATTRIBUTES` in the declaring package
(or in the class of a typed lexical) and passes it the package, a reference to
the fresh variable, and the attribute names.  Whatever the handler returns
unconsumed is a compile ERROR — `Invalid SCALAR attribute: Foo at … line N` —
so an unhandled attribute stops the program from compiling at all.
`attributes::get()` reads back what `FETCH_*_ATTRIBUTES` reports.

**PCL behaviour:** the attribute list is STRIPPED from the declaration before
anything else reads the statement, and the declaration then behaves exactly
like the same declaration without it (`my $x : shared = 1` binds and
initialises `$x` normally).  No `MODIFY_*_ATTRIBUTES` handler is called, no
attribute is recorded, and an attribute perl would reject is accepted.  Each
distinct attribute is ANNOUNCED once per file on stderr:

    PCL: attribute `:shared` on a variable declaration is dropped
    (MODIFY_*_ATTRIBUTES is not called; see docs/not-supported.md)

**Rationale:** this is rule 12's effect-only ANNOUNCE case
(`docs/fable-answers-s328.md` §1) — the declaration still binds the right
variable with the right value, so nothing downstream consumes a wrong VALUE;
only the hook does not run.  It is the same treatment attributes on a `sub`
already get (they are dropped by `Pl::PExpr`'s cast/attribute strip), and
stripping is what makes the declaration lower at all: before s395 the `:` was
read as an ordinary trailing operator, so `my $x : shared = 1;` lowered as a
bare `my $x` plus a discarded expression and printed EMPTY — a silent wrong.

**Cost, measured (s395):** `t/op/attrs.t` 0 → 28 ok / 61 not-ok (it was a whole
TRANSPILE-FAIL file), `t/uni/attrs.t` 0 → 8 / 26.  The remaining failures are
this entry: rows that require the compile error, and rows that read attributes
back through `attributes::get` (`undef-fn:attributes::pl-_guess_stash`).

**What would LIFT this:** implementing the attribute protocol — dispatch to
`MODIFY_*_ATTRIBUTES` at the declaration site, die with perl's message on what
comes back unconsumed, and a `lib/attributes.pm` shim for `get`/`reftype`.
That is a feature, not a rephrasing of this entry; filed as task #322 with the
row counts above as its bar.  Nothing about the box model blocks it.

**Owner:** task #322 (attribute protocol); the strip itself is #314 family F-A2.

---

## Regex script-run assertions `(*script_run:…)` / `(*sr:…)` (and the atomic pair)

**Perl behaviour:** perl 5.28+ has `(*script_run: PATTERN)` (short form `(*sr:`)
and `(*atomic_script_run: …)` / `(*asr:`), which match only when every character
the group consumed belongs to a single Unicode script — the defence against
mixed-script spoofing (`раураl` with a Latin `l`).

**PCL behaviour:** not supported.  PCL's regex engine is cl-ppcre, which has no
script-run assertion, so the pattern fails to compile and every assertion built
on it diverges.

**Cost, measured (s395):** `t/re/script_run.t` is 185 rows, all of them.  The
file is registered as XDIFF (`docs/perl-suite-expected.tsv`) — it still runs
and still prints its row count every sweep, so the number stays visible as a
fix target.  Its rows were never passing: before #202 made `unlike()` able to
fail, a pattern cl-ppcre refused to compile was reported as a PASS, which is
why the file's count DROPPED to zero when that hole was closed (s393, #315).

**What would LIFT this:** PCRE2 has had script runs since 10.33, so the PCRE2
backend (task #71) retires this registration outright.  Task #196
(exponential backtracking) is the same engine family.  This is non-support of
the CURRENT engine, not of PCL.

**Owner:** tasks #71 / #196; registration mechanics, task #320.

---

## Regex extended character classes `(?[ … ])`

**Perl behaviour:** perl 5.18+ has `(?[ ... ])`, a set-operation syntax inside a
pattern — union `+`/`|`, intersection `&`, subtraction `-`, symmetric difference
`^`, complement `!`, over bracketed character classes and `\p{}` properties, with
whitespace ignored and nesting allowed.

**PCL behaviour:** not supported — cl-ppcre has no equivalent, and the syntax is
not a rewrite of ordinary alternation (intersection and subtraction have no
plain-class spelling).

**Cost, measured (s395):** `t/re/regex_sets.t`, 96 perl rows.  With
`capture_warnings` added to the transpilable `t/test.pl` stub (task #320 step 1
— it had been an `undef-fn` crash that stopped the file after 53 rows), the
file now runs end to end at 4 ok / 84 not-ok; the 84 are this entry.
Registered as XDIFF, still counted every sweep.

**What would LIFT this:** nothing currently planned — `(?[ ])` is perl-only, and
PCRE2 does NOT implement it, so task #71 does NOT retire this registration
(unlike the script-run entry above).  Lifting it means implementing set algebra
over character classes in the pattern translator.

**Owner:** task #320 (registration); no implementation task filed.

---

## Warnings-gated diagnostics are absent (`use warnings` is not modelled)

**Perl behaviour:** most perl diagnostics are DEFAULT-OFF and switched on by
`use warnings` (or `-w`) in the enclosing lexical scope: `Use of uninitialized
value $x in addition (+)`, `Odd number of elements in hash assignment`,
`Reference found where even-sized list expected`, `Argument "…" isn't numeric`,
and the rest of the `uninitialized`/`numeric`/`misc` categories.

**PCL behaviour:** absent.  PCL tracks no warnings state anywhere (grepped
`Pl/` and `cl/`, s337c), so every diagnostic it has is UNCONDITIONAL — and
unconditional is the wrong answer for a default-off one: emitting `print() on
unopened filehandle` unconditionally broke `fileio-02.t` and
`transpile-test-09.t` (measured s337c).  So the standing rule is that a
default-off diagnostic stays ABSENT rather than being emitted always.

What DOES work, and is not part of this gap: `warn` itself, `$SIG{__WARN__}`
(so `capture_warnings` and the `warning_is`/`warning_like`/`warnings_like`
helpers evaluate honestly), and every diagnostic perl emits unconditionally.

**What would lift it:** task #221 — a minimal model (`use warnings`/`no
warnings` compiled per lexical scope into one dynamic boolean the runtime
consults; no categories).  It was UNSCHEDULED pending its own trigger, "the
first test family or CPAN module whose failure is *warning not emitted*".

**s399 IS that trigger.**  Task #323 replaced three `t/test.pl` stubs that had
been manufacturing a pass (they ran the code and never compared the warning —
the #202 class) with the real implementations, and the rows that had been
silently green became honest failures:

- `perl-tests/assignwarn.t` — 20 rows (96/20 vs 116/0 before), every one a
  `Use of uninitialized value` the compound assignment operators must warn;
- `perl-tests/hashassign.t` — 4 rows (305/4 vs 309/0), the `Odd number of
  elements in hash assignment` and `Reference found where even-sized list
  expected` pair, each in its plain and its `($s, %h)` spelling;
- `perl-tests/time.t` — unchanged at 72/0: its `warning_is` rows expect NO
  warning, and PCL emits none, so they pass honestly.

In the companion suite the population is CLOSED and was measured whole — eight
files call the three helpers anywhere under perl's `t/`, and all eight were
run:

- `t/op/assignwarn.t` 116/0 → 96/20 and `t/op/hashassign.t` 309/0 → 305/4 (the
  same files as above), plus `t/op/numify.t` 32/0 → 21/11 (`Argument "…" isn't
  numeric` — each string's numified VALUE is asserted separately and PCL passes
  those).  All three registered XDIFF; `op/assignwarn.t` opts out of the ROW
  check as `*rows-unstable*` because the file iterates `keys %should_warn`, so
  both sides emit rows in per-process random order.
- `t/op/utf8decode.t` 644/42 → 620/90, and it stays UNEXPLAINED on purpose:
  86 of the 90 are this entry (the malformed-UTF-8 warnings), but 4 —
  "Got expected Unicode characters" — are a PRE-EXISTING divergence of a
  different kind, verified by re-running the file in a worktree at `f8ffd56`.
  All-or-nothing, so no registration until those four have a cause.  Note the
  file's row count now MATCHES perl's 710: the old stub emitted one row where
  perl emits two, which is what produced the "PCL's TAP numbering is offset for
  592 rows" note the snapshot used to carry.
- `t/op/time.t`, `t/op/inc.t`, `t/op/split_unicode.t`, `re/subst.t` — UNMOVED.

**Owner:** task #221.  A row failing for this reason is a registration, not a
regression — and it stops being one the day #221 lands.
