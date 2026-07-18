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

## `__SUB__` (current sub reference)  [DEFERRED (non-eval case) — see roadmap]

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
`p-goto-computed` is a no-op.  (`goto &sub` — the tail-call form — *is* supported.)

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

## Triple (and higher) dereference without braces: `$$$ref`

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

## Sparse arrays (holes), element aliasing, and SV identity

**Perl behaviour:** A Perl array can have *holes* — index positions that have never
been assigned, which `exists $a[$i]` reports as false and which are distinct from an
element explicitly set to `undef`.  Holes survive being passed to a sub, sliced, or
iterated, and `map`/copy-assignment do not vivify them.  Separately, every array
element is an SV with its own identity: `\$_[0]` aliases the caller's element (so
`\$_[0] == \undef` holds for the shared `&PL_sv_undef`), and reading/refgen of a
not-yet-existing element can lazily create it as a *defelem* magical lvalue.

**PCL behaviour:** A PCL array is a CL adjustable vector of boxes.  It has no
hole/`undef`/defelem distinction (a missing slot reads as `undef` and may shift or
drop when the vector is rebuilt), no shared read-only `&PL_sv_undef`, and no
per-element SV identity that survives copying.  `@_` elements are copies, not aliases
(see "`@_` argument aliasing"), so writing through `$_[$i]` does not autovivify the
caller's element, and a hole passed to a sub loses its position.

**Rationale:** Emulating holes/defelem/SV-identity requires a sparse representation
with per-element magical lvalues and Perl's SV/refcount lifecycle — a pervasive change
to the box/vector model for behaviour real CPAN code does not rely on.

> **Revisit sketch (s295b, if ever needed):** the faithful middle path is a *lazy
> defelem-lite* — when `map`/`grep`/`foreach` flattening visits a nil (hole) slot,
> mint a **hole-flagged box**, store it into the slot, and alias `$_` to it.  The
> box then travels with `unshift`/`splice` (position tracking for free), `exists`
> reports false while flagged, and a write clears the flag.  Rejected because every
> placement of the flag taxes a hot path: a value sentinel costs every `unbox`, a
> clear-on-write costs every `box-set`, a side-table can't see writes without
> hooking `box-set` anyway, and the `p-magic-cell` route collides with the
> box-set-FETCHes-tie-proxy semantics.  Beneficiaries are only the perl #132729
> regression rows (array.t t189/t190 and the holes-to-sub family) — decision
> re-confirmed with the user 2026-07-19: not worth a hot-path tax (CLAUDE.md §2,
> speed wins).

**Affected tests:** `perl-tests/array.t` — `&PL_sv_undef` exists/identity, `undef
preserves identity`, `@_ alias to nonexistent elem`, `lazy element creation`,
`map {} @a does not vivify elements`, and `holes passed to sub do not lose their
position` (registered in `cl/skip-registry.lisp`).  Also covers the non-creatable
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
