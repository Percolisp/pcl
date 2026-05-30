# PCL — Remaining Work and Limitations

A reader-friendly overview of what PCL does and, more importantly, what it
**deliberately does not** do. For the full rationale and the exact affected
tests, see [`docs/not-supported.md`](docs/not-supported.md); this file is the
short version.

**Current status:** 85 PCL regression-test files (≈3072 tests), all passing.
Against Perl's own test suite (`perl-tests/`, 106 files) PCL passes the large
majority of individual tests; remaining failures are dominated by the
deliberate non-support items below plus a handful of open bugs tracked in
[`docs/sweep-bug-catalog.md`](docs/sweep-bug-catalog.md).

---

## What works

PCL covers the bulk of everyday Perl. In broad strokes:

- **Expressions:** all operators with correct precedence, ternary, ranges,
  string/list repetition, every number literal form (hex/octal/binary/underscored).
- **Control flow:** `if`/`elsif`/`else`/`unless`, `while`/`until`, C-style and
  list `for`/`foreach`, `last`/`next`/`redo`, loop labels, `goto &sub` and
  `goto LABEL` (tagbody).
- **Data:** scalars, arrays, hashes, references and dereferencing, slices,
  anonymous `[]`/`{}`, nested structures, autovivification.
- **Subs:** signatures with defaults, old-style prototypes (including `&` block
  and `\@`/`\%`/`\$` reference auto-boxing), `wantarray` (list/scalar/void),
  closures, recursion, `state` variables.
- **Built-ins:** the common string/list/math/IO set — `print`, `say`,
  `sprintf` (incl. `%g`/`%e`/`%a`/`%v` and positional args), `substr`, `split`,
  `join`, `sort`, `map`, `grep`, `pack`/`unpack` (most formats), and many more.
- **Regex:** match, `s///`, `tr///` with the usual flags and capture groups,
  via CL-PPCRE.
- **OO:** `package`, `bless`/`ref`, method calls, `@ISA` inheritance with C3
  MRO (CLOS-based), `SUPER::`, `can`/`isa`, multiple/diamond inheritance.
- **Modules & scoping:** `use`/`require` for pure-Perl modules, `use lib`,
  `use constant`, `our`/`local` (including `local` on hash/array elements and
  globs), package variables, cross-package refs.
- **Other:** string interpolation (incl. `$a[0]`, `$h{k}`, chained
  `$h->{a}[1]`), heredocs, `BEGIN`/`END` blocks, `eval { }` **and**
  `eval "string"`, `s///e`, `%ENV`, file I/O, time/system calls.

---

## What is deliberately NOT supported

These are design decisions, not bugs. Each replicates a Perl *interpreter*
implementation detail or a removed/niche feature that real CPAN code doesn't
depend on, and emulating it would cost a great deal for no practical gain.

### Perl interpreter internals (the SV/refcount model)
CL uses values and a garbage collector, not Perl's reference-counted SVs, so:

- **`@_` argument aliasing** — `$_[0] = 42` does **not** modify the caller's
  variable; PCL copies arguments.
- **Sparse arrays / holes / element identity** — no distinction between a
  never-assigned slot and `undef`; no shared read-only `&PL_sv_undef`; no
  lazy "defelem" lvalues; `\$_[0]` does not alias.
- **Interned boolean identity** — `!0`/`!1` return fresh mutable values, not a
  shared read-only scalar.
- **Read-only scalars & `Internals::*`** — `Internals::SvREADONLY`,
  `SvREFCNT`, `\undef` stash tricks: no per-box read-only flag or refcount.
- **`DESTROY` on garbage collection** — never called automatically; CL's GC
  gives no deterministic finalizer timing. (You can call `$obj->DESTROY()`
  explicitly.) Weak refs and `DESTROY`-driven cleanup are out of scope.

### Removed, deprecated, or experimental Perl features
- **`given`/`when` and smart match `~~`** — experimental since 5.10, removed in 5.38.
- **`?pattern?` one-match regexes and `reset()`** — `?pattern?` removed in 5.38.
- **Ref aliasing** (`use feature 'refaliasing'`, `\$x = \$y`) — experimental;
  never graduated to stable; removed in 5.40.
- **`format`/`write`** report templating — legacy, unused in modern CPAN.

### Error messages and input validation
PCL targets *running valid Perl*, not reproducing the interpreter's diagnostics:

- **Exact error wording** is not matched (`like($@, qr/...text.../)` tests).
- **The `" at FILE line N."` suffix** is not appended — no runtime
  source-location tracking.
- **`caller()` filename/line** — package name is correct; filename is
  `"(unknown)"` and line is `0`.
- **Invalid-Perl rejection** — PCL is a transpiler, not a linter; it won't
  produce Perl's compile-time errors for malformed input.

### Unicode and encoding edge cases
CL strings are always Unicode (no per-scalar UTF-8 flag):

- **`utf8::encode`/`decode`**, **`use bytes`**, **`pack 'U'` UTF-8 flag** — the
  byte-vs-character flag doesn't exist.
- **Regex encoding modifiers `/a`, `/d`, `/l`, `/u`** — accepted but ignored;
  CL-PPCRE always uses (roughly) `/u` semantics.
- **Some multi-char case mappings** and **`\p{IsWord}`** on non-ASCII differ
  from Perl.

### XS / C-level features
PCL loads only pure-Perl modules:

- **DynaLoader / XS binary extensions** — anything reaching
  `DynaLoader::bootstrap`. (XS bridge is a possible future phase.)
- **`pack`/`unpack` pointer types `p`/`P`** (no stable addresses under a moving
  GC) and **80-bit long double `D`** (SBCL has only 64-bit doubles).

### Niche syntax and introspection
- **Lvalue subs (`:lvalue`) and `substr`-as-lvalue** — use the 4-arg
  `substr($s,$o,$l,$repl)` form instead.
- **`prototype()`** always returns `undef` (the *behaviour* of prototypes works;
  only the introspection string is missing).
- **`__SUB__`**, **`${^MAX_NESTED_EVAL_BEGIN_BLOCKS}`** — not recognized.
- **Hex float literals `0x1.8p+1`** and **triple deref `$$$ref`** — PPI
  tokenizer limitations; use `${$$ref}` for the latter.
- **`Hash::Util` bucket statistics** — CL hash internals are opaque.

### Semantics that need a different execution model
- **Lazy/left-to-right argument side effects** — CL evaluates all arguments
  before the call, so a `$SIG{__WARN__}` handler mutating a later argument
  mid-build isn't observed.
- **`$SIG{__DIE__}` handlers** — not invoked (`$SIG{__WARN__}` is).
- **Context into `eval "string"`** — `wantarray()` inside a string eval isn't
  reliably context-aware yet (needs the deferred AST context annotations,
  [`docs/ast-annotation-plan.md`](docs/ast-annotation-plan.md)).
- **Runtime `$ENV{TZ}` changes** — not reflected by later `localtime` calls.

---

## Open work (bugs, not non-support)

Genuinely fixable gaps still being worked are tracked in
[`docs/sweep-bug-catalog.md`](docs/sweep-bug-catalog.md) and
[`docs/session-log.md`](docs/session-log.md). Current notable items include
arylen magic for freed/symbolic-ref arrays, the `map +(LIST)` unary-plus parse
case, and assorted `sprintf` warning-marker outputs. These are fix targets, not
declared limitations.

---

*For the complete rationale, the exact affected tests, and the skip-registry
mechanics, see [`docs/not-supported.md`](docs/not-supported.md) and
[`docs/test-skip-registry.md`](docs/test-skip-registry.md).*
