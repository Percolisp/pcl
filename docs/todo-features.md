# PCL: Features Left To Implement

This document lists Perl features that are **planned for implementation**
but not yet done.  Unlike `docs/not-supported.md` (design decisions), every
item here is a real bug or missing feature worth fixing.

Items are grouped by theme and roughly ordered by number of tests affected.
Cross-references to `PERL_TEST_SUITE_PLAN.md` sections are in parentheses.

---

## Tier 1 — High value (many tests, clear root cause)

### Tie::Array / Tie::Hash module loader hang  (~200+ tests, 4 files blocked)

**What's broken:** When any test file does `require Tie::Array` or
`require Tie::Hash`, PCL's module loader enters an infinite recursion
or binding-stack exhaustion.  The process hangs rather than failing
cleanly.

**Blocked files:** `sort.t`, `reverse.t`, `local.t`.

*(Note: `kvhslice.t` was previously listed here but is now **fully passing**
after the session 73 map fat-comma / `pl-hash-=` flattening fix.)*
*(Note: `kvaslice.t` is now **17/17 passing** (session 90) — Tie tests pass,
unsupported-feature tests commented out.)*

**Fix area:** `Pl/Parser.pm` `_process_use_statement` / `pl-require-file`
in `cl/pcl-runtime.lisp`.  Likely a circular dependency or re-running
`defpackage` init for an already-existing package.

*(PERL_TEST_SUITE_PLAN.md §A)*

---

### Implicit returns / bare-`if` return value  (widespread)

**What's broken:** In Perl, a subroutine returns the value of the last
expression evaluated.  For `sub { if(COND) { BODY } }` with no `else`,
if COND is false then *COND itself* is the last thing evaluated, so the
sub returns that false value (not `undef`/nil):

```perl
sub x { if(0)  { 5 } }   # returns 0, not undef
sub x { if("") { 5 } }   # returns "", not undef
sub x { if($n) { 5 } }   # returns $n when $n is false
```

PCL generates `(pl-if cond (progn body))` which returns NIL on the false
branch.

**Fix:** When a bare `(pl-if COND BODY)` (no else) is the tail expression
of a block, evaluate COND once and return it on the false branch:
```lisp
(let ((--c-- COND)) (if --c-- (progn BODY) --c--))
```
This requires annotating which `if` is in tail position.  See
`docs/rewrite-patterns.md` for a tree-annotation approach.

**Fix area:** `Pl/Parser.pm` `_process_if_statement`.

*(PERL_TEST_SUITE_PLAN.md §C)*

---

### ~~`kvhslice.t` / `map { k => v }` fat-comma~~  ✅ DONE (session 73)

`kvhslice.t` (3/3) now fully passes.  Root cause: `map { $_ => uc $_ }` was
not returning key-value pairs because comma expressions in function bodies
only returned the last value (scalar `progn` semantics).

**Two-part fix:**
1. `gen_progn` (ExprToCL.pm): SCALAR_CTX with ≥2 forms now generates
   `(if *wantarray* (vector ...) (progn ...))` for runtime list/scalar dispatch.
2. `pl-map` runtime: runs block in list context, flattens per-iteration
   vector results into the output.
3. `pl-hash-=` now uses `%pl-flatten-list` before iterating key-value pairs,
   handling nested vectors and odd-length inputs without crashing.

---

### ~~`hashassign.t` mass failures~~  ✅ DONE (session 71)

All 309 tests in `hashassign.t` now pass.  Fixed by the `pl-bless` scalar
ref fix (class set on wrapper box, not on pointed-to variable) and the
`%pl-flatten-list` fix (blessed non-hash boxes preserved through list
assignment).

*(PERL_TEST_SUITE_PLAN.md §E)*

---

### `index.t` / `rindex` failures  (~414 tests)

**What's broken:** Nearly all of `index.t` fails.  Root cause unclear;
likely a boxing issue in `pl-index` / `pl-rindex` for common cases.

**Quick check:**
```bash
echo 'print index("abcdef", "cd"), "\n";' | ./pl2cl | sbcl --script /dev/stdin
```
Expected: `2`.

**Fix area:** `cl/pcl-runtime.lisp` `pl-index`, `pl-rindex`.

*(PERL_TEST_SUITE_PLAN.md §F)*

---

### `$SIG{__WARN__}` / `$SIG{__DIE__}` handlers  (~50 tests)

**What's broken:** `$SIG{__DIE__}` handler is never invoked by `pl-die`.
`$SIG{__WARN__}` is invoked but the full handler protocol (re-throwing,
nested handlers) is incomplete.

**Fix area:** `cl/pcl-runtime.lisp` `pl-die` / `pl-warn`.  `pl-die`
needs to call the handler before unwinding; implementing this cleanly
requires CL condition restarts.

*(PERL_TEST_SUITE_PLAN.md §D / §1.4)*

---

### ~~Inline `package Pkg {}` inside a subroutine body~~  ✅ DONE (session 72)

**Root cause:** `_process_package_statement` called `_emit_package_preamble`
for block-form packages unconditionally, opening a new section and leaving
subsequent code in that new top-level section (not inside the function body).

**Fix:** When `in_subroutine > 0`, emit the package setup inline (no new
section, no `(in-package ...)`). Temporarily increment `_block_depth` so
`_process_sub_statement` emits fully-qualified names like `|Point|::pl-new`
instead of `pl-new`. The `pl-sub` macro's `(symbol-package ',name)` dispatch
correctly interns the symbol in the right package.

**Result:** `index.t` 0→518 tests passing. `substr.t` also unblocked.

*(PERL_TEST_SUITE_PLAN.md §N)*

---

### ~~Package-qualified variable declarations (`$Pkg::var`)~~  ✅ DONE

`p-scalar-=` auto-vivifies the box via `(unless (boundp ',place) ...)`,
so `$Dog::VERSION` works correctly with or without a prior `package Dog;`
block.  Verified in session 90.

---

## Tier 2 — Medium value (tens of tests, clear approach)

### String `eval "code"`  (~50 tests)

**What's broken:** `eval "string"` must parse and execute arbitrary Perl
at runtime.  Currently stubbed to run `$@` = "not implemented".

**Approach:** At runtime, `pl-eval-string` calls out to the PCL transpiler
via a subprocess, gets CL back, and `eval`s it.  `*pcl-pl2cl-path*` is
already set in the generated preamble.

```lisp
(defun pl-eval-string (str)
  (handler-case
    (let* ((cl-code (pcl-transpile str))
           (result  (eval (read-from-string cl-code))))
      (make-pl-box result))
    (error (e)
      (setf $@ (format nil "~A" e))
      *pl-undef*)))
```

**Caveats:** The eval'd code runs in a fresh package context (correct per
Perl semantics — string eval doesn't capture `my` vars from the caller's
scope, only package vars).

*(PERL_TEST_SUITE_PLAN.md Phase 3)*

---

### ~~Chained method calls: `$obj->m1()->m2()`~~ ✅ DONE (session 70)

**Root cause:** In `Pl/PExpr.pm` `handle_subcalls`, `print/say/printf` with
an uppercase bareword next token treated it as a filehandle unconditionally.
So `print B->new()->name()` had `B` spliced out as a filehandle, leaving
`-> new () -> name ()` with `->` at position 0 → parse error.

**Fix (1 line):** Before marking an uppercase bareword as a filehandle,
check if the token AFTER it is `->`. If so, it's a class method invocant
(`B->new()`), not a filehandle. Skip the filehandle treatment.

**Regression tests:** `Pl/t/transpile-test-03.t` tests 40–42.

*(PERL_TEST_SUITE_PLAN.md §G)*

---

### ~~Foreach loop variable capture in closures~~ ✅ DONE (session 70)

The actual root cause was NOT in `pl-foreach` — the `let` per iteration
was already fresh. The bug was in `Parser.pm` `_with_declarations`:

1. `in_subroutine > 0` guard prevented closure-var renaming at package level.
2. `_process_variable_statement` routed package-level `my` to `_process_my_toplevel_declaration` before the rename path.
3. `_vars_referenced_in_closures` scanned named subs as if they were closures, causing over-renaming.

All three fixed. `closure.t` now 50/50.

*(PERL_TEST_SUITE_PLAN.md §J)*

---

### `bop.t` hang  (unknown count — file skipped)

**What's broken:** `bop.t` (bitwise operators) hangs the SBCL process.
Root cause unknown.  The file tests bitwise `&`/`|`/`^`/`~` on both
integers and strings, plus `<<`/`>>` with very large shift counts.

**Hypothesis:** A very large shift count (e.g. `4 << 2147483648`) may
trigger an SBCL loop or timeout in the arithmetic tower.  Or string
bitwise ops (`&`/`|` on byte strings) may reach unimplemented code.

**Fix area:** Investigate by running `perl run-perl-test.pl perl-tests/bop.t`
with a timeout and capturing the last test that printed before the hang.

---

### `heredoc.t` hang  (unknown count — file skipped)

**What's broken:** `heredoc.t` hangs the SBCL process.  Root cause
unknown.  Likely an edge case in multi-line string interpolation or
indented heredocs (`<<~`).

**Fix area:** Investigate using `--lenient-ppi` + binary-search for the
hanging test.  Check `Pl/StringInterpolation.pm` for regex backtracking
on large strings.

---

### `use bytes` pragma  (~10 tests)

**What's broken:** Inside a `use bytes` scope, string operations work on
bytes not characters: `length("é")` returns 2 not 1, etc.

**Fix:** Add a dynamic variable `*use-bytes*`.  Guard `pl-length`,
`pl-substr`, `pl-index` to use byte counts when `*use-bytes*` is true.
CL's `sb-ext:string-to-octets` provides byte-level access.

**Fix area:** `cl/pcl-runtime.lisp`; `Pl/Parser.pm` `_process_use_statement`.

*(PERL_TEST_SUITE_PLAN.md §I / §1.8)*

---

### `local` on hash/array elements and slices

**What's broken:** `local $hash{key}`, `local @arr[1,2]`,
`local @hash{'a','b'}` — temporarily localize individual slots, restoring
them on scope exit.  Also `local *GLOB` to localize a whole symbol-table
entry.

```perl
local @arr[42];         # user's example
local $Config{key};     # common in Perl modules
local *STDOUT;          # redirect STDOUT temporarily
```

**Fix:** Save current value of each named slot before the scope and
restore on exit.  For element locals, needs a stack of (location, old-value)
pairs pushed/popped with `unwind-protect`.  For typeglob locals, snapshot
all five glob slots.

**Fix area:** `Pl/Parser.pm` `_process_local_statement`;
`cl/pcl-runtime.lisp` — new `pl-local-elem` and `pl-local-glob` macros.

*(Task #63 — deferred)*

---

### `caller()` improvements

**What's broken:** `pl-caller` returns only the package name.  Filename
and line number are always `0`.  `caller(N)` for N > 0 is unreliable.

**Fix:** At transpile time, PCL could embed `#.(line N)` reader macros in
generated CL to make line numbers available at runtime.  Alternatively, a
side-table mapping CL function names → source locations built during
transpilation.

*(Note: the "at FILE line N" suffix in error messages is deliberately not supported — see `docs/not-supported.md`. This item is specifically about `caller()` returning correct filename and line.)*

---

### `prototype()` function  (signatures.t, small)

**What's broken:** `prototype(\&foo)` should return the prototype string
for `&foo`, or `undef` if none.  Currently falls through to an undefined
function.

**Fix:** Return `*pl-undef*` for unknown/non-prototype subs; for subs
defined with signatures, return the appropriate prototype string.  A stub
returning `*pl-undef*` unconditionally would fix the common guard pattern.

*(PERL_TEST_SUITE_PLAN.md §L)*

---

## Tier 3 — Low value or high complexity

### Named inner sub closures  (uncommon)

**What's broken:** `sub outer { my $x = 1; sub inner { $x } }` — `inner`
is emitted as a global `pl-sub`, not a closure over `$x`.  The `__lex__`
renaming from session 63 only helps anonymous subs (lambdas).

**Fix:** Detect that a named inner sub references outer-scope lexical vars;
generate a closure stored in a package variable instead of a bare `pl-sub`.

*(PERL_TEST_SUITE_PLAN.md §K)*

---

### `flip-flop` operator `..` in scalar/boolean context  (flip.t — 3 tests)

**What's broken:** In list context `1..5` generates a range (works).  In
scalar (boolean) context `if ($. == 1 .. $. == 5)` is a stateful
flip-flop that toggles on when the LHS becomes true and off when the RHS
becomes true.  Completely different semantics.

**Fix:** `pl-flip-flop` macro with per-call-site state (a `defvar` counter
per source location).  Each `..` expression in the source needs a unique
state variable.

*(PERL_TEST_SUITE_PLAN.md §M "flip.t")*

---

### Regex named captures via `%+` and `%-`

**What's broken:** After a match with `(?<name>...)`, Perl populates `%+`
with the named captures.  PCL's `do-regex-match` sets `$1`, `$2`, ... but
does not populate `%+`.

**Fix:** After a successful match, build a hash from the
`:named-registers` return of `cl-ppcre:scan` and store it in `%+`.

**Fix area:** `cl/pcl-runtime.lisp` `do-regex-match`.

---

### ~~`s///r` non-destructive substitution~~  ✅ DONE (session 90)

Fixed in `do-regex-subst` in `cl/pcl-runtime.lisp`: `:r` modifier now
skips in-place update and returns `(make-p-box result)` instead of count.

---

### `qr//` objects as first-class values

**What's broken:** `qr/pattern/` produces a compiled regex object that can
be interpolated into other patterns (`/$qr/`) or passed as a value.  PCL
may not fully support all operations on stored `qr//` objects.

**Fix area:** `cl/pcl-runtime.lisp` `pl-qr`; verify interpolation into
`do-regex-match`.

---

### `DESTROY` method (object finalizers)

**What's broken:** Perl calls `DESTROY` when an object is garbage-collected
or when it goes out of scope.  PCL does not hook into SBCL's finalizer
system.

**Fix:** Register a `trivial-garbage:finalize` callback for each blessed
hash-table that has a `DESTROY` method.  Tricky because `DESTROY` must run
in the right package context.

---

### `concat2.t` — unknown failure  (3 tests)

**What's broken:** All 3 tests in `concat2.t` fail.  Root cause unknown.
May be a codegen edge case in string concatenation or repetition (`x=`).

**Fix area:** Run `perl run-perl-test.pl perl-tests/concat2.t` and inspect
the error.

*(PERL_TEST_SUITE_PLAN.md §M "concat2.t")*

---

## ~~Number formatting edge cases~~  ✅ DONE

### ~~Trailing decimal point in float-to-string~~ ✅ DONE

`stringify-value` in `cl/pcl-runtime.lisp` already does
`(string-right-trim "." (string-right-trim "0" s))` (line ~576).
Trailing `.` is stripped; `"1."` → `"1"`.

### ~~Inf / NaN string representations~~ ✅ DONE

`stringify-value` already handles this (lines ~565–567):
```lisp
#+sbcl ((sb-ext:float-infinity-p v) (if (plusp v) "Inf" "-Inf"))
#+sbcl ((sb-ext:float-nan-p v) "NaN")
```
Both fixes were present before this todo entry was written.

---

## Summary table

| Feature | Tests affected | Tier | Plan ref |
|---------|---------------|------|----------|
| Tie::Array/Tie::Hash loader hang | sort, reverse, local blocked | 1 | §A |
| Implicit returns / bare-if | widespread | 1 | §C |
| index.t / rindex | ~414 | 1 | §F |
| $SIG{__DIE__} handler | ~50 | 1 | §D/1.4 |
| ~~Inline package inside function~~ | ✅ DONE (session 72) | — | §N |
| ~~$Pkg::var forward decls~~ | ✅ DONE (session 90) | — | — |
| String eval | ~50 | 2 | Phase 3 |
| bop.t hang | unknown | 2 | §H |
| heredoc.t hang | unknown | 2 | §H |
| use bytes | ~10 | 2 | §I/1.8 |
| local on slices / *GLOB | local.t | 2 | Task #63 |
| caller() file/line | 1 | 2 | — |
| prototype() | small | 2 | §L |
| Named inner sub closures | small | 3 | §K |
| Flip-flop .. in scalar ctx | 3 | 3 | §M |
| Regex %+ named captures | small | 3 | — |
| ~~s///r non-destructive~~ | ✅ DONE (session 90) | — | — |
| qr// first-class objects | small | 3 | — |
| DESTROY finalizers | rare | 3 | — |
| concat2.t (overload + magic vars) | 3 | 3 | §M |
| ~~kvaslice.t repeated keys~~ | ✅ DONE (session 90) | — | — |
| ~~kvhslice.t / map fat-comma~~ | ✅ DONE (session 73) | — | — |
| ~~hashassign.t crash~~ | ✅ DONE (session 71) | — | §E |
| ~~Chained method calls~~ | ✅ DONE (session 70) | — | §G |
| ~~Foreach closure var capture~~ | ✅ DONE (session 70) | — | §J |
| ~~Trailing decimal / Inf/NaN fmt~~ | ✅ DONE | — | — |

---

## Known Warnings / Minor Bugs

### `indent_level` going negative — "Negative repeat count does nothing"

**Symptom:** During transpilation of some inputs, Perl prints:
```
Negative repeat count does nothing at Pl/Parser.pm line 3695.
Negative repeat count does nothing at Pl/ExprToCL.pm line 253.
```
Both lines are `"  " x $self->indent_level` (Parser) and
`$self->indent_str x $self->indent_level` (ExprToCL).

**Root cause:** `indent_level` is being decremented below zero somewhere in
the parser or code generator — a block-close decrements without a matching
open, or `indent_level` is not properly scoped/saved across recursive calls.

**Impact:** Cosmetic only — the warning fires but does not affect the generated
CL (the `x` operator silently produces `""` for negative counts).  Still, it
indicates an indent-tracking accounting error that should be fixed.

**Fix area:** Find every `$self->indent_level($self->indent_level - 1)` (or
equivalent decrement) in `Pl/Parser.pm` and `Pl/ExprToCL.pm` and ensure it
is guarded so `indent_level` never goes below 0.  Also audit calls to
`parse_block_to_cl_string` and similar recursive helpers that save/restore
indent state.
