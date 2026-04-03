# PCL: Features Left To Implement

This document lists Perl features that are **planned for implementation**
but not yet done.  Unlike `docs/not-supported.md` (design decisions), every
item here is a real bug or missing feature worth fixing.

Items are grouped by theme and roughly ordered by number of tests affected.
Cross-references to `PERL_TEST_SUITE_PLAN.md` sections are in parentheses.

---

## Tier 1 — High value (many tests, clear root cause)

### ~~Tie::Array / Tie::Hash module loader hang~~ ✅ RESOLVED (session 103)

`require Tie::Array` and `require Tie::Hash` now load cleanly.  The hang
that blocked `sort.t`, `reverse.t`, and `local.t` is gone.  No code change
was needed — the hang appears to have been resolved by earlier fixes.

*(Note: `kvhslice.t`, `kvaslice.t` were previously listed here — both now fully passing.)*

---

### ~~Implicit returns / bare-`if` return value~~  ✅ DONE (session 102)

**Files:** `perl-tests/do.t` tests 9-10 fixed. 20-test regression suite in `Pl/t/bareif-01.t`.

**What was done:** Six new methods in `Pl/Parser.pm`:
- `_fresh_ret_var` — counter-based CL symbol name `--pcl-if-ret--N`
- `_is_if_without_else` — detects compound if/unless without final else
- `_is_postfix_if_without_else` — detects postfix `EXPR if/unless C`
- `_generate_if_tail_clauses` — like `_generate_if_clauses` but wraps condition in `(setf ret_var COND)` and recurses into branch bodies via `_process_block_in_tail_context`
- `_process_if_tail` — thin wrapper: collect clauses, call `_generate_if_tail_clauses`
- `_process_block_in_tail_context` — like `_process_block` but dispatches last significant stmt to `_process_tail_stmt`
- `_process_tail_stmt` — handles one tail statement: if-without-else recurses, postfix if/unless emits `p-if/p-unless (setf ...)`, simple expr emits `(setf ret_var cl)`

`_process_block` pre-scans `schildren`; if last is a bare if or postfix if/unless and `in_subroutine > 0`, opens `(let ((--pcl-if-ret--N nil)) ...)` and returns `--pcl-if-ret--N` after the if form.

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

### ~~`index.t` / `rindex` failures~~  Partially resolved (session 101)

**Status:** 87/12 (was 230/162).

**Fixed:** `p-rindex` empty-substr + negative-position bug — `rindex("abc","", -1)`
returned -1; fix: check empty-substr before negative-pos, clamp to max(0, min(pos, len)).

**Commented out:** 293 tests using `eval "string"` (OPpTARGET_MY bytecode-optimizer
tests, large-codepoint tests, lvalue tests). Plan: 413 → 120.

**Remaining 12 failures:**
- Tests 49–58 (10): `utf8::encode` / octet-level string handling — not implemented
- Test 90: overloaded `""` stringification in `p-index` / `to-string`
- Test 98: `require Tie::Scalar` module loader

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

### `bop.t` crash at test 33  (blocked — session 96)

**What's broken:** `bop.t` does NOT hang — it crashes at test 33 with
`invalid number of arguments: 1` to `pl-is`.

**Root cause:** `sub _and($) { ... }` has prototype `($)` (one scalar arg).
PCL does not track user-defined sub prototypes, so `is _and 0, '0', 'str'`
is transpiled as `(pl-is (pl-_and 0 "0" "str"))` — all args go to `_and`,
leaving `is` with only the return value.

**Other issues in bop.t (even if prototype fixed):**
- String bitwise ops `"AAAAA" & "zzzzz"` return `0` instead of character-by-character AND result (tests 21-23, 24-26)
- `use integer` arithmetic-shift fill for large negative shifts (tests 13-14)
- `tie`/double-magic variable tests (requires `tie` support)
- `native_to_uni()` from `charset_tools.pl`

**Fix area:** User sub prototype tracking in `Pl/Parser.pm` / `Pl/PExpr/Config.pm`
(record `($)` etc. at sub definition time, limit args at call site).
String bitwise ops need character-by-character implementation in `p-band`/`p-bor`/`p-bxor`.

---

### `heredoc.t` — mostly untestable (session 96)

**What's broken:** `heredoc.t` does NOT hang — it crashes cleanly because
137/138 tests use `fresh_perl_is` / `fresh_perl_like` which are no-ops in
PCL's `test.pl` stub (they spawn a fresh Perl subprocess).  Only test 1
produces TAP output (and it fails because it uses string `eval`).

**Root cause:** `fresh_perl_is` = `sub { return; }` in PCL's test.pl.  No TAP
output for those tests → "planned 138, ran 1".

**Fix area:** `fresh_perl_is` could spawn an actual Perl subprocess and compare
its output.  But test 1 (the only non-subprocess test) needs string eval anyway.
This file is essentially blocked on: (1) `fresh_perl_is` subprocess support,
(2) string eval, (3) indented heredoc (`<<~`) parsing.

**Verdict:** Not worth pursuing for v1.  See `docs/not-supported.md`
for rationale on `fresh_perl_*` subprocess tests.

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

### ~~`caller()` improvements~~  ✗ NOT SUPPORTED (moved to `docs/not-supported.md`)

`caller()` filename and line are always `0`; this is deliberate — see
`docs/not-supported.md` for rationale.  `caller.t` also requires string eval
and `%::` stash manipulation.

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

### ~~Regex named captures via `%+`~~  ✅ DONE (session 91)

`%+` now fully implemented: `cl-ppcre:*allow-named-registers*` enabled,
`%+` declared and exported, cleared on every match attempt (including
failures), populated from `create-scanner`'s `reg-names` list in all
match paths (`do-regex-match` and `do-regex-subst` including s///e).
`$+{name}` in string interpolation generates `(p-gethash %+ "name")`.
Optional non-matching groups set `$N` to nil (was crashing).
10 runtime tests in `Pl/t/named-capture-01.t`.

---

### ~~`state` variables~~  ✅ DONE (session 94)

**File:** `perl-tests/state.t` — now 23/0 fully passing.

Six bugs fixed (see `docs/v1-implementation-plan.md` B6 for full details):
- `%p-flatten-list`: `(listp nil)` = T in CL swallowed undef return values as empty lists → changed to `(consp item)`
- `p-post++`: returned nil for nil box; Perl's `undef++` returns 0 → `old = (if (null val) 0 val)`
- `state ($t) //= 3`: list form and `//=` operator now handled in `_process_state_declaration`
- Nested state vars in bare blocks: `_find_all_declarations` now recurses into bare `PPI::Structure::Block` (excludes anon sub bodies via `sprevious_sibling` check)
- Initial binding: `$` → `(make-p-box nil)`, `@` → empty array, `%` → empty hash (not nil)
- Anon sub rename merge: `{%$existing, %state_renames}` instead of replacing

New test: `Pl/t/state-01.t` (20 tests, all passing).

---

### ~~`sort NAME LIST` named comparator form~~  ✅ DONE (session 93)

**What was broken:** `sort compare @arr` generated `(p-sort (pl-compare ...))` — the
comparator was called as a bare function, not passed as a lambda for `p-sort` to
call per-pair.  Also, `$a`/`$b` were not declared as CL special variables, so named
sort subs couldn't see the dynamically-bound values.

**Fix:** `Pl/PExpr.pm` detects `sort WORD LIST` and wraps the comparator in an
`inline_lambda` node; `Pl/ExprToCL.pm` generates `(lambda ($a $b) (pl-NAME))`.
`Pl/Parser.pm` `_insert_variable_forward_declarations` unconditionally emits
`(defvar $a ...)` / `(defvar $b ...)` before computing `@undeclared`.

**Result:** sort.t 31/29 → 33/27. New test: `Pl/t/sort-01.t` (16 tests).

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
| ~~Tie::Array/Tie::Hash loader hang~~ | ✅ RESOLVED (session 103) | — | §A |
| ~~Implicit returns / bare-if~~ | ✅ DONE (session 102) | — | §C |
| ~~index.t / rindex~~ | ~~87/12~~ (session 101) | — | §F |
| $SIG{__DIE__} handler | ~50 | 1 | §D/1.4 |
| ~~Inline package inside function~~ | ✅ DONE (session 72) | — | §N |
| ~~$Pkg::var forward decls~~ | ✅ DONE (session 90) | — | — |
| String eval | ~50 | 2 | Phase 3 |
| bop.t crash (prototype + string bitwise) | 32 pass before crash | 2 | §H |
| heredoc.t (fresh_perl_is no-ops) | 0/138 useful tests | — | not-supported |
| use bytes | ~10 | 2 | §I/1.8 |
| local on slices / *GLOB | local.t | 2 | Task #63 |
| ~~caller() file/line~~ | ✗ not-supported | — | — |
| prototype() | small | 2 | §L |
| Named inner sub closures | small | 3 | §K |
| Flip-flop .. in scalar ctx | 3 | 3 | §M |
| ~~Regex %+ named captures~~ | ✅ DONE (session 91) | — | — |
| ~~sort NAME LIST named comparator~~ | ✅ DONE (session 93) | — | B5 |
| ~~state variables~~ | ✅ DONE (session 94) | — | B6 |
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

## Codegen Cleanup

### Re-introduce `p-eval-direct` macro for compile-time forms

**What's wrong:** Session 114 removed the `p-eval-direct` macro (which was
`(eval-when (:compile-toplevel :load-toplevel :execute) ...)`) and inlined
the full `eval-when` stanza at every emit site in `Pl/Parser.pm` (12 sites).
The generated CL now repeats:

```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $x (make-p-box nil)))
```

everywhere, which is verbose and noisy in intermediate code.

**Fix:** Restore `p-eval-direct` (or a better name like `pcl-at-compile`) to
`cl/pcl-runtime.lisp`, re-add it to the `:pcl` export list, and revert the
12 Parser.pm emit sites to use the macro again.  The generated CL is an
intermediate representation — named macros are preferable to repeated verbose
stanzas.  The original removal was correct in spirit (p-my/p-our were truly
dead) but went too far by also inlining `eval-when`.

**Fix area:** `cl/pcl-runtime.lisp` (add macro + export); `Pl/Parser.pm`
(revert 12 `_emit("(eval-when ...")` calls back to `_emit("(p-eval-direct")`).

---

## Known Warnings / Minor Bugs

### ~~SBCL style-warnings on load~~  ✅ FIXED (session 93)

Two forward-reference warnings were emitted on every SBCL load:

1. `cl/pcl-runtime.lisp`: `p-aref-deref` called `p-aslice` before its `defun`.
   Fixed by adding `(declaim (ftype function p-aslice))` before `p-aref-deref`.
2. `cl/pcl-test.lisp`: `pl-diag` and `pl-note` called `split-string` before it
   was defined. Fixed by moving `split-string` before those two functions.

---

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
