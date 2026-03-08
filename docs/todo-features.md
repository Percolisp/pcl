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

**Blocked files:** `sort.t`, `reverse.t`, `local.t`, `kvaslice.t`,
`kvhslice.t`.

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

### `hashassign.t` mass failures  (~280 tests)

**What's broken:** Crash at test 33 ("direct list assignment to hash")
with `SIMPLE-PROGRAM-ERROR: invalid number of arguments: 1`.  Tests 1–32
now pass (session 68 fixed the `\@arr` boxing).  The remaining failure
appears to be a function called with the wrong arity — needs investigation
on what test 33 does.

**Fix area:** `cl/pcl-runtime.lisp` hash assignment path; likely
`pl-list-=` or hash-construction from a flat list.

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

### Inline `package Pkg {}` inside a subroutine body  (index.t, substr.t, ~unknown)

**What's broken:** When an inline package block appears inside a function
body, PCL's hoisting logic misplaces code that follows it:

```perl
sub run_tests {
    {
        package MyTie { sub STORE { ... } };
        my $x;          # ← ends up OUTSIDE run_tests() in generated CL
        tie $x, "MyTie";
    }
}
```

**Fix area:** `Pl/Parser.pm` `_process_package_block` — when
`in_subroutine > 0`, inline packages must be emitted in-place, not
hoisted.

*(PERL_TEST_SUITE_PLAN.md §N)*

---

### Package-qualified variable declarations (`$Pkg::var`)  (for.t, others)

**What's broken:** When `$Dog::VERSION` is used without a prior
`package Dog;` block, PCL emits `(defpackage :Dog ...)` but the
`(defvar $VERSION ...)` runs in the wrong package, leaving
`DOG::$VERSION` unbound at runtime.

**Fix:** When generating access to `Pkg::$var`, also emit a
`(defvar Pkg::$var (make-pl-box nil))` in the preamble bucket (guarded
so it doesn't clobber existing values), or do a first-pass scan for all
`$Pkg::var` references and forward-declare them.

**Fix area:** `Pl/ExprToCL.pm` `gen_leaf` / `Pl/Parser.pm` preamble bucket.

*(IMPROVEMENT_PLAN.md "Package-qualified variable declarations")*

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

### Chained method calls: `$obj->m1()->m2()`  (~30–50 tests)

**What's broken:** The parser emits a PARSE ERROR for the second `->` when
the LHS is a method call result rather than a plain variable.  Example:
`B->new()->name()` fails.

**Fix area:** `Pl/PExpr.pm` — allow postfix `->` after any complete
expression node, not only after `Symbol` / subscript tokens.

*(PERL_TEST_SUITE_PLAN.md §G)*

---

### Foreach loop variable capture in closures  (8 tests)

**What's broken:** `for my $n (0..4) { $foo[$n] = sub { $n } }` — all
closures share the final value of `$n` because `pl-foreach` uses a single
mutated binding.

**Fix:** `pl-foreach` macro wraps each iteration body in a fresh `let`
that copies the loop variable, giving each closure its own independent
binding.

**Fix area:** `cl/pcl-runtime.lisp` `pl-foreach` macro.

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

### `s///r` non-destructive substitution

**What's broken:** `my $new = $str =~ s/foo/bar/r` returns the modified
copy without changing `$str`.  Whether PCL supports `/r` needs
verification.

**Fix area:** `Pl/ExprToCL.pm` regex codegen; `cl/pcl-runtime.lisp`
`pl-s-replace`.

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

## Number formatting edge cases  (exp.t, infnan.t, num.t, negate.t)

### Trailing decimal point in float-to-string

**What's broken:** SBCL's `format nil "~F"` sometimes produces `"1."` or
`"0."` instead of `"1"` or `"0"`.  Perl never produces trailing decimal
points.

**Fix:** Post-process `to-string` output: strip a trailing `.` if present
and no fractional part follows.

### Inf / NaN string representations

**What's broken:** Perl stringifies IEEE infinities as `"Inf"` / `"-Inf"`
and NaN as `"NaN"`.  SBCL prints `"infinity"` / `"NaN"`.

**Fix:** In `to-string`, detect SBCL's `float-infinity-p` /
`float-nan-p` and return Perl-compatible strings.

**Fix area:** `cl/pcl-runtime.lisp` `to-string` / `box-sv`.

---

## Summary table

| Feature | Tests affected | Tier | Plan ref |
|---------|---------------|------|----------|
| Tie::Array/Tie::Hash loader hang | ~200+ | 1 | §A |
| Implicit returns / bare-if | widespread | 1 | §C |
| hashassign.t crash (test 33+) | ~280 | 1 | §E |
| index.t / rindex | ~414 | 1 | §F |
| $SIG{__DIE__} handler | ~50 | 1 | §D/1.4 |
| Inline package inside function | index.t, substr.t | 1 | §N |
| $Pkg::var forward decls | for.t, others | 1 | — |
| String eval | ~50 | 2 | Phase 3 |
| Chained method calls | ~30–50 | 2 | §G |
| Foreach closure var capture | 8 | 2 | §J |
| bop.t hang | unknown | 2 | §H |
| heredoc.t hang | unknown | 2 | §H |
| use bytes | ~10 | 2 | §I/1.8 |
| local on slices / *GLOB | local.t | 2 | Task #63 |
| caller() file/line | 1 | 2 | — |
| prototype() | small | 2 | §L |
| Named inner sub closures | small | 3 | §K |
| Flip-flop .. in scalar ctx | 3 | 3 | §M |
| Regex %+ named captures | small | 3 | — |
| s///r non-destructive | small | 3 | — |
| qr// first-class objects | small | 3 | — |
| DESTROY finalizers | rare | 3 | — |
| concat2.t root cause | 3 | 3 | §M |
| Trailing decimal / Inf/NaN fmt | ~30 | 2 | — |
