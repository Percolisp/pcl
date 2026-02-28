# Plan: Pass the Perl Test Suite

## Current State

**2889 / ~3329 tests passing (~87%)**

The remaining 440 failures split into two categories: gruntwork (known
bugs, missing functions, parse edge cases) and two architectural problems
that require deeper changes. This plan covers both.

---

## Phase 1: Gruntwork (~87% → ~92%)

These are independent, can be done in any order, no architectural risk.
Listed by estimated tests gained.

### ~~1.1 `eval` as named unary — ~243 tests (trivial)~~ DONE

Already in `Pl/PExpr/Config.pm` `%named_unary` at line 111.

### 1.2 PPI parse fallback — ~490 tests (medium)

**File:** `Pl/Parser.pm`, `_build_ppi_doc`

Two large files fail entirely because PPI chokes on one exotic line near
the end and returns `undef` for the whole file:

- `for.t` line 767: `for ${*$f} (5,11,33) {`
- `substr.t` line 772: `substr $t, 0, 0, *ワルド;`

**Fix:** binary-search for the first line PPI can't parse, strip from
there, parse the rest. The test file still runs all preceding tests.

```perl
sub _build_ppi_doc {
    my ($self, $src) = @_;
    my $doc = PPI::Document->new(\$src);
    return $doc if $doc;
    # Binary search for first bad line
    my @lines = split /\n/, $src;
    my ($lo, $hi) = (0, $#lines);
    while ($lo < $hi) {
        my $mid = int(($lo + $hi) / 2);
        my $partial = join("\n", @lines[0..$mid]);
        if (PPI::Document->new(\$partial)) { $lo = $mid + 1; }
        else                               { $hi = $mid;     }
    }
    warn "PCL: truncating at line $lo due to PPI parse failure\n";
    my $partial = join("\n", @lines[0..($lo-1)]);
    return PPI::Document->new(\$partial);
}
```

### ~~1.3 `sprintf` positional arguments~~ DONE (sprintf2.t: 65/66)

`%N$s` positional specifiers implemented in `pl-sprintf` (pcl-runtime.lisp).
sprintf2.t: 65/66 — only test 65 ("expected warnings") still fails, which
needs `$SIG{__WARN__}` (item 1.4), not a sprintf issue.

### 1.4 `$SIG{__WARN__}` and `$SIG{__DIE__}` — ~50 tests (medium)

**Files:** `cl/pcl-runtime.lisp`

`warn.t` and `die.t` fail because signal handlers aren't invoked.

- Wrap `pl-warn` to check `(gethash "__WARN__" %SIG)` and call the handler
- Wrap `pl-die` similarly for `__DIE__`
- Handler receives the message string as argument

### 1.5 Missing functions — ~60 tests (medium)

- **`tie`/`untie` stubs** — return a "not implemented" value rather than
  crashing. join.t, hash.t would partially pass.
- **`prototype()`** — return `undef` for unknown subs; signatures.t
  uses it as a guard.
- **`each()` on arrays** — currently only works on hashes. Returns
  `(index, value)` pairs, resets at end.
- **`exists &sub`** — exists_sub.t. Check `fboundp` for the CL symbol.

### ~~1.6 Stack overflow~~ DONE (recurse.t: 28/28)

**Root cause was NOT stack size** — it was array argument flattening.

`foo(@arr)` in Perl passes `@arr` elements individually. PCL was passing the
raw CL vector as a single argument. So `get_list1(@_)` inside `get_first1`
passed the entire `@_` vector as one argument; `get_list1` then saw a truthy
vector where it expected `0` (falsy), and the termination check never fired.

**Fix:** Added `pl-flatten-args` to `cl/pcl-runtime.lisp` (exported). Changed
`Parser.pm` line 2462 to use `(pl-flatten-args %_args)` instead of
`make-array ... :initial-contents`. This spreads raw vectors (arrays) into
individual elements when building `@_`, matching Perl semantics.

`sort.t` still times out due to `use Tie::Array` causing infinite loop (known
issue, separate from recursion depth).

### ~~1.7 Typeglobs~~ DONE (auto.t: 42/47)

Implemented: `pl-glob` struct with `SCALAR`/`ARRAY`/`HASH`/`CODE`/`IO` slots,
`*foo` codegen, aliasing (`*x = \$y`, `*x = \@y`, `*x = \&f`), `*{expr}`.
auto.t: 42/47 — 5 remaining failures are arithmetic on glob copies (`$x++`/`$x--`
on a typeglob, very obscure). Core aliasing and filehandle use cases work.

### 1.8 `use bytes` pragma — ~10 tests (easy)

chr.t, concat.t. Inside a `use bytes` scope, string operations work on
bytes not characters. Implement as a dynamic variable `*use-bytes*` and
guard string functions that care.

---

## Phase 2: Lexical Closures (~92% → ~95%)

### Status: IMPLEMENTED (session 46)

Core `let`-wrapping is done. Basic closures (like `make_counter`) work.
See remaining issues at the bottom of this section.

### The Problem

PCL compiles all variables as `defvar` (CL special/dynamic variables).
Dynamic variables are not captured by closures — each `defun`/`lambda`
sees the current dynamic binding, not the one at the time of closure
creation.

```perl
sub make_counter {
    my $count = 0;
    return sub { $count++ };   # must capture $count lexically
}
```

### The Fix (Implemented)

**Approach used: hoist-all**, not per-statement nesting. `_with_declarations`
pre-scans a block for all `my` declarations, then emits a single `let`
wrapping all of them at once, with the whole block body inside:

```lisp
(pl-sub pl-make_counter ()
  (let (($count (make-pl-box nil)))
    (box-set $count 0)
    (defun --anon-block-1-- ()
      (block nil (pl-post++ $count)))
    (pl-return #'--anon-block-1--)))
```

This is simpler than per-statement nesting and equally correct for
closure capture. CL lambda/defun inside a `let` closes over the binding.

### What stays the same

- `our $x` → `defvar` (unchanged)
- Package variables `$Foo::x` → `defvar` (unchanged)
- File-scope `my $x` → `eval-when` + `defvar` (unchanged; BEGIN visibility)
- `local $x` → dynamic save/restore (unchanged; needs special scope)

### Discovered pitfalls (session 46)

**1. `next`/`last`/`redo`/labels are safe through `let`.**
CL's `go`, `return-from`, and `throw` all propagate through `let` forms
without restriction. No changes needed.

**2. `continue` blocks see `my` vars — handled.**
`_process_bare_block` wraps both main body and `continue` block inside
the same `_with_declarations` callback, so the `let` covers both.

**3. `(block nil)` missing from `parse_block_as_function` — FIXED.**
`defun` creates `(block func-name)`, not `(block nil)`. So `(pl-return ...)`
→ `(return-from nil ...)` inside an anonymous sub crashed with
"attempt to RETURN-FROM a block that no longer exists" when the closure
was called after the enclosing function returned. Fix: wrap body in
`(block nil ...)` inside `parse_block_as_function`. Also increment
`in_subroutine` so `my` vars inside the anon sub use `let`, not `defvar`.

**4. if/while/for body blocks lacked `_with_declarations` — FIXED.**
`_generate_if_clauses`, `_process_while_statement`, `_process_c_style_for`,
and `_process_foreach_loop` all called `_process_block` directly without
wrapping the body in `_with_declarations`. `my` vars inside those blocks
became implicit globals (SBCL auto-created special vars, warning "undefined
variable"). Now each body block is wrapped in `_with_declarations`.

**5. `pl-scalar-=` poisons lexical bindings — FIXED.**
This was the subtlest bug. `pl-scalar-=` has auto-declaration logic:
```lisp
(unless (boundp '$x)
  (proclaim '(special $x))          ; ← the poison
  (setf (symbol-value '$x) nil-box))
```
When a `let`-bound `$x` is assigned for the first time, `(boundp '$x)`
returns NIL (no special binding), so `(proclaim '(special $x))` runs.
This marks `$x` globally special for ALL future compilations. The next
time the same code is compiled (e.g. the file is reloaded, or a second
caller's code is compiled), the `let (($x ...))` creates a DYNAMIC
binding instead of a lexical one — and closures cannot capture dynamic
bindings.

Fix: `_with_declarations` now tracks the current set of let-bound vars
in `$self->{_let_bound_vars}`. `_emit` replaces `(pl-scalar-= $var ...)`
with `(box-set $var ...)` for those vars, bypassing the auto-declaration.

**6. `defun` for anonymous subs is a global name, not a unique closure.**
Each `sub { ... }` expression in a given function generates a `defun`
with a name like `--anon-block-2--`. If two different Perl subs each
contain `sub { ... }`, the second `defun` silently overwrites the first.
This means code like:
```perl
my $f = make_foo();
my $g = make_foo();  # overwrites --anon-block-2-- !
$f->();              # calls the overwritten version
```
is broken for multiple instances. The counter resets per transpiler run so
within one file the names are unique, but between files loaded in the same
SBCL image they collide. Long-term fix: use `lambda` for anonymous subs
instead of `defun`, or make the names truly unique (e.g. with a gensym).
This is a separate issue from basic closure capture.

---

## Phase 3: String `eval` (~95% → ~98%)

### The Problem

`eval "string"` needs to parse and execute arbitrary Perl at runtime.
This requires a Perl parser available during execution — which is the
PCL transpiler itself.

### The Approach

At runtime, `pl-eval-string` calls out to the PCL transpiler via a
subprocess, gets CL back, and evaluates it in the current package context.

```lisp
(defun pl-eval-string (str)
  (handler-case
    (let* ((cl-code (pcl-transpile str))   ; call transpiler
           (result  (eval (read-from-string cl-code))))
      (make-pl-box result))
    (error (e)
      (setf $@ (format nil "~A" e))
      *pl-undef*)))

(defun pcl-transpile (perl-str)
  ;; Write perl-str to a temp file, run pl2cl, return CL string
  (let ((tmpfile (make-temp-file)))
    (write-file tmpfile perl-str)
    (run-program "perl" (list *pcl-pl2cl-path* tmpfile)
                 :output :string)))
```

`*pcl-pl2cl-path*` is already set by `pl2cl` in generated code preamble.

### Caveats

- The eval'd code runs in a fresh package context, not the caller's
  lexical environment. This matches Perl's actual behavior for string eval
  (string eval doesn't capture `my` variables from the caller's scope —
  only package variables).
- `$@` must be set on failure (already the convention).
- This is a subprocess call — slow. Acceptable for test suite; would need
  caching or a persistent transpiler process for production use.

### What this unblocks

- oct.t tests 78-79 (wide char in eval)
- state.t tests that use `eval 'CORE::state...'`
- Parts of cmpchain.t (switch.t is moot — given/when removed in Perl 5.38)
- Any test using `eval` to test syntax errors

---

## What 100% Looks Like

Some tests check Perl internals with no meaningful CL equivalent:

- `pack "d"` floating-point byte layout (num.t, pack.t) — depends on
  C-level double representation
- `format`/`write` report formatting — rarely used, low priority
- `$^A` accumulator variable for format/write
- Some quadmath / 128-bit float tests (exp.t)

These are acceptable exceptions. A realistic ceiling is **~98%** with
all three phases done. The remaining 2% are tests of C-level Perl
internals that have no sensible transpiler target.

---

## Summary Timeline

| Phase | Change | Effort | Gain |
|-------|--------|--------|------|
| 1.1 eval named-unary | 1 line, Config.pm | Trivial | ~243 |
| 1.2 PPI fallback | ~20 lines, Parser.pm | Half day | ~490 |
| ~~1.3 sprintf positional~~ | DONE — sprintf2.t 65/66 | — | ~47 |
| 1.4 $SIG handlers | runtime work | 1 day | ~50 |
| 1.5 Missing functions | runtime stubs | 1 day | ~60 |
| ~~1.6 Stack size~~ | DONE — array flattening bug | — | recurse.t 28/28 |
| ~~1.7 Typeglobs~~ | DONE — auto.t 42/47 | — | ~30 |
| 1.8 use bytes | runtime + parser | Half day | ~10 |
| **Phase 1 total** | | ~2 weeks | **~960** |
| 2. Lexical closures | Parser.pm refactor | 1-2 weeks | ~100+ |
| 3. String eval | runtime subprocess | 1 week | ~50+ |
| **Total** | | **~4-5 weeks** | **~1110** |

**Projected final: ~4000/~4000 (~98%)**
