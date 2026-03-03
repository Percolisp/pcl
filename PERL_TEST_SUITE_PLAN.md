# Plan: Pass the Perl Test Suite

## Current State

**5422 / 6316 tests passing (~86%)**
*(session 61 sweep, 2026-03-03, `--jobs 2 --timeout 60`, 98 files + 4 skipped)*

Note: `-j8` sweep gives artificially low counts (~2168) due to SBCL FASL race
conditions when 8 parallel processes share the cache. Always use `--jobs 2`
(or 1) for accurate counts.

PCL suite: **51 files, 2467 tests**, all passing.

---

## Phase 1: Gruntwork

### ~~1.1 `eval` as named unary~~ DONE

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

### ~~1.5 Missing functions~~ MOSTLY DONE

- ~~**`tie`/`untie`**~~ DONE (session 56) — 15/15 tie-01.t pass
- ~~**`each()` on arrays**~~ DONE (session 60) — returns `(index, value)` pairs
- ~~**`exists &sub`**~~ DONE (session ~54) — checks `fboundp` for CL symbol
- **`prototype()`** — NOT done. Return `undef` for unknown subs; signatures.t uses it as a guard.

### ~~1.6 Stack overflow~~ DONE (recurse.t: 28/28)

Root cause was NOT stack size — it was array argument flattening.
`pl-flatten-args` added to runtime; `Parser.pm` uses it for `@_` binding.

`sort.t` still times out due to `use Tie::Array` infinite loop (separate issue).

### ~~1.7 Typeglobs~~ DONE (auto.t: 42/47)

Implemented: `pl-glob` struct with `SCALAR`/`ARRAY`/`HASH`/`CODE`/`IO` slots,
`*foo` codegen, aliasing (`*x = \$y`, `*x = \@y`, `*x = \&f`), `*{expr}`.
auto.t: 42/47 — 5 remaining failures are arithmetic on glob copies (`$x++`/`$x--`
on a typeglob, very obscure). Core aliasing and filehandle use cases work.

### 1.8 `use bytes` pragma — ~10 tests (easy)

chr.t, concat.t. Inside a `use bytes` scope, string operations work on
bytes not characters. Implement as a dynamic variable `*use-bytes*` and
guard string functions that care.

### ~~1.9 `__DATA__` / `__END__`~~ DONE (session 58)

Parser.pm extracts embedded text and emits `(setf (gethash 'DATA *pl-filehandles*) ...)`.
`*pl-filehandles*` exported from `:pcl`.

### ~~1.10 Lexical filehandles~~ DONE (session 59)

`pl-open`, `pl-close`, `pl-eof` use `%pl-fh-arg` macro to handle both bareword
and `my $fh` forms. `pl-get-stream` checks for actual stream in box.

---

## Phase 2: Lexical Closures

### Status: MOSTLY DONE — one key issue remains

Basic `let`-wrapping for `my` vars is complete (session 46).
`state` variable infrastructure is done (session 61).

**Remaining: `defun` → `lambda` for anonymous subs**

Each `sub { ... }` expression currently generates:
```lisp
(defun --anon-block-N-- (&rest %_args) ...)
...
#'--anon-block-N--
```

`defun` creates a **global** function. All invocations of the enclosing
function share the same `--anon-block-N--` definition, so the generator
pattern is broken:

```perl
sub make_counter {
    return sub { state $n = 0; $n++ };  # all instances share one defun
}
my $c1 = make_counter();
my $c2 = make_counter();  # $c2 is the SAME function as $c1
```

**Fix:** Generate `(lambda (&rest %_args) ...)` directly instead of a named
`defun`. The lambda IS the value — no `#'name` indirection needed.

**Files to change:**
- `Pl/Parser.pm` `parse_block_as_function`: return a lambda form string
  instead of emitting a `defun` and returning a name
- `Pl/PExpr.pm` line ~1419: caller currently wraps the name in `#'name`;
  with lambda it uses the form directly
- The outer state `let` already wraps before the `defun` emit — with lambda
  it wraps the `lambda` form instead, which is correct

This also fixes the between-file name collision issue (session 46, pitfall #6).

### Previously discovered pitfalls (all fixed)

**1.** `next`/`last`/`redo`/labels are safe through `let` — CL's non-local
exits propagate through `let` without restriction.

**2.** `continue` blocks see `my` vars — `_process_bare_block` wraps both
main body and `continue` block inside the same `_with_declarations`.

**3.** `(block nil)` missing from `parse_block_as_function` — fixed.
`defun` creates `(block func-name)` not `(block nil)`.

**4.** if/while/for body blocks lacked `_with_declarations` — fixed.

**5.** `pl-scalar-=` poisoned lexical bindings via `(proclaim '(special $x))`
— fixed. `_emit` replaces assignments to let-bound vars with `(box-set ...)`.

**6.** `defun` for anonymous subs is global — NOT YET FIXED. See above.

### `state` variables (session 61)

Infrastructure done:
- Named subs: `$state__subname__varname__N` unique names, outer `let` wraps `pl-sub`
- Anonymous subs: same, with `$state__anon__varname__N`
- Package-level `state`: routed to `_process_my_toplevel_declaration` (same as `my`)
- Forward-decl scanner: `__` separators prevent false `$state` defvar

Still broken: generator pattern (multiple instances sharing one defun) —
same root cause as the `defun` → `lambda` issue above.

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
- Parts of cmpchain.t
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

## Summary

| Item | Status | Tests gained |
|------|--------|-------------|
| 1.1 eval named-unary | ✅ DONE | ~243 |
| 1.2 PPI fallback | ❌ TODO | ~490 |
| 1.3 sprintf positional | ✅ DONE | ~47 |
| 1.4 $SIG handlers | ❌ TODO | ~50 |
| 1.5 Missing functions | ✅ MOSTLY DONE | ~60 |
| 1.6 Stack size (array flatten) | ✅ DONE | recurse.t 28/28 |
| 1.7 Typeglobs | ✅ DONE | ~30 |
| 1.8 use bytes | ❌ TODO | ~10 |
| 1.9 __DATA__/__END__ | ✅ DONE | ~5 |
| 1.10 Lexical filehandles | ✅ DONE | ~10 |
| 2. anon sub → lambda | ❌ TODO | ~100+ |
| 2. state var infrastructure | ✅ DONE | partial |
| 3. String eval | ❌ TODO | ~50+ |

**Remaining high-value items:**
1. `defun` → `lambda` for anonymous subs (fixes closures + state generators)
2. PPI parse fallback (easy win, ~490 tests)
3. $SIG handlers (warn.t, die.t)
4. String eval (Phase 3)

**Projected final: ~98%**
