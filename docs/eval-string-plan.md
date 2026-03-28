# PCL: `eval "string"` Implementation Plan

**Written:** 2026-03-28

---

## What already works

- `eval { block }` → `p-eval-block` macro (fully working)
- `eval "string"` → already generates `(p-eval ARG)` in `ExprToCL.pm` (no codegen change needed)
- `p-transpile-file` already does the subprocess pattern we need (see `cl/pcl-runtime.lisp` line ~5109)
- `*pcl-pl2cl-path*` is set in the preamble and available at runtime

---

## Approach: subprocess transpilation

Pipe Perl code to `perl pl2cl --eval-pkg PKGNAME` via stdin, get CL back, read and `eval` it.
Same pattern as module loading via `p-transpile-file`. No in-process transpilation needed.

### Why `--eval-pkg`?

When `p-eval` runs at runtime, `*package*` is (say) `:|Foo::Bar|`. The eval'd Perl code
must execute in that same package. The pl2cl subprocess doesn't know this without being told.

- **Normal stdin mode** injects the full preamble: `(p-defpackage :main)(in-package :main)` — wrong package, and redundant (@INC etc. already live in the running SBCL)
- **`--module` mode** emits `(in-package :pcl)` and stays there — also wrong

`--eval-pkg Foo::Bar` passes the current package to the subprocess. It emits a minimal
preamble — just the package switch — instead of the full startup preamble.

---

## Files to change

| File | Change |
|------|--------|
| `pl2cl` | +`$eval_pkg` option, +`build_eval_preamble`, modify stdin branch |
| `cl/pcl-runtime.lisp` | +`*p-eval-string-cache*`, +`p-transpile-string`, replace `p-eval` stub |
| `Pl/t/eval-01.t` | +8 runtime tests |

No changes to `Pl/ExprToCL.pm`, `Pl/Parser.pm`, or `Pl/PExpr.pm`.

---

## Step 1: `pl2cl` — add `--eval-pkg PKGNAME` mode

### 1a. New option variable (after `my $lenient_ppi`)

```perl
my $eval_pkg;   # When set, emit minimal preamble for eval-string mode
```

### 1b. Register in GetOptions

```perl
'eval-pkg=s' => \$eval_pkg,
```

### 1c. New `build_eval_preamble` sub (after `build_preamble`)

```perl
sub build_eval_preamble {
  my ($pkg_name) = @_;
  # Always use :|PKGNAME| pipe-quoting — works for both MAIN and Foo::Bar
  return "(p-defpackage :|$pkg_name|)\n(in-package :|$pkg_name|)";
}
```

### 1d. Modified stdin branch

Replace the `elsif (!-t STDIN)` block:

```perl
elsif (!-t STDIN) {
  local $/;
  $code = <STDIN>;
  my $output = Pl::Parser->parse_code($code);

  if (defined $eval_pkg) {
    # eval-string mode: REPLACE (in-package :pcl) with just the package switch.
    # Everything else (@INC, *pcl-pl2cl-path*) is already live in the caller.
    my $preamble = build_eval_preamble($eval_pkg);
    $output =~ s/\(in-package :pcl\)/$preamble/;
  } else {
    # Normal stdin mode: inject full preamble after (in-package :pcl)
    my $preamble = build_preamble(undef);
    $output =~ s/(\(in-package :pcl\))/$1\n$preamble/;
  }
  print $output;
}
```

**Why REPLACE rather than append:** In eval-string mode the code must start in the caller's
package, not `:pcl`. The normal mode keeps `(in-package :pcl)` in place; eval mode drops it.

---

## Step 2: `cl/pcl-runtime.lisp` — three additions

### 2a. Cache variable (near line 261, in forward-declarations section)

```lisp
(defvar *p-eval-string-cache* (make-hash-table :test 'equal)
  "Cache for p-eval: maps (cons perl-code pkg-name) -> cl-text.
   Avoids re-spawning pl2cl for repeated identical eval calls.")
```

Uses `equal` test because keys are cons cells of two strings.

### 2b. `p-transpile-string` (insert after `p-transpile-file`, around line 5131)

```lisp
(defun p-transpile-string (perl-code pkg-name)
  "Transpile a Perl string to CL code via pl2cl --eval-pkg.
   Returns the CL text string, or signals an error on failure."
  (unless *pcl-pl2cl-path*
    (error "pl2cl path not set - cannot transpile eval string"))
  (let* ((in-stream (make-string-input-stream perl-code))
         (out-buf   (make-array 0 :element-type 'character
                                  :adjustable t :fill-pointer 0))
         (err-buf   (make-array 0 :element-type 'character
                                  :adjustable t :fill-pointer 0))
         exit-code)
    (with-output-to-string (out-s out-buf)
      (with-output-to-string (err-s err-buf)
        (let ((proc (sb-ext:run-program
                     "perl"
                     (list (namestring *pcl-pl2cl-path*)
                           "--eval-pkg"
                           pkg-name)
                     :input in-stream
                     :output out-s
                     :error err-s
                     :wait t
                     :search t)))
          (setf exit-code (sb-ext:process-exit-code proc)))))
    (unless (zerop exit-code)
      (error "pl2cl --eval-pkg failed: ~A" err-buf))
    out-buf))
```

### 2c. Replace the `p-eval` stub (lines 3995–4010)

```lisp
;;; p-eval: Perl eval(STRING) — full string eval via runtime transpilation.
;;;
;;; Design notes:
;;; - (let ((*package* *package*)) (eval ...)) protects the caller's package.
;;;   The (in-package ...) inside eval'd code changes *package* only within
;;;   that let-frame; on exit it is restored. Perl eval "package Foo" likewise
;;;   does NOT persist after the eval returns.
;;; - eval runs in CL's null lexical environment — correct, since Perl
;;;   eval "string" also cannot see the caller's 'my' variables.
;;; - Package variables (defvar) ARE visible — correct Perl behavior.
;;; - $@ format: we omit the " at (eval N) line M." suffix that Perl appends.
;;;   This is a documented PCL limitation; see docs/not-supported.md.
(defun p-eval (string)
  "Perl eval(STRING): transpile and evaluate a Perl string at runtime."
  (let ((s (to-string (unbox string))))
    ;; eval undef / eval "" -> return nil (undef), $@ = ""
    (when (string= s "")
      (box-set $@ "")
      (return-from p-eval nil))
    (let* ((pkg-name  (package-name *package*))
           (cache-key (cons s pkg-name))
           (cached    (gethash cache-key *p-eval-string-cache*)))
      (handler-case
          (let* ((cl-text (or cached
                              (let ((r (p-transpile-string s pkg-name)))
                                (setf (gethash cache-key
                                               *p-eval-string-cache*) r)
                                r)))
                 ;; READ with *package* bound so symbol interning uses the
                 ;; eval package (e.g. $x refers to the caller's $x).
                 (cl-form (let ((*package* *package*))
                            (read-from-string
                             (concatenate 'string "(progn " cl-text ")"))))
                 ;; EVAL with *package* bound so (in-package ...) in eval'd
                 ;; code does not escape into the caller's dynamic scope.
                 (result  (let ((*package* *package*))
                            (eval cl-form))))
            (box-set $@ "")
            result)
        (p-exception (e)
          ;; Object die: die $obj
          (box-set $@ (p-exception-object e))
          nil)
        (error (e)
          ;; String die or transpiler error
          (box-set $@ (format nil "~A" e))
          nil)))))
```

Keep `parse-number` (lines 4012–4017) unchanged.

---

## Step 3: `Pl/t/eval-01.t` — add 8 runtime tests

Update `tests => 12` to `tests => 20`. Add a `SKIP` block with:

1. Basic arithmetic: `eval "1 + 2"` → 3
2. String result: `eval q{"hello"}` → "hello"
3. `$@` is empty string (length 0) on success
4. `die` inside eval sets `$@`
5. eval returns undef on die
6. `eval undef` → undef, `$@` = ""
7. Multi-statement: return value is last expression
8. Package variable visible in eval

---

## Step 4 (post-implementation): uncomment eval tests

After all tests pass, selectively uncomment `eval "string"` tests in `perl-tests/`.
Start with `perl-tests/negate.t` test 48 (simplest). Leave the bulk of `index.t`
eval tests (Unicode/bytecode-optimizer internals) for later.

---

## Known limitations (intentional, not bugs)

- `$@` will NOT have `" at (eval N) line M.\n"` suffix — PCL doesn't track source locations.
  Already documented in `docs/not-supported.md`.
- Lexical `my` vars from the enclosing scope are NOT visible in the eval string.
  In PCL, `my` vars compiled to `defvar` ARE accessible (since defvar is dynamic), which
  is slightly more permissive than Perl. Acceptable — no CPAN code relies on this difference.
- Performance: first call for a given string spawns a Perl process (~50–100ms).
  The cache handles repeated identical strings.

---

## Future extension: persistent transpiler subprocess

**The idea (suggested 2026-03-28):** Instead of spawning a new `perl pl2cl` process
per `eval` call, start one long-lived subprocess running a transpiler server. Send
requests (package + code string) over a pipe, get CL back.

**Benefits:**
- Eliminates Perl startup cost on every `eval` call
- Also speeds up `require` (every uncached module load has the same problem)
- Cleaner architecture: the transpiler becomes a persistent service

**Protocol sketch:**
```
Request (SBCL → perl):   "<pkg>\n<length>\n<perl-code>"
Response (perl → SBCL):  "<status>\n<length>\n<cl-code-or-error>"
```

**SBCL side changes needed:**
- `*p-transpiler-process*` defvar — holds the live subprocess
- `p-ensure-transpiler` — start if nil or dead, restart on crash
- `p-transpile-string` sends a request, reads response (replaces subprocess call)

**Perl side changes needed:**
- `pl2cl --server` mode: loop reading requests, transpile, write response
- `Pl::Parser` must stay stateless across requests (it likely already is)

**Why not now:** Non-trivial structural change. Need to design the protocol, implement
subprocess lifecycle management on the SBCL side, and verify Parser.pm is stateless.
Get eval working correctly first with the simple per-call approach, then profile to
confirm the persistent subprocess is worth the complexity.
