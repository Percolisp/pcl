# Debugging Hangs and Crashes in PCL

## The Core Problem

PCL runs SBCL as a subprocess. `run-perl-test.pl` uses backticks to capture all SBCL
output — so if SBCL hangs, the backtick never returns. If SBCL crashes (exit before EOF),
the backtick returns with whatever was written so far.

Both look similar from the outside: "only N tests pass, then nothing."

---

## Step 1: Distinguish Hang vs Crash (30 seconds)

```bash
# Run with explicit timeout. If it returns quickly → crash. If it times out → hang.
timeout 10 perl run-perl-test.pl perl-tests/foo.t 2>&1 | tail -3
```

- Returns in <10s with partial output → **crash** (SBCL exited)
- Times out → **hang** (SBCL is spinning)

---

## Step 2a: Diagnosing a CRASH

SBCL crashes silently (no error message) when:
- An unhandled condition propagates past the top level
- `sb-ext:exit` is called explicitly
- A stack overflow or memory error occurs

### Add handler-case at the suspect location

```lisp
;; In pcl-test.lisp or the runtime, wrap the suspect call:
(handler-case
  (... suspect code ...)
  (error (e)
    (format t "### CRASH in foo: ~A~%" e)
    (force-output)
    fallback-value))
```

### Check if it's in test infrastructure vs. generated code

```bash
# Compare: does a trivial test crash too?
echo 'print "hello\n";' | ./pl2cl | sbcl --load cl/pcl-runtime.lisp --load cl/pcl-test.lisp --script /dev/stdin
```

### Run SBCL directly (not via run-perl-test.pl) to see SBCL's own error output

```bash
# run-perl-test.pl swallows stderr. Run manually:
cd perl-tests
perl ../pl2cl foo.t > /tmp/foo.lisp 2>/dev/null
sbcl --load ../cl/pcl-runtime.lisp \
     --load ../cl/pcl-test.lisp \
     --load /tmp/foo.lisp 2>&1 | head -50
```

This shows SBCL's condition/backtrace which is hidden in the normal test runner.

---

## Step 2b: Diagnosing a HANG

### Profile with sb-sprof to find the hot function

```bash
cd perl-tests
perl ../pl2cl foo.t > /tmp/foo.lisp 2>/dev/null
sbcl --eval "
(load \"../cl/pcl-runtime.lisp\")
(load \"../cl/pcl-test.lisp\")
(sb-sprof:start-profiling)
(let ((th (sb-thread:make-thread
           (lambda ()
             (setf *default-pathname-defaults* #p\"/home/bernt/pcl/perl-tests/\")
             (load \"/tmp/foo.lisp\")))))
  (sleep 3)
  (sb-sprof:stop-profiling)
  (sb-sprof:report :type :flat :max-samples 5)
  (sb-thread:terminate-thread th))" 2>&1 | grep -v "^;"
```

The top function in the flat profile is what's spinning.

### Use SBCL's built-in trace

```lisp
;; Trace a specific function — prints every call/return
(trace pcl::test-to-scalar)
(trace pcl::p-split)
```

**WARNING**: `(trace foo)` with `~S` format on complex structs (p-box, hash-table) can
itself cause a hang. Use `(untrace foo)` and add manual prints if trace hangs.

### Add manual progress prints with force-output

The key: `format t` output is buffered. `force-output` flushes it. Without `force-output`,
you won't see output from a hanging function.

```lisp
(defun suspect-fn (x)
  (format t "### entering suspect-fn type=~A~%" (type-of x))
  (force-output)
  ... body ...
  (format t "### leaving suspect-fn~%")
  (force-output))
```

**NEVER use `~S` for printing p-box values** — the struct printer recurses into
the box contents and can loop. Use `(type-of x)` or `(p-box-p x)` checks instead.

### Safe value display pattern

```lisp
;; SAFE: type-only, no recursion
(format t "### val type=~A box=~A~%" (type-of x) (p-box-p x))

;; SAFE: convert to string first with length limit
(let ((s (with-output-to-string (out)
           (handler-case (format out "~A" x)
             (error () (write-string "ERROR" out))))))
  (format t "### val=~A~%" (subseq s 0 (min 40 (length s)))))

;; UNSAFE: ~S on a p-box — may hang
(format t "~S~%" some-p-box)   ; DON'T DO THIS
```

---

## Step 3: Fast Iteration Loop

Once you know WHERE the issue is, work on a **minimal Perl snippet** not the full test file:

```bash
# Minimal repro — run directly with SBCL for full error output:
cat > /tmp/min.pl << 'EOF'
# Just the failing construct
my @arr = ([1,"a"], [2,"b"]);
for (@arr) { print "$_->[0]\n"; }
EOF

perl pl2cl /tmp/min.pl > /tmp/min.lisp 2>/dev/null
# Check generated code first:
grep "p-foreach\|p-flatten" /tmp/min.lisp

# Then run with SBCL directly to see all output including errors:
cd perl-tests
sbcl --load ../cl/pcl-runtime.lisp --load ../cl/pcl-test.lisp \
     --load /tmp/min.lisp 2>&1
```

**Always check the generated CL before running it.** Many bugs are visible in the CL
without needing to run SBCL at all.

---

## Step 4: Regression Testing After Fix

After any runtime or codegen fix:

```bash
# 1. Quick smoke test
echo 'print "ok\n";' | ./pl2cl | sbcl --script /dev/stdin

# 2. PCL test suite (catches codegen regressions fast)
prove -j8 Pl/t/ 2>&1 | tail -3

# 3. Run the specific Perl test you were fixing
timeout 60 perl run-perl-test.pl perl-tests/foo.t 2>&1 | tail -3

# 4. Check previously-passing files that might be affected
# (run a few related ones, not the full sweep)
for t in perl-tests/sprintf.t perl-tests/closure.t perl-tests/my.t; do
  result=$(timeout 30 perl run-perl-test.pl $t 2>&1 | grep "Passed:")
  echo "$t: $result"
done

# 5. Full sweep only when confident
timeout 600 perl sweep-perl-tests.pl 2>&1 | tail -5
```

**Do step 4 before step 5.** A regression caught in step 4 (30 seconds) saves
rerunning the full sweep (10 minutes).

---

## Common PCL-Specific Pitfalls

### Working directory
`run-perl-test.pl` `chdir`s to `perl-tests/` before running SBCL. When running SBCL
manually, do the same: `cd perl-tests` or set `*default-pathname-defaults*`.

### FASL cache
The sweep script passes `--eval "(setf pcl::*pcl-skip-cache* t)"`. Manual SBCL runs
don't do this. If you see stale behavior, `rm -rf ~/.pcl-cache/`.

### `~S` format on structs
SBCL's `~S` on a p-box calls the struct printer which recurses into box contents.
If the box holds a hash-table with a back-reference, this loops forever. Always use
`(type-of x)` or a length-limited `~A` string for debugging output.

### Double-wrapping
Codegen fixes that wrap args (e.g., `(p-flatten ...)`) can double-wrap if the same
arg appears in two code paths (gen_funcall AND gen_progn both check @-sigil). Always
check the generated CL after a codegen change to look for double-wrapping.

### `format t` vs `format *error-output*`
Both get swallowed by `run-perl-test.pl`'s backtick. Run SBCL directly to see all output.

---

## Do We Need to Change the Test Runner?

**Not urgently**, but these improvements would help:

1. **Show SBCL stderr separately**: Add `2>/dev/null` to suppress compiler noise but
   capture condition errors separately. Currently both go to backtick.

2. **Timeout per-test**: Add `alarm(30)` around SBCL invocation in `run-perl-test.pl`
   to detect hangs automatically rather than waiting forever.

3. **Run SBCL directly mode**: A `--debug` flag that skips backtick capture and lets
   SBCL output go directly to terminal — useful when investigating crashes.

The current runner is fine for normal use; it just makes hang/crash debugging harder
than it needs to be.
