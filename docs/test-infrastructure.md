# PCL: Test Infrastructure Notes

## Overview

PCL has two test layers:

- **`Pl/t/`** — the PCL unit-test suite (currently ~2700 tests across 67 files).
  Run with `prove -j8 Pl/t/`.  Each file spawns one SBCL process.

- **`perl-tests/`** — the Perl op-test suite ported from Perl's own `t/op/`
  directory.  Run with `perl sweep-perl-tests.pl --jobs 8` from the project
  root.  Each file spawns one SBCL process via `run-perl-test.pl`.

---

## Why tests are slow: SBCL startup time

A single test file takes 1–2 seconds to run, even if the test itself finishes
in milliseconds.  Nearly all of that time is SBCL startup overhead, not the
test code.  This section explains why.

### The core image

SBCL ships as two parts:

- A small executable (~13 MB on x86-64).
- A **core file** (~60–100 MB) that contains the entire pre-built Lisp system:
  the compiler, garbage collector, all standard packages and their interned
  symbols, CLOS infrastructure, condition system, reader tables, and more —
  serialized into a heap image.

When SBCL starts it `mmap`s the core file into the process's address space.
Even though `mmap` avoids reading most pages until they are touched, there is
still significant initialization work: pointer relocation, GC card-table setup,
thread-state restoration, and so on.

By comparison, Perl's executable (~2 MB) is a conventional shared-library
binary.  Its startup is essentially `execve` + dynamic linking — something the
OS has been heavily optimized to do in a few milliseconds.

### Loading source files compiles to native code

The bigger cost in our pipeline is loading `pcl-runtime.lisp`.  When SBCL
evaluates a `--load` argument it does **not** interpret the source; it compiles
each top-level form to x86-64 machine code on the fly.  Every `defun`,
`defmacro`, and `defvar` passes through the full SBCL compiler: IR1 (source
transforms and type inference), IR2 (register allocation and instruction
selection), and code emission.

`pcl-runtime.lisp` is about 7,000 lines with several hundred definitions.
Compiling it takes 0.5–1.0 seconds on a modern machine.

Perl also compiles source to bytecode at load time, but its compiler is a
single-pass, non-optimising translator.  SBCL's multi-pass optimising compiler
produces faster-running code — but at a higher up-front cost.

### GC heap initialisation

SBCL's generational garbage collector needs to set up multiple heap spaces
(nursery, older generations, large-object space) and a write-barrier card table
covering the entire heap range.  This happens unconditionally at startup,
regardless of how much Lisp code will actually run.

### Per-test cost breakdown (approximate, typical laptop)

| Phase | Time |
|---|---|
| SBCL core `mmap` + pointer relocation | ~0.2–0.3 s |
| `pcl-runtime.lisp` compile and load | ~0.5–1.0 s |
| `pcl-test.lisp` load | ~0.1 s |
| Transpiled test file load and run | ~0.05–0.3 s |
| **Total per test file** | **~1–2 s** |

Perl starts and compiles a comparable source file in 20–50 ms — roughly
30–60× faster.

---

## The `fresh_perl_is` / `fresh_perl_like` problem

Perl's standard test harness (`t/test.pl`) provides `fresh_perl_is` and
`fresh_perl_like`.  These functions:

1. Take a Perl code snippet as a string.
2. Spawn a fresh `$^X` (Perl) subprocess to run it.
3. Capture the subprocess's stdout (and optionally stderr).
4. Compare the output with `is()` or `like()`.

In PCL's `perl-tests/t/test.pl` stub these functions are no-ops
(`sub fresh_perl_is { return; }`).  If we implemented them to run snippets
through the PCL pipeline instead (transpile with `pl2cl`, run with SBCL),
each call would add another 1–2 second SBCL startup.  A test file with 137
`fresh_perl_is` calls would take 2–5 minutes just for subprocesses.

An alternative — run snippets through the real Perl interpreter — would make
those tests pass but would be testing Perl's behaviour, not PCL's.

The correct long-term fix is either a saved SBCL core (see below) or
implementing string `eval` (which is what the inline eval-based tests need
anyway).

---

## Error message format

Perl's error messages have specific, documented wording
(`"Can't find string terminator"`, `"syntax error at FILE line N"`, etc.).
SBCL condition text looks completely different.  PCL therefore does **not**
guarantee that its error messages match Perl's.  Test files that use
`fresh_perl_like(..., qr/error pattern/)` or `like($@, qr/.../)` on error
strings will mostly fail.  See `docs/not-supported.md` for details.

---

## The saved-core optimisation (not yet implemented)

The standard SBCL way to amortise startup cost is to save a **Lisp image**
(core) with the runtime already loaded:

```bash
sbcl --load cl/pcl-runtime.lisp \
     --load cl/pcl-test.lisp \
     --eval '(sb-ext:save-lisp-and-die "pcl-test.core" :executable nil)'
```

Then each test is launched with:

```bash
sbcl --core pcl-test.core --script /tmp/transpiled-test.lisp
```

Loading a pre-built core avoids recompiling the runtime from source.  The
SBCL startup cost would drop to roughly 0.2–0.3 s per test — comparable to
Perl.

**Trade-off:** the saved core must be rebuilt whenever `pcl-runtime.lisp` or
`pcl-test.lisp` changes.  It is also platform-specific (an x86-64 Linux core
cannot be used on arm64 or macOS).  A `Makefile` target that rebuilds the
core when the source files are newer would handle this automatically.

This optimisation would make `fresh_perl_is`-style subprocess tests
practical: 138 subprocesses × 0.25 s ≈ 35 s instead of 3–5 minutes.

---

## Running the test suites

```bash
# PCL unit tests (fast, ~2 min with -j8)
prove -j8 Pl/t/

# Single file, verbose
prove -v Pl/t/sort-01.t

# Perl op-test suite sweep (parallel)
perl sweep-perl-tests.pl --jobs 8

# Single perl-test file
perl run-perl-test.pl perl-tests/sort.t
```

The sweep script passes `*pcl-skip-cache* t` to SBCL so module caching is
bypassed, ensuring each run starts from a clean state.
