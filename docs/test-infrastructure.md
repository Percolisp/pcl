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

## The saved-core optimisation — IMPLEMENTED, and the DEFAULT since s439

**Status (s439, USER ruling "by default the CL runtime is kept compiled and
cached"):** `tools/lib/PCLSbcl.pm` — the ONE SBCL command builder every
runner shares — resolves a core in this order: an explicit `core` argument;
`PCL_TEST_CORE` (the gate's contract, mtime-fresh); the INSTALLED
`<root>/pcl.core` (`tools/install-pcl`'s product); **the CACHED core**
`~/.pcl-cache/core/pcl-<path8>-<content12>.core`, built on first use from
`cl/pcl-runtime.lisp` and named by a hash of the runtime's absolute path,
its source, `sbcl --version`, `~/.sbclrc`'s size+mtime and a format version
— an edit, an SBCL upgrade or a second checkout produces a different name,
so there is no stale-core case; source mode last (`PCL_NO_CORE=1` forces it,
a failed build falls back to it loudly and leaves a one-hour `.failed`
marker).  The extensions (`cl/pcl-pack.lisp`, `pcl-mro`, `pcl-warnings`,
`pcl-xs`) are loaded lazily from `*pcl-runtime-directory*` and are NOT in
the core; `cl/pcl-test.lisp` and the skip registry are `--load`ed by the
callers after the prefix, as before.  Concurrent first spawns take an
`flock`; the build is tmp+rename.  Measured: plain `prove -j8 Pl/t/` 224 s
wall (s439) — the same as `tools/prove-core`, which still rebuilds a fresh
temp core per run as belt and braces.  Test: `tools/t/sbcl-prefix.t`.

The original note, kept for the reasoning:

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

---

## How a MEASUREMENT runner loads the generated CL (task #467, s434)

**Rule: both measurement runners load the emitted CL with
`pcl::p-load-with-recovery`; a program (`./runpcl`, a user's own SBCL) uses a
plain load.**

`p-load-with-recovery` (`cl/pcl-test.lisp`) reads and evaluates the generated
file **one top-level form at a time** and continues past an uncaught error in
any single form, instead of ending the file the way `LOAD` does. It is
faithful to `LOAD` for PCL's output — the reader tracks `*package*` between
forms, and every `eval-when` PCL emits includes `:execute` — so a file with no
uncaught top-level die evaluates identically, form for form. Each caught error
is PRINTED on `*error-output*`, never swallowed.

Why it is a rule and not a preference: the two runners disagreed on exactly
this axis until s434, and **the disagreement is invisible in either report**.
`sweep-perl-tests.pl` had recovery; `tools/run-perl-suite.pl` used `--load`.
Measured s432, the *same* compiler change (#456 half (a): a called
forward-declaration stub dies instead of answering nil) cost

* the sweep **one row** — the form that died;
* the companion suite **94 rows** — op/method.t 96→44, op/sort.t 181→142,
  op/lexsub.t 9→6 — every one of them a row *after* the dying form, in files
  that already crashed.

So a per-file row count from one population was not comparable to one from the
other for any change that makes something die, and the difference was the
RUNNER, not PCL. Same class of trap as #324 (one runner measuring PCL on a
2 MB control stack for months), which is why the five SBCL-spawning runners
share one command-line builder (`tools/lib/PCLSbcl.pm`). Recovery is the
second axis they have to share.

The trade is real and accepted: recovery lets a file report rows perl would
never have reached either, so a hard failure can read as a partial pass. It is
paid for by making the recovery LOUD — `tools/run-perl-suite.pl` counts the
aborted forms and puts `aborted-forms:N: <first condition>` in the file's
signature column, which keeps such a file out of status OK even when its
remaining TAP happens to match perl's. `baselines/perl-suite-run.tsv` was re-blessed
in one measured `--all` pass when the change landed (see its s434 header
block); a file that reported FEWER rows after the change would have been a
finding, not a re-bless.

Users stay on a plain load on purpose: recovery is a MEASUREMENT policy — it
buys rows after a failure, which a harness wants and a program must not.

**The signature has a 90-character budget, and a path can eat all of it.**
Since the s435 announce→DIE flip the commonest aborted-form condition is the
drop's own message, which begins with an absolute file name — and every such
signature came back as `…/perl-N.N.N/t/o`, naming nothing, which is the one
thing a signature exists to do. `tools/run-perl-suite.pl` therefore collapses a
DEEP absolute path to its basename before truncating (two or more directory
components, and only where a path can start, so a substitution in the quoted
source text and a shallow `"/dev/tty"` are left alone). This is cosmetic **by
construction**: `read_snapshot` compares status + `C_ok` + `C_notok` and never
the signature, so no verdict can move with it. Keep it that way — the moment a
comparison reads `$sig`, every random tmpdir in a condition string becomes a
phantom mover.

### The snapshot's own hole

`tools/run-perl-suite.pl` also prints, at the end of every run, how many of the
files it measured have **no row in `baselines/perl-suite-run.tsv`** (with their
names). Five files had none for months (s431): a file with no snapshot row can
never read as a mover, because the mover check compares against the snapshot —
the #176 family, a hole inferred from an absence. It is printed, never fatal:
it is a fact about the baseline, not a measurement this run failed. A row is
added by splicing the run's FIRST measurement in with a `# sNNN first
measurement` marker.

### Drops in what the run itself transpiles (`PCL_DROP_LOG`, task #472, s438)

Every instrument that counts the #138 drop family — `tools/drop-census.pl`,
`tools/corpus-diff.pl`'s SILENT-DROP counter, both measurement runners' `drops`
column — finds drops by reading **emitted `.lisp` files**. A program run by
`fresh_perl_is`/`runperl` is transpiled from a **string** at run time
(`tools/pclperl-for-tests`), and a module the running program loads is
transpiled on the fly, so neither is a file in any census population. A drop
there was invisible to everything the project had, and the two that were known
had been making rows **pass on nothing** for years (perl-tests/split.t:682,
perl-tests/bop.t:701 — both surfaced only when the s435 flip made the child die
instead of silently printing nothing).

**The instrument is a side channel in the ONE announcer.**
`Pl::Parser::_announce_dropped_statement` appends
`FILE<TAB>LINE<TAB>TEXT<TAB>REASON` to the file named by `$PCL_DROP_LOG`, for
every drop, **ungated by `PCL_DROP_ANNOUNCE`** (it must see module-mode drops
too, which announce nothing by ruling). It is *not* routed to stderr, and that
is the whole design: **the child's stderr IS the row's observed output**, which
`fresh_perl_is` compares — an instrument that changes a verdict is not an
instrument. It records the same thing the census counts (a statement lost from
an emitted program), so it is deduplicated per statement, a ruled Track-A
refusal is not logged (the file is refused, loudly, in every mode) and an
eval-string drop is not either (it DIES at transpile, #363, and emits nothing).

`sweep-perl-tests.pl` sets it **around the SBCL run only**, never around the
file's own transpile — otherwise the file's own drops would be counted twice —
and reports two numbers, because either alone lies:

* the per-file **count** (also the `child-drops` column in `_status.tsv`): what
  that file's run lost. It double-counts a drop in the harness, which every
  file re-transpiles under `*pcl-skip-cache*`;
* the **distinct sites** across the corpus, with how many test files reached
  each. That is the census view: a harness drop reads as one site in many
  files, not as many drops.

REPORTED, NOT GATED (ruled `fable-answers-s437.md` §2 ask 5): rows are blessed
by hand in `baselines/parse-error-drop-census-s399.tsv`'s header after a measured
run, and only then can a rise be a failure. The first measurement (s438, 108
files) is 241 drops in 98 files across **ten** distinct sites.

A test file that clears `%ENV` for its children hides them from this
instrument — an undercount, never a miscount.
