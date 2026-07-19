# Assessment (PCL team, 2026-07-19, Opus 4.8)

External advice from Google Gemini, scored against **this session's
measurements** (`tools/bench-exec.pl` + head-to-head CL variant experiments;
see `docs/faster-codegen-suggestions.md`) and PCL's design constraints
(CLAUDE.md §1: match Perl semantics exactly, run real CPAN code). The raw
advice is preserved below. TL;DR: one real lever we'd already identified, one
fair caution we'd already heeded, one correctness trap, one benchmark-gaming
suggestion that would break CPAN compat.

| point | verdict | disposition |
|---|---|---|
| #2 type-declared Lisp | **Right lever, unsound as written** | Already the plan (O1 + the analysis rewrite); the `safety 0` framing is rejected |
| #1 `create-scanner` once | **Already doing** | = perf-doc R1 (compile constant patterns at load time) |
| #1 FFI-overhead caution | **Useful confirmation** | Reinforces task #71 / §8 ("measure plumbing vs engine, cache the buffer") |
| #3 displaced-array substr | **Correctness trap** | Rejected; niche sound variant noted in §4 |
| #4 implicit multithreading | **Breaks Perl semantics** | Rejected |

**#2 — type declarations (the one with substance).** Measured this session:
`(declare (fixnum …)) (safety 0) (the fixnum …)` ran **10×** faster than the
current `p-+`. So the *direction* is right and is exactly where PCL's big
arithmetic win lives. **But the advice omits the entire hard part, and the
omission is unsound:** (a) Perl integers promote to *double* on overflow, so
`(the fixnum (+ x y))` under `safety 0` is undefined behavior — silent
corruption, not promotion — unless the value's range is *proven* fixnum
(= perf-doc **O1**, "hard soundness, low priority"); (b) our sound typed
variants (`a3` bignum-correct `integer`, `a4` fixnum-guarded) gained **nothing**
over today's `p-+` — the 10× is *only* available behind a proof; (c) global
`(safety 0)` turns a catchable Perl error into a segfault, unacceptable for
arbitrary CPAN code — PCL targets `(speed 2) (safety 1)`. Verdict: validates
the destination, hand-waves the road. Proving the types soundly **is** the
variable-analysis rewrite.

**#1 — cl-ppcre `create-scanner`.** The "compile the pattern once" tip is
already **R1** in the perf doc (constant patterns → `load-time-value`
scanner); it does not touch the measured ~3.7× *engine* gap. The **FFI-overhead
caution about PCRE2 is legitimate and independently confirms our own note** —
the string-marshalling cost (UCS-4↔UTF-8, byte→char offset mapping) is exactly
why task #71 says *measure PCL plumbing vs the engine first and cache the
encoded subject*. Where it's wrong: "ditch PCRE2" ignores the stronger reason
to want it — **Perl-compatible correctness** (closes `/n`, `(?{…})`, Unicode
property classes that cl-ppcre cannot). Keep #71 as the *contingent* future
item; this caution strengthens its "measure first" gate rather than killing it.

**#3 — displaced arrays for substr/chomp.** Rejected. (a) *Semantics*: rvalue
`substr` returns an independent **copy** in Perl; a `:displaced-to` array is a
live **view** that changes when the parent mutates → wrong results. (b)
*Performance backfire*: displaced arrays are **non-simple** strings, so
downstream ops lose SBCL's simple-string fast paths and may be slower or copy
anyway. The measured string win is the **append buffer (~2400×)**, not this.
One narrow *sound* use survives — read-only substring **scanning** via
`(start,end)` index pairs that never materializes the substring — recorded in
`faster-codegen-suggestions.md` §4.

**#4 — implicit multithreading.** Firmly declined: it rejects the design
premise. Perl has **defined evaluation order and pervasive side effects**
(print order, `$_`/global mutation, tied vars, IO); auto-parallelizing
`map`/loops changes observable behavior and races shared boxes — it would break
real CPAN code. "Obliterate Perl in a benchmark" is the tell: it games an
embarrassingly-parallel microbench while violating CLAUDE.md §1. The only sound
form needs a *proof* that a `map` body is pure and order-independent (very hard
over arbitrary Perl, rare payoff). Explicit concurrency PCL already supports
(`fork` works; `threads`/`kill` map to real facilities when the *user* asks).

**Meta-lesson.** This is generic "make SBCL fast" advice that assumes you
already know the types and can drop safety — precisely the assumption a Perl
transpiler *cannot* make for free. Everything hard about PCL performance is
*earning the right* to emit those fast declarations soundly.

---

# Original advice (Gemini, verbatim)

## 1. Ditch PCRE2, Use cl-ppcre (With a Secret Weapon)
While PCRE2 has a fast JIT, calling it from SBCL introduces FFI (Foreign Function Interface) overhead. Passing strings back and forth between Lisp memory and C memory will kill your performance layout.
Instead, stick to cl-ppcre, but use its create-scanner function.

* cl-ppcre compiles regular expressions into native Lisp closures (closures are essentially compiled machine code functions in SBCL).
* Once compiled, a cl-ppcre scanner runs entirely in SBCL's native space with zero memory-copying overhead.

## 2. Macro-Expand to Type-Declared Lisp Code
Perl is dynamically typed and spends time checking types at runtime. SBCL can compile code that matches C-speed if you give it strict type hints.
When your Percolisp compiler encounters a function, emit Lisp code wrapped in strict optimization qualities.

(declaim (optimize (speed 3) (safety 0) (debug 0)))

If you know a variable is a fixnum (SBCL's unboxed integer) or a simple string, declare it explicitly:

(defun percolisp-add (x y)
  (declare (fixnum x y))
  (the fixnum (+ x y)))

This strips away all of SBCL's runtime safety checks, turning your Lisp code into raw, blazing-fast assembly instructions that will effortlessly outrun Perl's interpreter loops.
## 3. Exploit Lisp's Direct Memory Access
Perl handles strings by constantly managing ref-counts and heavy SV (Scalar Value) structures. SBCL handles arrays beautifully. If Percolisp has string-manipulation functions (like substr or chomp), map them directly to SBCL's make-array with :displaced-to.
Displaced arrays let you point to a subsection of a string without copying the data in memory. It is a zero-copy operation, which will make your string operations instantly smoke Perl's memory-allocator.
## 4. Cheat with Multithreading
Perl has notoriously clunky, heavy-weight pseudo-threads (ithreads). SBCL uses raw, native OS threads (sb-thread).
If you want to absolutely obliterate Perl in a benchmark, bake implicit parallelism into Percolisp's heavy loops or file-processing functions. A simple map-reduce function running across 4 native cores in SBCL will leave a single-threaded Perl script in the dust.
------------------------------


