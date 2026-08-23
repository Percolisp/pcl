# Percolisp (PCL) — a Perl-to-Common-Lisp compiler

**PCL compiles Perl 5 source to Common Lisp, and a CL runtime reproduces
Perl's semantics** — so the result runs as native code under
[SBCL](https://www.sbcl.org/).  It is a from-scratch source-to-source
compiler: Perl in, Common Lisp out.  No Perl interpreter, no bytecode engine,
nothing of perl's C runtime is linked or reimplemented.

```bash
$ echo 'my @a=(1..5); print join(",", map { $_*2 } @a), "\n";' | ./runpcl
2,4,6,8,10

$ ./pcl -MData::Dump=dump -E '@q=(1 .. 5); say dump [ map { $_, ":", $_ ** $_ } @q ];'
[1, ":", 1, 2, ":", 4, 3, ":", 27, 4, ":", 256, 5, ":", 3125]
```

| | |
|---|---|
| **Status** | pre-release; v0.1.0 is the first tag (see [Roadmap](#roadmap)) |
| **Measured compatibility** | [`docs/STATUS.md`](docs/STATUS.md) — every number re-runnable |
| **What deliberately does not work** | [`docs/not-supported.md`](docs/not-supported.md) — each entry with its reason |
| **Changes** | [`CHANGELOG.md`](CHANGELOG.md) |
| **License** | same terms as Perl itself — [`LICENSE`](LICENSE) |

---

## Contents

1. [Why compile Perl to Common Lisp](#why-compile-perl-to-common-lisp)
2. [Where it stands](#where-it-stands)
3. [Quick start](#quick-start) — requirements, **minimum SBCL 2.5.2**, install, test
4. [The plan: a compiler toolkit](#the-plan-a-compiler-toolkit) — the IR, speed, XS
5. [Architecture](#architecture)
6. [How it is tested](#how-it-is-tested)
7. [Roadmap](#roadmap)
8. [Reading guide](#reading-guide)

---

## Why compile Perl to Common Lisp

* **A high-level target keeps the compiler tractable.**  CL is expressive
  enough to model Perl's data model and control flow directly, so PCL stays a
  compiler of manageable size instead of growing into a second interpreter.
* **Perl's run-time *magic* runs at run time.**  Much of Perl only exists
  while the program executes (context, coercion, `local`, ties, overloads,
  string `eval`).  PCL does not try to decide those statically: the CL runtime
  ([`cl/pcl-runtime.lisp`](cl/pcl-runtime.lisp)) reproduces them with the
  same semantics, and the compiler emits calls into it.
* **Lisp is trivial to parse, so the output is a usable IR.**  The generated
  CL is specified ([`docs/ir-spec.md`](docs/ir-spec.md)) and readable —
  sigils kept (`$x`, `@a`, `%h`), built-ins as `pl-`/`p-` names — which makes
  it a stepping stone for compiling Perl onward to other environments.
* **SBCL is a real optimizing compiler.**  Once the code generator proves
  where Perl's generality is not needed, the same loop runs as plain native
  code — that is the speed plan below.

## Where it stands

All numbers are measured on named, re-runnable instruments; the table in
[`docs/STATUS.md`](docs/STATUS.md) is the authoritative copy.

| measurement | result (2026-08-23) | reproduce |
|---|---|---|
| PCL's own regression gate (`Pl/t/`) | **166 files / 5,780 assertions**, all passing (the 14 XS-bridge rows need the `pclxs` sibling) | `tools/prove-core` |
| Perl's own test suite, extracted (`perl-tests/`, 108 files from perl's `t/op`, `t/base`, …) | **18,312 assertions pass / 894 fail (95.3 %)**; **62 files pass completely**; tracked row by row against blessed baselines, so the number can only move honestly | `perl sweep-perl-tests.pl --jobs 8` |
| Perl's full `t/` tree, in place (528 files) | run per directory as a bug-finder; verdicts per file against a blessed snapshot | `tools/run-perl-suite.pl --all --quick --jobs 4` |
| XS bridge conformance ([pclxs](#4-xs--c-extensions), 398 cases, real perl as oracle) | **398 pass / 0 fail**; `Digest::MD5`'s own `md5-aaa.t` passes 256/256 under PCL | `tools/pcl-conform` |
| Pure-Perl CPAN modules (183-dist board) | 65 PASS / 65 PARTIAL / 53 FAIL at dist granularity (a PARTIAL runs most of its suite) | `tools/cpan-scoreboard.pl` |

What is in: the expression grammar with all precedence levels and context
propagation; closures and `state`; `local` on scalars, aggregates, elements and
typeglobs; OO with C3 MRO, `SUPER::`, `AUTOLOAD`, `use overload`; block and
string `eval` **with lexical capture**; regexes via CL-PPCRE; `pack`/`unpack`;
`tie`; signatures; perl 5.34 `try`/`catch`.  What is deliberately out (with the
reasons): `@_` aliasing, deterministic `DESTROY`, exact error-message text,
`given`/`when`/`~~` (removed in perl 5.42), the `mro` switch, and a short list
of internals-introspection features — [`docs/not-supported.md`](docs/not-supported.md).

A statement the compiler cannot lower is **never silently lost**: it is
announced at compile time (`PCL: statement dropped at FILE line N: …`) and
**dies, perl-shaped and trappable, when the program reaches it**.  The count of
such statements over every test population is tracked and gated in-repo
([`docs/parse-error-drop-census-s399.tsv`](docs/parse-error-drop-census-s399.tsv)).

## Quick start

### Requirements

| what | version | notes |
|---|---|---|
| Perl | 5.20+ | with [PPI](https://metacpan.org/pod/PPI) ≥ 1.291 and [Moo](https://metacpan.org/pod/Moo): `cpanm PPI Moo` |
| **SBCL** | **≥ 2.5.2 — hard minimum** | the runtime uses SBCL-internal APIs (`sb-unicode`, float bit accessors, …) and is validated on 2.5.2 and 2.6.0.  Its first form checks the version and warns loudly on an older host.  Distribution packages that qualify: Debian 13 "trixie", Ubuntu 25.10 / 26.04 LTS.  Debian 12 (2.2.9), Ubuntu 24.04 LTS (2.2.9) and 22.04 LTS (2.1.11) do **not** — install the current binary from [sbcl.org](https://www.sbcl.org/platform-table.html); a home-directory install needs no root. |
| cl-ppcre | any current | via Quicklisp: `sbcl --eval '(ql:quickload :cl-ppcre)' --quit` |

Do not load the runtime with `sbcl --script`: that flag skips `~/.sbclrc`, so a
Quicklisp-installed cl-ppcre becomes invisible and the load fails.  The
wrappers below pass the right flags.

### Run, transpile, install, test

```bash
./runpcl prog.pl                      # transpile + run a program
echo 'print 1+2, "\n"' | ./runpcl     # … or from stdin
./pl2cl prog.pl > prog.lisp           # transpile only (readable CL)
./pcl -MList::Util=sum -E 'say sum 1..10'   # one-liners, -M imports of pure-Perl modules

tools/install-pcl --prefix ~/.local   # install: copies the runtime tree, writes bin/ wrappers,
                                      # COMPILES the runtime into a saved SBCL core at install time
                                      # (never at first use), then self-verifies by running a program
tools/install-pcl --no-core --dry-run # show what it would do

tools/prove-core                      # the regression gate against a fresh saved core (~4 min)
prove -j8 Pl/t/                       # the same gate, plain (the reference; slower)
```

`tools/prove-core` and `prove -j8 Pl/t/` check the same thing — the core is a
speed cache rebuilt fresh on every run, never a stale image.

### Example

```perl
# input.pl
package Animal;
sub new   { bless { name => $_[1] }, $_[0] }
sub speak { "I am " . $_[0]->{name} }

package Dog;
our @ISA = ('Animal');
sub speak { $_[0]->SUPER::speak() . " and I bark" }

package main;
my $d = Dog->new("Rex");
print $d->speak(), "\n";
```

```bash
$ ./runpcl input.pl
I am Rex and I bark
```

## The plan: a compiler toolkit

PCL is built to be more than one translator.  Three product targets, in the
order they are being pursued; each has a design document and a measurement.

### 1. Correctness first — run real CPAN code

The current phase.  The oracle is Perl itself: perl's own `t/` suite and CPAN
dists' test suites are compiled and run, perl's expectations are the
assertions, and every failure is a row in a blessed baseline that a change
may only move with an explanation.  Module-specific behaviour lives in
pure-Perl shims under `lib/` that PCL compiles like user code — the compiler
and runtime hold language mechanisms only
([`docs/shipped-modules.md`](docs/shipped-modules.md)).

### 2. The generated CL is a specified intermediate representation

The output is meant to be *read by other tools*, not only loaded by SBCL:

* [`docs/ir-spec.md`](docs/ir-spec.md) — the normative manual: the data model
  (the "box"), coercion and truthiness tables, the context protocol, the
  calling convention, control flow and non-local exits, OO dispatch, the load
  model and string `eval`.  A translator to another target reads this and
  the runtime as reference.
* The emission is a small **named vocabulary** of macros (`p-list-ctx`,
  `p-sort-cmp`, `p-try`, …) over plain CL, so the structure of a Perl
  program survives into the output instead of being expanded away.
* [`docs/generated-cl-ir-review.md`](docs/generated-cl-ir-review.md) — what
  a consumer may rely on, and the friction list still being worked down.
* Extensions written *in Perl* and compiled by PCL ship as part of the
  runtime — `pack`/`unpack` ([`cl/pack-impl.pl`](cl/pack-impl.pl)),
  `mro`, `warnings` — see [`docs/extensions.md`](docs/extensions.md).

### 3. Speed — beat perl

The generated code compiles to native code, yet a naive translation is slower
than perl, because perl is 30 years of hand-tuned C and the naive translation
makes the machine do bookkeeping around every operation (every scalar a box
that can be a number, a string and a reference at once; every operation
coercing and checking context).  The plan is to **stop paying for generality a
given line of code provably does not need**, with a boxed fallback whenever the
proof is not available — a wrong analysis loses a speed-up, never
correctness.

* [`docs/where-the-time-goes.md`](docs/where-the-time-goes.md) — the four
  "taxes" every emitted operation pays today, in plain language, with numbers.
* [`docs/faster-codegen-suggestions.md`](docs/faster-codegen-suggestions.md) —
  the measured worklist: for each hot category, the emitted shape vs the
  faster shape that computes the identical result, head to head (string
  append ~2400×, method dispatch ~15×, boxed accumulator ~13×, …), and the
  precondition each needs.
* The optimisations are **named, switchable transforms** in
  [`Pl/Passes.pm`](Pl/Passes.pm) — `PCL_OPT=none` is the general-form
  compiler, `PCL_OPT=-str-buffer,-raw-numeric` turns named ones off — so
  every speed change is measurable in isolation and the gate runs both ways.
* `tools/bench-exec.pl` measures execution time against perl with startup
  subtracted (best-of-3, 2026-08-23; ratio < 1 = PCL faster):

  | bench | perl | PCL | PCL/perl |
  |---|---:|---:|---:|
  | `fib(27)` (recursion) | 1.47 s | 0.43 s | **0.29×** |
  | C-style counting loop | 0.106 s | 0.027 s | **0.25×** |
  | collatz (`while` + `%` `/` `*`) | 1.93 s | 0.78 s | **0.40×** |
  | gcd, recursive | 0.187 s | 0.094 s | **0.50×** |
  | `for (1..n) { $s += $_ }` | 0.065 s | 0.138 s | 2.1× |
  | hash + array element writes | 0.129 s | 0.278 s | 2.2× |
  | `$s .= 'x'` (string append) | 0.002 s | 0.008 s | 3.8× |
  | array slices | 0.066 s | 0.316 s | 4.8× |
  | overloaded-operator calls / symbolic refs | 0.03 s | 0.2 s | ~7× |
  | `pack`/`unpack` (the compiled-from-Perl oracle) | 0.004 s | 4.9 s | ~1300× |

  Calls, recursion and arithmetic loops already beat perl; element access,
  slices, dispatch and the `pack` oracle are the gap, and each row of the
  worklist above is one of them.  When readability of the output and speed
  conflict, speed wins.

### 4. XS / C extensions

The long game, and the one where help is most welcome.  A separate project,
**pclxs**, implements a `libperl`-ABI shim: an unmodified compiled XS module
is built once against the shim and calls into *any* host that provides the
vtable — PCL is the first host.  It is real, not a sketch: pclxs's 398-case
conformance corpus passes against real perl as the oracle, XS OO works
(`Digest::MD5->new->add->hexdigest`), and the artifact cache and the
install-time compile model are in place.  Design and status:
[`docs/xs-shim-design.md`](docs/xs-shim-design.md),
[`docs/xs-artifact-cache.md`](docs/xs-artifact-cache.md),
[`docs/xs-blessed-ref-referent-bug.md`](docs/xs-blessed-ref-referent-bug.md)
(XS OO, done), [`docs/xs-abi5-and-destroy.md`](docs/xs-abi5-and-destroy.md)
(what is next).  The `io` capability group (filehandles crossing the bridge)
is the remaining optional group; getting a broad slice of CPAN's XS dists
working is the goal.

## Architecture

```
Perl source → PPI → Pl::Parser2 (statement lowering) → Pl::CLForm → Common Lisp
                        ↓                ↑                               ↓
              Pl::VarAnnotator    Pl::PExpr → Pl::ExprToCL        cl/pcl-runtime.lisp
             (scopes, captures)   (expression AST → forms)       (Perl semantics in CL)
```

| component | purpose |
|---|---|
| [`Pl/Parser2.pm`](Pl/Parser2.pm) | statement lowering — the one pipeline |
| [`Pl/VarAnnotator.pm`](Pl/VarAnnotator.pm) | scope and capture analysis, lexical renaming |
| [`Pl/PExpr.pm`](Pl/PExpr.pm) | the expression parser (precedence, terms, prototypes) |
| [`Pl/ExprToCL.pm`](Pl/ExprToCL.pm) | expression code generation (forms) |
| [`Pl/CLForm.pm`](Pl/CLForm.pm) | the emitted-form data structure and printer |
| [`Pl/Passes.pm`](Pl/Passes.pm) | the named optimisation registry (`PCL_OPT`) |
| [`cl/pcl-runtime.lisp`](cl/pcl-runtime.lisp) | the runtime: Perl semantics in CL (~17k lines) |
| `cl/pcl-pack.lisp`, `cl/pcl-mro.lisp`, `cl/pcl-warnings.lisp` | extensions written in Perl, compiled by PCL, checked in |
| `lib/` | pure-Perl shims of core/CPAN modules the compiler reads like user code |
| `pl2cl`, `runpcl`, `pcl`, `tools/install-pcl` | the entry points |

The target shape of the compiler and the gap to it:
[`docs/v2-target-architecture.md`](docs/v2-target-architecture.md).

## How it is tested

* **Perl is the oracle.**  `perl-tests/` (perl's `t/op`, `t/base`, … extracted)
  and the full `t/` tree are compiled and run; a differential fuzzer
  (`tools/difftest-*.pl`) evaluates generated expressions in both engines;
  CPAN dists' own suites run through the full pipeline.
* **Baselines move only honestly.**  Every failing row is blessed with its
  cause; a change that breaks a passing row fails the sweep; the sweep also
  reports rows that *vanished* (a file aborting earlier) and statements the
  compiler dropped.
* **Not-supported is declared, not hidden.**  The skip registry
  ([`cl/skip-registry.lisp`](cl/skip-registry.lisp),
  [`docs/test-skip-registry.md`](docs/test-skip-registry.md)) keys on the
  test description, cites the reason, still runs the assertion, and flags
  itself stale the moment a skipped test starts passing.  Crashing tests are
  never auto-skipped.
* **Emission is diffed, not trusted.**  `tools/corpus-diff.pl` and
  `tools/emission-ab.pl` A/B the generated CL over ~1,100 files before a
  change is called done; `tools/drop-census.pl` counts what the compiler
  could not lower.
* The procedure: [`docs/test-debugging-runbook.md`](docs/test-debugging-runbook.md).

## Roadmap

* **v0.1.0 — the first tag** (imminent): the repository's public push, the
  first green CI run (`.github/workflows/ci.yml` installs from scratch and runs
  the gate), then the tag.  What ships is what the tables above measure.
* **v0.2**: the compatibility phase continues — the drop census worked down to
  its explained floor, the remaining perl `t/` families, more of the CPAN
  board.
* **Then, in order:** the speed plan (target 3 above — the boxed-aggregate
  design and the raw-numeric transforms, measured per shape with the bench);
  the IR vocabulary completed (target 2); XS breadth (target 4).
* **Deferred language features — a matter of *when*, not *whether*:** live
  symbol-table hashes (`%Foo::`), richer `caller()` (file/line), perl 5.38
  `class`/`field`/`method`, indirect-object syntax with a scalar invocant, a
  `use warnings` model.  Each has its entry and sketch in
  [`docs/not-supported.md`](docs/not-supported.md).

## Reading guide

| if you want to… | read |
|---|---|
| know what runs and what does not | [`docs/STATUS.md`](docs/STATUS.md), [`docs/not-supported.md`](docs/not-supported.md) |
| translate the generated CL to something else | [`docs/ir-spec.md`](docs/ir-spec.md), then [`docs/generated-cl-ir-review.md`](docs/generated-cl-ir-review.md) |
| understand the speed plan | [`docs/where-the-time-goes.md`](docs/where-the-time-goes.md), [`docs/faster-codegen-suggestions.md`](docs/faster-codegen-suggestions.md) |
| work on XS | [`docs/xs-shim-design.md`](docs/xs-shim-design.md) and the `pclxs` repository |
| add or fix a module shim | [`docs/shipped-modules.md`](docs/shipped-modules.md) |
| debug a failing perl test | [`docs/test-debugging-runbook.md`](docs/test-debugging-runbook.md), [`docs/test-skip-registry.md`](docs/test-skip-registry.md) |
| see how a question was settled | [`docs/DECIDED.md`](docs/DECIDED.md) (one-line index), [`docs/session-log.md`](docs/session-log.md) (history) |
| see PPI bugs PCL works around | [`docs/ppi-upstream-bugs.md`](docs/ppi-upstream-bugs.md) |
| contribute | [`CLAUDE.md`](CLAUDE.md) is the working rulebook (principles, the test cadence, what runs when) |

### Background

The compiler was planned and largely written with Claude (Anthropic's
Fable/Opus models) — the rewrite of the compiler core into the present
single pipeline included; my own Common Lisp is from long ago, so that side
is essentially all Claude.  Two things worth passing on: differential fuzzing
between PCL and perl found real bugs cheaply and is standard compiler
practice; and `pack` was easiest to get right by writing it *in Perl* and
letting PCL compile it — eating our own dog food.  PCL will go on CPAN once it
is closer to ready.

## License

This library is free software; you can redistribute it and/or modify it under
the same terms as Perl itself — dual-licensed under the Artistic License 1.0
or the GNU GPL v1-or-later.  See [`LICENSE`](LICENSE).
