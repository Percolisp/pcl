# PCL status — what runs, what does not

The measured compatibility state.  Every number below comes from a named,
re-runnable measurement; nothing here is estimated.

**Last full re-measure: 2026-09-04** — the gate, the extracted perl suite,
the in-place perl `t/` snapshot, the execution benchmarks
([`faster-codegen-suggestions.md`](faster-codegen-suggestions.md) §0.2i is
the table) and the CPAN board, the bench and CPAN boards on main `bc9aa4a` (generation v2-611, the morning of 2026-09-04); the gate, the sweep, the companion counts and the census re-measured the same afternoon on main `a715608` (generation v2-650, after round 23 landed).

**Contents:** [what runs](#what-runs) · [what deliberately does not](#what-deliberately-does-not-work) · [known sharp edges](#known-sharp-edges) · [XS](#xs)

## What runs

| measurement | result | how to reproduce |
|---|---|---|
| PCL's own regression gate (`Pl/t/`) | **195 files, 6,729 assertions, all passing.**  With the experimental [pclxs](#xs) sibling checked out beside the tree, three more files add 14 XS-bridge rows (198 / 6,743); those 14 currently fail because pclxs is mid-change | `tools/prove-core` (or `prove -j8 Pl/t/`) |
| perl's own test suite, extracted (`perl-tests/`, 108 files from perl 5.40's `t/op`, `t/base`, …) | **18,581 assertions pass / 649 fail (96.6 %)**; **58 files pass completely**; 92 files run to the end, 16 abort part-way, none fails to compile (`state.t`'s one `given` block is a statement-level refusal since round 23, so its other 88 assertions run) | `perl sweep-perl-tests.pl --jobs 8` |
| perl's full `t/` tree, run in place (528 files, perl 5.40.3) | **92 files identical to perl**; **108** differ for a registered, explained reason (`baselines/perl-suite-expected.tsv` — perl-internals probes, threads, taint, …); **275** differ and are the bug queue; 10 do not compile; 9 time out; 31 produce no TAP; 2 are quarantined; 1 is a harness fixture | `tools/run-perl-suite.pl --all --quick --jobs 4` |
| pure-Perl CPAN modules: a 14-distribution board, 183 test files | **78 files PASS / 54 PARTIAL / 51 FAIL; 2,140 assertions ok / 342 not ok** (a PARTIAL file ran most of its suite; FAIL is "zero ok", which also counts the files perl itself skips, such as `*-report-prereqs.t`).  Snapshot [`../baselines/cpan-board14-s467.tsv`](../baselines/cpan-board14-s467.tsv); the previous one (2026-08-23) read 70 / 64 / 49 and 2,053 / 483 — nine files moved DOWN between the two, task #1061 | the command below |
| statements the compiler cannot translate, counted over six populations (the two suites above, the CPAN board, PCL's shipped `lib/`, the examples, the `Pl/t` fixtures) | **62 statements in 19 files**, every one classified with an owning task; zero in PCL's own shipped module tree | `tools/drop-census.pl` vs [`../baselines/parse-error-drop-census-s399.tsv`](../baselines/parse-error-drop-census-s399.tsv) |
| XS bridge conformance corpus (pclxs, real perl as oracle) | 398 pass / 0 fail at the last measurement (2026-08-03); `Digest::MD5`'s own `md5-aaa.t` passed 256/256 under PCL.  Not re-run since — pclxs is under separate development | `tools/pcl-conform` |

Failures are tracked row by row in blessed baselines
(`baselines/fail-baseline.tsv`, `baselines/pass-baseline.tsv`,
`baselines/perl-suite-fails.tsv`, `baselines/row-shortfall.tsv`).  A change
that breaks a previously passing assertion, or that makes a file stop
before rows it used to produce, fails the run — so the numbers above can
only move honestly.

**Untranslatable statements are never silent.**  One the compiler cannot
lower is announced on stderr at compile time
(`PCL: statement dropped at FILE line N: …`) and, when the program reaches
it, dies perl-shaped and trappable (in `eval STRING` the die lands in `$@`).
A deliberately unsupported construct dies the same way, naming its entry in
[`not-supported.md`](not-supported.md).

## What deliberately does not work

The full list with rationale and edge cases is
[`not-supported.md`](not-supported.md) — each entry says *why* and what the
observable difference is.  The big items:

| feature | status |
|---|---|
| XS / compiled C extensions | experimental via the separate [pclxs](#xs) bridge; not part of the core |
| `@_` argument aliasing | args are copies; `$_[0] = 42` does not write back (plain lexical parameters work) |
| `DESTROY` | never called: memory is reclaimed by the garbage collector, there is no scope-exit destructor |
| `tie` on arrays, hashes and filehandles | announced and ignored; scalar `tie` works |
| regex code blocks `(?{…})`, `(??{…})` | not supported (CL-PPCRE has no equivalent); the block is stripped with an announcement |
| `given`/`when`, smart match `~~` | refused with a message (removed in perl 5.42 anyway) |
| `format`/`write` | refused |
| perl 5.38 `class`/`field`/`method` | refused when the feature is provably in use; planned |
| taint mode | not implemented |
| warnings-gated diagnostics | PCL emits no warnings at all; `use warnings` is accepted and inert |
| exact error-message text | not a goal; error *behaviour* (`die`, `$@`, exit status) is |
| unicode identifiers in stashes/globs | partial (task #418 family) |
| indirect object syntax with a SCALAR invocant (`method $obj LIST`) | maybe later; the `new Foo(…)` class-name spellings work, `method $obj …` is dropped loudly |

## Known sharp edges

* **A statement PCL cannot translate dies when reached**, announced at
  compile time.  So a program runs up to the first such statement; the
  census above says how many there are in the test populations.  Two found
  while writing the current README, both filed: under
  `use feature 'signatures'` (so under `use v5.36`) the output-field
  separator `$,` is mis-tokenized (task #1059 — `local $, = …` silently
  binds nothing useful, `$, = …` is dropped loudly), and
  `pl2cl --executable` runs the program at build time and produces a binary
  that does nothing (task #1060).
* **Compile happens at run start.**  A large program pays a transpile and
  SBCL-compile cost on every run (about five seconds for 800 statements);
  module transpiles are cached (`~/.pcl-cache`), as is the runtime itself
  (a saved SBCL core, keyed on the runtime's source).
* **Signatures are read as signatures whenever the feature could be on.**
  A `sub f ($x)` before the pragma is an old-style prototype in perl; PCL
  follows the pragma's region rules but see `not-supported.md`
  "Signature syntax".

## <a name="xs"></a>XS

XS support lives in a separate experimental project (**pclxs** — a
`libperl` shim that lets unmodified XS `.so` files talk to PCL's runtime).
One real module (`Digest::MD5`) has been validated end to end, and the
398-case conformance corpus was green at its last run, but pclxs is **not
bundled** and is currently mid-change (the 14 bridge rows in PCL's gate
fail against its present state).  The core PCL distribution is
pure-Perl-only: any module that needs compiled C fails to load.

The board command (the `--no-dist-lib` flag applies to every dist after it;
Scalar-List-Utils must not put its own unbuilt XS `lib/` on `@INC`):

```
perl tools/cpan-scoreboard.pl --jobs 8 --timeout 120 --tsv baselines/cpan-board14-sNNN.tsv \
  ~/.cpan/build/{Algorithm-Diff-1.201-0,Capture-Tiny-0.50-0,Class-Inspector-1.36-0,Class-Method-Modifiers-2.15-0,Data-Dump-1.25-0,File-Which-1.27-0,Mojo-DOM58-3.002-0,Role-Tiny-2.002004-0,Safe-Isa-1.000010-0,Sort-Versions-1.62-0,Sub-Uplevel-0.2800-0,Text-Balanced-2.07-0,Try-Tiny-0.32-0} \
  --no-dist-lib ~/.cpan/build/Scalar-List-Utils-1.70-0
```
