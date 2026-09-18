# PCL status — what runs, what does not

The measured compatibility state.  Every number below comes from a named,
re-runnable measurement; nothing here is estimated.

**Last full re-measure: 2026-09-18** (main `67781634`, generation v2-1480) —
the gate, the extracted perl suite, the CPAN board, the drop census and the
execution benchmarks ([`faster-codegen-suggestions.md`](faster-codegen-suggestions.md)
§0.2p is the table, taken on a quiet box) on that commit; the in-place perl `t/` counts from the same day's `--all --quick` run on `158be071`, two merges earlier (the later merge changed only the module cache and was re-checked on the 33 files that exercise the search path, zero movers).

**Contents:** [what runs](#what-runs) · [what deliberately does not](#what-deliberately-does-not-work) · [known sharp edges](#known-sharp-edges) · [XS](#xs)

## What runs

| measurement | result | how to reproduce |
|---|---|---|
| PCL's own regression gate (`Pl/t/`) | **246 files, 8,365 assertions, all passing.**  Three of the files are the XS-bridge tests, parked (`plan skip_all`) while the experimental [pclxs](#xs) sibling is mid-change; `PCL_XS_TESTS=1` runs them | `tools/prove-core` (or `prove -j8 Pl/t/`) |
| perl's own test suite, extracted (`perl-tests/`, 108 files from perl 5.40's `t/op`, `t/base`, …) | **18,686 assertions pass / 675 fail (96.5 %)**; **60 files pass completely**; 96 files run to the end, 12 abort part-way, none fails to compile.  (The fail count is higher than the 649 of 2026-09-04 although 105 more assertions pass: files that used to stop early now run further and show rows that always failed) | `perl tools/sweep-perl-tests.pl --jobs 8` |
| perl's full `t/` tree, run in place (528 files, perl 5.40.3) | **107 files identical to perl**; **105** differ for a registered, explained reason (`baselines/perl-suite-expected.tsv` — perl-internals probes, threads, taint, …); **258** differ and are the bug queue; 9 do not compile; 3 time out; 31 produce no TAP; 12 are too slow for the `--quick` form and are listed as NOT-RUN; 2 are quarantined; 1 is a harness fixture | `tools/run-perl-suite.pl --all --quick --jobs 4` |
| pure-Perl CPAN modules: a 14-distribution board, 183 test files | **84 files PASS / 49 PARTIAL / 50 FAIL; 2,273 assertions ok / 339 not ok** on 2026-09-18 (a PARTIAL file ran most of its suite; FAIL is "zero ok", which also counts the seven files perl itself skips — they carry the cause `PERL-SKIP`).  The BLESSED snapshot is still [`../baselines/cpan-board14-s473w.tsv`](../baselines/cpan-board14-s473w.tsv) of 2026-09-09 (84 / 50 / 49; 2,213 / 353, every failing row with a cause); today's run differs from it in six files — three gained rows (Data-Dump `dump.t` to PASS, Safe-Isa `safe_does.t` 6 → 20 rows, Scalar-List-Utils `reftype.t`), Safe-Isa `safe_isa.t` now runs 68 rows instead of 8 and shows one failing row (#1912), `openhan.t` lost two rows that had been passing on nothing (#1571), and Text-Balanced `05_extmul.t` exhausts the 1 GB heap and produces no rows (#1512) — and attributing those movers and re-blessing is task #1913.  The ROW-level file [`../baselines/cpan-board14-fails.tsv`](../baselines/cpan-board14-fails.tsv) is the board's answer to the sweep's `fail-baseline.tsv`: one line per failing assertion with its got/expected and its cause, compared by `tools/cpan-scoreboard.pl --diff` (task #1502) | the command below |
| statements the compiler cannot translate, counted over six populations (the two suites above, the CPAN board, PCL's shipped `lib/`, the examples, the `Pl/t` fixtures) | **62 statements in 19 files** (re-counted 2026-09-18, row for row the blessed census), every one classified with an owning task; zero in PCL's own shipped module tree | `tools/drop-census.pl` vs [`../baselines/parse-error-drop-census-s399.tsv`](../baselines/parse-error-drop-census-s399.tsv) |
| XS bridge conformance corpus (pclxs, real perl as oracle) | 398 pass / 0 fail at the last measurement (2026-08-03); `Digest::MD5`'s own `md5-aaa.t` passed 256/256 under PCL.  Not re-run since — pclxs is under separate development | `tools/pcl-conform` |

Failures are tracked row by row in blessed baselines
(`baselines/fail-baseline.tsv`, `baselines/pass-baseline.tsv`,
`baselines/perl-suite-fails.tsv`, `baselines/row-shortfall.tsv`).  A change
that breaks a previously passing assertion, or that makes a file stop
before rows it used to produce, fails the run — so the numbers above can
only move honestly.

**And every blessed row carries a CAUSE, so the failures can be split into
what is a bug and what is the deliberate edge of the language PCL
implements.**  One rule, six classes, one command
(`tools/cause-census.pl --markdown`, rule and reconciliation in
[`failure-cause-classes.md`](failure-cause-classes.md)) — measured
2026-09-18 against the sweep of 2026-09-18 on `67781634`:

| population | rows | not-supported | parked | bug | other | unexplained | perl-skip | not-supported + parked |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| perl-tests sweep | 670 | 319 | 69 | 275 | 7 | 0 | 0 | 57.9% |
| companion (perl's own t/) | 11,736 | 5,464 | 88 | 5,440 | 3 | 741 | 0 | 47.3% |
| CPAN board (14 dists) | 389 | 203 | 0 | 179 | 0 | 0 | 7 | 53.1% |
| companion XDIFF rows | 2,516 | 2,515 | 0 | 0 | 0 | 1 | 0 | 100.0% |
| shortfall: perl-tests | 12,213 | 267 | 0 | 104 | 0 | 11,842 | 0 | 2.2% |
| shortfall: perl's t/ | 443,859 | 55,432 | 417 | 387,706 | 0 | 304 | 0 | 12.6% |
| **all populations** | **471,383** | **64,200** | **574** | **393,704** | **10** | **12,888** | **7** | **13.7%** |

`not-supported` = the cause names a [`not-supported.md`](not-supported.md)
section; `parked` = a scheduling decision; `bug` = a filed task; `perl-skip`
= perl skips the file too (excluded from the share's denominator);
`unexplained` = not yet attributed — the audit's own queue, counted on every
run so it cannot grow unnoticed.  **Read the per-population rows, not the
total**: the total is dominated by the `t/` shortfall's few enormous
generated files.  The `perl-tests` sweep is the population PCL is measured
against on every change, and **58 % of its failing rows are deliberate
non-support or parked, not bugs**.  Rows the skip registry relabels as
skips are not-supported failures too, and they sit OUTSIDE the sweep row:
198 rows in 24 files on the same sweep (the `(198 by the registry)` in its
TOTAL line).  Counting them, the sweep's honest not-supported share is
(319 + 69 + 198) / (670 + 198) = 67.5 %.

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
* **Compile happens on the first run after an edit.**  A large program pays
  its transpile and SBCL-compile cost once (about six seconds for 1,200
  lines) and then starts from its cache entry in 0.04 s; module transpiles
  are cached the same way (`~/.pcl-cache`), as is the runtime itself (a
  saved SBCL core, keyed on the runtime's source).  `pcl -e` one-liners are
  not cached.  See
  [`caching.md`](caching.md) for what is cached, where, and how to clear
  or disable it.
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
