# PCL Status — what runs, what doesn't

Measured compatibility state.  Every number below comes from a named,
re-runnable measurement; nothing is estimated.  Last full re-measure:
2026-08-23 (internal session s440, on the v0.1.0 tree).

## What runs

| measurement | result | how to reproduce |
|---|---|---|
| PCL's own regression gate (`Pl/t/`) | **171 files, 5,846 assertions, all passing** (the 14 XS-bridge rows run only with the experimental [pclxs](#xs) sibling built) | `tools/prove-core` (or `prove -j8 Pl/t/`) |
| Perl's own test suite, extracted (`perl-tests/`, 108 files from perl 5.x `t/op`, `t/base`, …) | **18,313 assertions pass / 893 fail (95.4 %)**; **61 files pass completely** | `perl sweep-perl-tests.pl --jobs 8` |
| Perl's full `t/` tree, run in place (528 files) | run per-directory as a bug-finder; verdicts tracked per file against blessed baselines | `tools/run-perl-suite.pl --all --quick --jobs 4` |
| XS bridge conformance corpus (pclxs, 398 cases, real perl as oracle) | **398 pass / 0 fail**; `Digest::MD5`'s own `md5-aaa.t` passes 256/256 under PCL | `tools/pcl-conform` |
| Pure-Perl CPAN modules (183-dist test board) | 65 PASS / 65 PARTIAL / 53 FAIL (dist granularity; a PARTIAL runs most of its suite) | internal board (`cpan-board14-*.tsv` snapshots); [`cpan-module-log.md`](cpan-module-log.md) records per-dist causes |

Failures are tracked row-by-row in blessed baselines
(`docs/fail-baseline.tsv`, `docs/pass-baseline.tsv`) — a change that breaks
a previously-passing assertion fails the sweep, so the numbers above can
only move honestly.

A statement the compiler cannot translate is **announced on stderr at
compile time** (`PCL: statement dropped at FILE line N: …`) **and dies, perl-shaped
and trappable, when the program reaches it** (since s435; in `eval STRING` the
die lands in `$@`).  These are counted over six test populations: 73 files
carry 167 such statements (s440; the two perl-test populations: 24 files / 75),
every one classified with an owning task (`docs/parse-error-drop-census-s399.tsv`,
`docs/drop-census-s419-flip-gate.md`).  Zero drops in PCL's own shipped
module tree.

## What deliberately does not work

The full list with rationale and edge cases is
[`not-supported.md`](not-supported.md) — each entry says *why* and what the
observable difference is.  The big items:

| feature | status |
|---|---|
| XS / compiled C extensions | experimental via the separate [pclxs](#xs) bridge; not part of the core |
| `@_` argument aliasing | args are copies; `$_[0] = 42` does not write back (plain scalars work) |
| `tie` on arrays / hashes | announced, not silent; scalar `tie` works |
| Regex code blocks `(?{…})`, `(??{…})` | not supported (CL-PPCRE has no equivalent) |
| `given`/`when`, smart match `~~` | refused with a clear message (removed in perl 5.42 anyway) |
| `format`/`write` | not supported |
| Perl 5.38 `class`/`field`/`method` | refused when the feature is provably in use; planned |
| taint mode | not implemented |
| warnings-gated diagnostics | PCL emits no warnings-category diagnostics yet |
| exact error-message text | not a goal; error *behavior* (die/`$@`) is |
| `DESTROY` at GC time | no deterministic finalizer timing on a GC'd host |
| unicode identifiers in stashes/globs | not yet (tracked, task #410) |
| indirect object syntax with a SCALAR invocant (`method $obj LIST`) | maybe later (USER, s425) — the `new Foo(…)` / `new Foo` class-name spellings work; `method $obj …` is dropped loudly; see `docs/not-supported.md` |

## Known sharp edges

* **A statement PCL cannot translate dies when reached** (announced at compile
  time, perl-shaped and trappable at run time — the s435 "flip"; `eval STRING`
  gets it in `$@`).  So a program runs up to the first such statement; the
  census above says how many there are in the test populations.
* **Compile happens at run start** (or at install for the runtime): a large
  program pays a transpile+compile cost on first run; module transpiles are
  cached (`~/.pcl-cache`).

## <a name="xs"></a>XS

XS support lives in a separate experimental project (**pclxs** — a
`libperl` shim that lets unmodified XS `.so` files talk to PCL's runtime).
One real module (`Digest::MD5`) is validated end-to-end, and the 398-case
conformance corpus is green, but pclxs is **not bundled**: the supported
surface of a random CPAN XS dist is still narrow.  The core PCL
distribution is pure-Perl-only.
