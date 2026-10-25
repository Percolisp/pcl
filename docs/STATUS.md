# PCL status: what runs, what does not

This page gives the measured state of PCL's compatibility with perl. Every
number on it comes from a command you can run yourself (the last column of
each table), and nothing is estimated.

**Each number carries its own date.** The regression suite, the extracted
perl tests, the CPAN board and the untranslatable-statement count were
measured on 2026-10-17; the run of perl's full `t/` tree is from
2026-09-18 (the full `t/` figures are refreshed at each release); the
failure causes were re-counted on 2026-10-01 against that day's sweep and
the blessed baselines of the other populations. The speed numbers are in
[`faster-codegen-suggestions.md`](faster-codegen-suggestions.md).

**Contents:** [what runs](#what-runs) · [what deliberately does not](#what-deliberately-does-not-work) · [known sharp edges](#known-sharp-edges) · [speed](#speed) · [XS](#xs)

## What runs

| measurement | result | how to reproduce |
|---|---|---|
| PCL's own regression suite (`Pl/t/`) | **337 files, 11,466 assertions, all passing** (2026-10-25; PCL now parses with its own vendored PPI, `vendor/PPI`, upstream master plus 12 tokenizer fixes made at the source). Three of the files test the [XS bridge](#xs) and are parked (`plan skip_all`) while that project is being reworked; `PCL_XS_TESTS=1` runs them | `tools/prove-core` (or `prove -j8 Pl/t/`) |
| perl's own tests, extracted (`perl-tests/`: 108 files from perl 5.40's `t/op`, `t/base` and others) | **18,761 assertions pass, 626 fail (96.8 %)** (2026-10-24: a shift's left operand is read as a UV, so bop.t's two negative-shift rows pass; the stash-delete row is back: `delete $Pkg::{x}` now removes the symbol from the package's view). 60 files pass completely; 98 run to the end, 10 stop part-way, and all 108 compile. | `perl tools/sweep-perl-tests.pl --jobs 8` |
| perl's full `t/` tree, run in place (528 files, perl 5.40.3) | **107 files identical to perl** (2026-09-18). 105 differ for a registered, explained reason (probes of perl's internals, threads, taint and so on; listed in `baselines/perl-suite-expected.tsv`); 258 differ and are the bug queue; 9 do not compile; 3 time out; 31 produce no test output; 12 are too slow for the `--quick` form and are listed as not run; 2 are quarantined; 1 is a harness fixture | `tools/run-perl-suite.pl --all --quick --jobs 4` |
| pure-Perl CPAN distributions: 26 of them, 582 test files (six joined 2026-10-24: TimeDate, HTTP-Date, URI, HTTP-Message, YAML-Tiny, Data-Dumper; IO-Compress joined 2026-10-18; five joined 2026-10-11: Text-CSV, Getopt-Long, JSON-PP, HTTP-Tiny, Path-Tiny) | **362 files pass, 136 partly pass, 84 fail; 69,476 assertions ok, 1,524 not ok** (2026-10-24, the widened board on the s526w1 tree: the 20 older distributions read the round-4 board's 277 / 107 / 65 and 66,729 exactly, the six new ones 85 / 29 / 19 with 2,747 ok, every failing row blessed with its cause.  On 2026-10-20 the round-3 board: the sigil-exact capture promoter let Text-CSV's 20 test files run for the first time -- 25,119 of the new assertions are its 55_combi.t alone -- and Path-Tiny, Mojo-DOM58 and IO-Compress gained files; the 908 new not-ok rows of Scalar-List-Utils exotic_names.t are one shim gap, set_subname.  On 2026-10-18 the round-2 board read 232 / 128 / 89 and 28,489 ok; on 2026-10-17 the 19-distribution board read 180 / 104 / 72 and 8,909 ok -- the day's four merges let Capture-Tiny go from 5,648 to 10,256 passing assertions and IO-Compress from 1,855 to 11,568: `PerlIO::get_layers` now exists, `\Q` is applied where a regex literal is compiled, a loop's embedded `my` is fresh per iteration, a re-blessed glob copy reaches the glob).  Counted under the runner's contract of 2026-10-13: a test file runs from its distribution's directory, as `prove` runs it, and counts as passing only when its whole plan ran. A partial file ran most of its suite; a failing file has zero passing assertions, and that count includes files perl itself skips. Every failing assertion, with its cause, is in [`../baselines/cpan-board14-fails.tsv`](../baselines/cpan-board14-fails.tsv); the review of those causes with the fix plan is [`cpan-board-failure-review-s521.md`](cpan-board-failure-review-s521.md). The per-file snapshot is [`../baselines/cpan-board-s521r2.tsv`](../baselines/cpan-board-s521r2.tsv) | the [board command](#the-cpan-board-command) below |
| statements the compiler cannot translate, counted over six populations (the two perl test sets above, the CPAN board, PCL's shipped `lib/`, the examples and the regression-suite fixtures) | **61 statements in 19 files** (2026-10-16), each classified with its cause; none in PCL's own shipped modules | `tools/drop-census.pl`, compared against [`../baselines/parse-error-drop-census-s399.tsv`](../baselines/parse-error-drop-census-s399.tsv) |
| XS bridge conformance corpus (real perl as the oracle) | 398 pass, 0 fail at its last run (2026-08-03), and `Digest::MD5`'s own `md5-aaa.t` passed 256 of 256 under PCL. Not re-run since: the bridge is being reworked separately | `tools/pcl-conform` |

**The numbers can only move honestly.** Failures are tracked assertion by
assertion in blessed baselines (`baselines/fail-baseline.tsv`,
`baselines/pass-baseline.tsv`, `baselines/perl-suite-fails.tsv`,
`baselines/row-shortfall.tsv`). A change that breaks a passing assertion,
or makes a file stop before rows it used to produce, fails the run.

### Why the failures fail

Every blessed failing row carries a cause, so the failures split into bugs
and the deliberate edges of the language PCL implements. The rule and the
reconciliation are in [`failure-cause-classes.md`](failure-cause-classes.md);
the command is `tools/cause-census.pl --markdown`. Measured 2026-10-01
(the sweep rows against that day's run; the other populations against their
blessed baselines, which are edited row by row as failures are fixed):

| population | rows | not-supported | parked | bug | other | unexplained | perl-skip | not-supported + parked |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| perl-tests sweep | 654 | 315 | 68 | 267 | 4 | 0 | 0 | 58.6% |
| companion (perl's own t/) | 11,620 | 5,884 | 88 | 5,541 | 2 | 105 | 0 | 51.4% |
| CPAN board (14 dists) | 389 | 203 | 0 | 179 | 0 | 0 | 7 | 53.1% |
| companion XDIFF rows | 2,423 | 2,421 | 0 | 0 | 0 | 2 | 0 | 99.9% |
| shortfall: perl-tests | 12,208 | 265 | 0 | 103 | 0 | 11,840 | 0 | 2.2% |
| shortfall: perl's t/ | 444,035 | 55,618 | 417 | 387,997 | 0 | 3 | 0 | 12.6% |
| **all populations** | **471,329** | **64,706** | **573** | **394,087** | **6** | **11,950** | **7** | **13.9%** |

The classes: `not-supported` means the cause names a section of
[`not-supported.md`](not-supported.md); `parked` is a scheduling decision;
`bug` is a filed bug; `perl-skip` means perl skips the file too (left out of
the share's denominator); `unexplained` is not attributed yet, and is counted
on every run so it cannot grow unnoticed. A "shortfall" row is an assertion
the file should have produced and did not, because the file stopped early.

Read the per-population rows, not the total: a few enormous generated files
in perl's `t/` tree dominate the total. The `perl-tests` sweep is the
population every change is measured against, and **59 % of its failing rows
are deliberate non-support or parked, not bugs.** The skip registry relabels
another 191 rows in 23 files as skips; they are not-supported failures too,
but sit outside that row. Counting them, the sweep's not-supported share is
(315 + 68 + 191) / (654 + 191) = 67.9 %.

### Untranslatable statements are never silent

A statement the compiler cannot translate is announced on stderr at compile
time (`PCL: statement dropped at FILE line N: …`). If the program reaches it,
it dies with a normal Perl exception that `eval` can catch (inside
`eval STRING`, the error lands in `$@`). A construct that is deliberately
unsupported dies the same way, naming its entry in
[`not-supported.md`](not-supported.md).

## What deliberately does not work

The full list, with the reason and the observable difference for each entry,
is [`not-supported.md`](not-supported.md). The main items:

| feature | status |
|---|---|
| XS / compiled C extensions | experimental, through the separate [XS bridge](#xs); not part of PCL itself. One exception: `Compress::Raw::Zlib` is provided over the system zlib library, as a temporary route until the bridge supplies the module (see [XS](#xs)) |
| `@_` argument aliasing | `$_[0] = 42` changes the caller's variable, array element or hash element when PCL sees the sub as it compiles the call; for a method, a code reference or a sub in another file, a plain scalar argument is copied in and written back after the call. An element reached through a reference (`f($r->{k})`) still gets a copy. Also not covered: the same variable passed twice, a kept reference to `$_[0]` written later, a write followed by `die`, a closure reading the variable mid-call, and a plain-string invocant (see "`@_` argument aliasing" in [`not-supported.md`](not-supported.md)) |
| `DESTROY` | never called: memory is reclaimed by the garbage collector, and there is no scope-exit destructor. For now, close filehandles explicitly |
| regex code blocks `(?{…})`, `(??{…})` | removed from the pattern with a warning at compile time (cl-ppcre has no equivalent); the rest of the match runs |
| `given`/`when`, smart match `~~` | refused with a message (removed in perl 5.42 anyway) |
| `format`/`write` | refused with a message |
| perl 5.38 `class`/`field`/`method` | refused when the feature is provably in use; planned |
| taint mode | not supported |
| warnings | PCL emits no warnings-gated diagnostics; `use warnings` is accepted and has no effect |
| exact error-message text | not a goal; error *behaviour* (`die`, `$@`, exit status) is |
| indirect object syntax with a scalar invocant (`method $obj LIST`) | maybe later; `new Foo(...)` with a class name works, `method $obj ...` is dropped with a message |

## Known sharp edges

* **A wrong answer is usually silent, so check *your* program.** Most
  programs PCL gets wrong still exit 0 with nothing on stderr.
  `pcl --check prog.pl ARGS` runs the program under perl and under PCL and
  prints `IDENTICAL`, or the first line where they differ
  ([`pcl-check.md`](pcl-check.md)); its output is most of a good bug report
  ([`CONTRIBUTING.md`](../CONTRIBUTING.md)). It runs the program twice, so do
  not use it on a program whose side effects must happen only once.
* **A statement PCL cannot translate dies when reached**, and is announced
  at compile time, so a program runs up to the first such statement. The
  table above counts how many there are in the test populations. One known
  case (checked 2026-09-27): under `use feature 'signatures'`, and so under
  `use v5.36` and later, the output field separator `$,` is misread.
  `$, = "+"` is dropped with a message, and `local $, = "-"` silently has no
  effect.
* **The first run after an edit compiles.** A large program pays its
  compile once (about six seconds for 1,200 lines) and then starts from its
  cache entry in about 0.04 seconds. Modules and the runtime itself are
  cached the same way, under `~/.pcl-cache`; `pcl -e` one-liners, and a
  script run with a source-changing switch (`-n`, `-M`, `-l`, ...), are not.
  [`caching.md`](caching.md) says what is cached, where, and how to clear or
  disable it.
* **`pcl` takes perl's command-line switches** (`-lane`, `-pi.bak`,
  `-0777`, `-F:`, `-s`, `-x`, `-E`, `-C`, `-w` ...) by perl's rules, and a
  script's own `#!perl -SWITCHES` line; taint (`-T`) is accepted but not
  applied, and says so; the debugger (`-d`) is refused
  ([`pcl-commands.md`](pcl-commands.md)).
* **Signatures are read as signatures whenever the feature could be on.**
  In perl, a `sub f ($x)` before the pragma is an old-style prototype. PCL
  follows the pragma's region rules; see "Signature syntax" in
  [`not-supported.md`](not-supported.md).

## Speed

The speed table in the README measures the work a program does, with
start-up subtracted; every measurement behind it is in
[`faster-codegen-suggestions.md`](faster-codegen-suggestions.md). For a
short, ordinary program, start-up is most of the time, and **start-up has
deliberately not been optimized yet.** Measured 2026-09-27, over 101
ordinary programs whose output matches perl's:

* **A warm run** (the program already compiled and cached) takes 48 ms at
  the median, about ten times perl's time; a one-line program takes 43 ms
  under PCL and 1.4 ms under perl. About 40 ms of a typical run is fixed
  cost, and about 34 ms of that is the `pcl` launcher: a Perl script that,
  on every run, loads its modules, starts `sbcl --version`, and hashes the
  1.5 MB runtime source to find the right saved core. SBCL itself, booting
  PCL's saved core, takes 3 to 7 ms.
* **The first run after an edit** runs the program from its transpiled
  text, then builds the program's compiled file after the program has
  ended, in the same process, using the modules it already loaded (their
  own compiled files when they exist). Measured 2026-10-07 on a quiet box:
  a script using Getopt::Long takes 0.22 seconds on its first run (3.6
  before), a 60-line script 0.56 (1.5 before); a 1026-line script
  using List::Util and POSIX takes 7.1 seconds (6.6 before). Every later
  run takes about 0.05 seconds.
* **Code created by string `eval`** is compiled by SBCL's full compiler
  every time it runs. The common case is a Moo class's set-up: one small
  program that defines a Moo class spends 2.7 seconds a run, most of it
  here.

Only about 5 % of these programs spend most of their time running their
own code. The three costs above are planned as **later extensions for
faster start-up**:

* a faster launcher: a native one, or one that remembers the saved core's
  name and hashes the runtime only when the runtime file has changed;
* a cheaper first compile of a large script;
* a cheaper compile for code created by string `eval`, and cached compiled
  `eval` strings.

## <a name="xs"></a>XS

XS support lives in a separate experimental project, **pclxs**: a `libperl`
shim that lets unmodified XS `.so` files talk to PCL's runtime. One real
module (`Digest::MD5`) has been validated end to end, and the 398-case
conformance corpus passed at its last run (2026-08-03). pclxs is **not
bundled**, and it is being reworked: PCL's three bridge test files are
parked until it works again. PCL itself is pure Perl only, so a module that
needs compiled C fails to load, with one exception: since 2026-10-17
`Compress::Raw::Zlib` is a Perl shim over the **system zlib library**, called
through SBCL's foreign-function interface (`libz.so.1` on Linux, `libz.dylib`
on macOS). `Compress::Zlib`, `IO::Compress::Gzip`, `IO::Uncompress::Gunzip`,
`IO::Zlib` and `Archive::Tar`'s compressed archives work and produce zlib's
own bytes; a machine without libz gets a one-line error at load. The route
is a stopgap until pclxs runs the real module; what it does not cover (zip
append scanning, `-LimitOutput` chunk sizes, the memory of a stream that is
abandoned before it ends) is listed under "Compress::Raw::Zlib" in
[`not-supported.md`](not-supported.md).

## The CPAN board command

The board's distributions are listed in
[`../baselines/cpan-board-dists.tsv`](../baselines/cpan-board-dists.tsv)
(19 since 2026-10-11; how a distribution joins is in
[`cpan-release-plan.md`](cpan-release-plan.md)).  One command runs them all,
passing Scalar-List-Utils with `--no-dist-lib` (its own unbuilt XS `lib/` must
not shadow PCL's shim):

```
tools/cpan-board.sh . /tmp/board      # writes /tmp/board.tsv and /tmp/board.rows.tsv
tools/cpan-scoreboard.pl --diff baselines/cpan-board14-fails.tsv /tmp/board.rows.tsv
```
