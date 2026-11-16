# The CPAN board's RUN PHASE, perl vs PCL (s541a, 2026-11-16)

The USER's question (s540, paraphrased): the first measurement (#4787,
`docs/cpan-speed-comparison-s540.md`) found that the board's 33x is compile,
not run -- so measure just the run time.  With everything transpiled, compiled
and loaded before the clock starts, how much slower (or faster) than perl does
a real CPAN test file RUN under PCL, in seconds, and what are the things behind
the difference?  (Task #4791.)  A MEASUREMENT: nothing under `Pl/`, `cl/` or
`lib/` changed.  Every number in the tables is of main's code sha
**`c1fe4930`** (MAIN-READY-97, generation v2-14900), measured on an extraction
of it (`scratch/s541a/tree-c1fe4930/` in the s541a worktree) with its own core
and module cache.

## 0. The answer

**With everything compiled and loaded before the clock starts, PCL RUNS the
board 4.2x slower than perl: 137.7 s of CPU against 32.4 s, +105.3 s, over
the 659 files PCL runs exactly as perl does** (same plan, same ok count; quiet
box, best of 3).  The first measurement's 33.2x (whole file, transpile and load
included) is 4.2x for the run alone.  Of PCL's 137.7 s:

* **40.5 s (29 %) are LATE compiles -- compile work that happens at run time**:
  32.5 s are STRING EVALS (8,905 evals over 207 files of 23 distributions: 28.1 s
  their SBCL compile, 4.4 s their transpile when the eval cache misses) and
  8.0 s are modules first `require`d at run time (perl: 2.0 s for the same
  requires).  Perl compiles a string eval in microseconds; PCL compiles each one
  natively, every execution.  Take the late compiles out of both sides and the
  run is **97.3 s against 30.4 s, 3.2x**.
* **1.1 s are SBCL compiling CLOS constructors at run time** (cl-ppcre's regex
  node classes, on a program's first regex of a shape) -- a cost perl does not
  have and the core could pre-pay.
* **96.1 s are the programs' own work** against perl's 30.4 s (3.2x), GC
  included (6.8 s over the whole run).  Most of it is in three distributions:
  JSON-PP's `099_binary*` round trips (46 s of excess, a spread profile: boxes,
  EQUAL-table lookups, per-character allocation), Capture-Tiny (11.6 s: the stash
  view `Symbol::gensym` materialises, and `fork`), and YAML-PP (3.4 s: an
  `s///` with an interpolated pattern re-translated on every call, and the
  long-string prefix search).

The distribution of ratios (PCL run / perl run, per file): **58 files under
1x, 71 at 1-2x, 205 at 2-5x, 171 at 5-10x, 150 over 10x**.  The files over 10x
are short runs dominated by a late compile (a median perl run is 2.5 ms, PCL's
13.4 ms); the seconds are in the 2-5x bucket (+71.7 s of the +105.3 s:
JSON-PP, Capture-Tiny, YAML-PP).  Subprocesses are not in these numbers:
the children the programs start cost perl 11.5 s and PCL 30.7 s of CPU
(Capture-Tiny's forks, #4965).

The things behind the difference, ranked by board seconds, are section 4: the
string-eval compile (28 s; #2421 / #1410 / #2191), JSON::PP's spread (#4967),
Capture-Tiny's stash view and fork (#4964, #4965), run-time module loads
(#5065), eval transpile misses (#5062, with a correctness bug behind part of
them, #5061), the interpolated `s///` (#5063), the long-string search (#4966)
and the CLOS constructors (#5064).

## 1. How it was measured

**The instrument is `tools/board-timing.pl --run`** (s540a's tool, extended;
its positive control `tools/t/board-timing.t`, 32 rows).  Every board file runs
as the board runs it (cwd = the dist root, the same `-I`, `PERL5LIB` and
timeout table), with the module cache ON in a private directory per
distribution, WARM: a coarse pass ran every file cold and then warm, so every
module the file loads -- at compile time or at run time -- has its fasl.

**PCL's clock** (the hooks fasl the tool generates; nothing in `cl/`) starts
where perl's INIT phase starts -- after the main program's CHECK blocks
(`%p-drain-compile-blocks` inside the main `p-run-compile-phase-blocks`) -- and
stops at the last exit hook, after the END phase.  Every bucket (a runtime
entry point wrapped with `sb-int:encapsulate`) keeps wall, CPU, the CPU of the
children reaped inside it, GC time and bytes consed, EXCLUSIVE of nested
buckets, and the tables are snapshotted at the boundary, so a bucket's share of
the run is total minus snapshot.  Inside the window:

* **subtracted -- load work the board's text path still does there**: reading
  the program's own forms (`sread`, the reader inside SBCL's `load`) and
  compiling them (`scompile`: a native compile requested by SBCL's top-level
  eval).  Perl did both before INIT.  What the subtraction leaves behind was
  measured: ~9 us per top-level form (section 1a).
* **kept in the run, shown as a column -- LATE load and compile**: a module
  first `require`d at run time (its fasl load and its top-level code), a string
  eval's transpile (through the `pl2cl` server: counted by WALL, the server
  being a child nobody reaps until exit) and its compile.  It is run time: perl
  pays its own late compiles inside its run too (#2421: for the program an
  eval's compile counts).  The reader who wants run minus compile subtracts it.
* **kept in the run, shown as a column -- SBCL's own run-time compiles**
  (bucket `rcompile`, new): CLOS (SB-PCL) compiling an optimized constructor or
  a dispatch function the first time a class is instantiated or a generic
  function called -- cl-ppcre's regex node classes on a program's first regex
  of a shape.  Found while building the instrument; s540a's buckets had counted
  these as the script's own compile.
* **listed apart, never in the run number -- subprocesses**: the CPU of the
  children the program reaps in the window (`getrusage(RUSAGE_CHILDREN)`),
  minus the late transpiles.

**THE PCL RUN NUMBER** (`pr_cpu`) = SBCL's own CPU in the window - the load
work + the late transpiles' outside CPU.  Wall beside it (`pr_wall`; it holds
the waits on children).

**perl's clock** is a helper module loaded with `-M` before the program
compiles (`PCLBTRun`, written by the tool): its INIT block is the first INIT
block to run (INIT is FIFO) and its END block the last END block (END is LIFO);
it records `Time::HiRes` wall and `CLOCK_PROCESS_CPUTIME_ID` CPU at both, and
the reaped children's CPU (`times`).  It takes its own directory out of `@INC`
again, and a forked child does not write.  Perl's late `require`s are timed in
ONE extra run per file (`--perl-late`: `CORE::GLOBAL::require` from BEGIN, the
outermost run-time require only), never in the timed runs, because the
override adds a frame and a call to every require.  Perl's string-eval compile
cannot be hooked: it stays inside perl's run number (microseconds an eval).

**Cross-check of perl's clock** (the brief: INIT/END or `perl -c`, show they
agree): every rep of the precise pass also timed `perl -c`, so each file has
(whole file - `perl -c`) beside INIT..END -- section 1b.

**Which files are compared.**  Class `C` only: PCL ran its whole plan with no
failing row and the same ok count as perl, which ran its plan too (s540a's
definition, unchanged); a rep that is not `C` is not used.

**The numbers are a QUIET pass**: `heavy.sh s541a bench` legs (the whole box,
started at a 1-minute load < 2, each under ten minutes), best of 3 per file,
perl and PCL interleaved per rep, the spread of each side beside it.  The
coarse pass (bounded legs at load 2-18) only ranked the files and built the
caches; no number below comes from it.

**The population.**  Of the board's 896 files, **659 are compared** (s540a:
638 -- the harness fixes merged since moved files into the class), 7
partial-equal, 67 skipped by both, 163 excluded (a PCL failure or a timeout):
the coarse pass's classes; every precise rep of the 659 was class `C` again.

**Where the data is** (the s541a worktree, `scratch/s541a/`, not committed;
every leg log ends in its `=== heavy.sh s541a ... END rc=0` line): the precise
legs `legs/precise-a-01.log` .. `a-03.log`, `legs/precise-b-01.log` and their
TSVs `precise-c1fe4930-*.tsv` (each leg under eight minutes, 02:37-03:17); the
list's measurements `legs/listbench.log` (03:27-03:30: the micro programs
`micro/m-*.pl`, the cl-ppcre probe, the eval-policy A/B
`ab-evalpolicy-{base,cheap}.tsv`, the fasl method check `calib2-fasl.tsv`); the
coarse pass `legs/coarse-g{1..4}.log`; the profiles `legs/prof-profA.log`,
`prof-profB.log`, `prof-c1fe4930/*.prof`.  The tables are `tools/board-timing.pl
--report --run` over the precise TSVs (`--table` for the `.tsv`).

### 1a. What the subtraction leaves behind (PCL's clock checked)

The board runs a test file from its transpiled TEXT, so the program's own forms
after the boundary are read and compiled inside the window; the tool takes
that out by bucket.  Two checks that what is left is the run:

* **The same 13 files (13 distributions) run from a FASL** (`--fasl`: the
  program built by `pcl`'s own first-run path, so nothing of it is read or
  compiled in the window), best of 3, in the same quiet leg family
  (`scratch/s541a/legs/listbench.log` part 3 against the precise pass):
  **8,647.7 ms from the text, 8,715.8 ms from the fasl (+0.8 %)**, every file
  within 0-25 ms, and the MB consed in the window are the same on both paths
  (14.0 / 14.1, 3,775.7 / 3,775.8, 1,224.1 / 1,224.4 ...) -- the subtraction
  isolates the same work.  The fasl path is the slightly SLOWER one: its
  window holds the fasl loader's work for the run forms (code objects, fixups),
  which is why the text path is the one used.
* **A program that does nothing** (an early control on `d2bc17d4`, method
  only: 300 top-level `$x = N;` statements) leaves 2.7 ms in the text path's
  window -- **~9 us per top-level form** of bookkeeping (SBCL's top-level eval,
  the buckets' own clock reads) -- and 0.5 ms in the fasl path.  A loop of
  200,000 additions reads the same on both (2.9 / 3.0 ms; perl 3.3 ms).

**Found while building it: SBCL compiles at RUN time on its own.**  The first
`make-instance` of a CLOS class with constant initargs compiles an optimized
constructor (`sb-pcl::install-optimized-constructor` -> `pcl-compile`); cl-ppcre
builds its regex parse tree from CLOS node classes, so a program's first regex
of each shape pays a native compile of ~0.7-1.3 ms (bucket `rcompile`, 1.13 s
over the board, section 4 row 9).  The s540a buckets counted these as the
script's own compile (load); here they are run time.

### 1b. perl's clock: INIT/END against the whole file minus `perl -c`

Every precise rep also timed `perl -c`; walls, best of the reps, the ten files
perl spends longest in:

| file | whole | `perl -c` | whole - (-c) | INIT..END | difference | INIT..END spread |
|---|---:|---:|---:|---:|---:|---:|
| Log-Log4perl/017Watch.t | 14,070.0 | 50.0 | 14,020.0 | 14,016.4 | 3.6 | 12 % |
| IO-Compress/105oneshot-zip-only.t | 9,076.0 | 44.4 | 9,031.6 | 9,029.6 | 2.0 | 11 % |
| IO-Compress/105oneshot-gzip-only.t | 9,057.0 | 42.6 | 9,014.4 | 9,010.4 | 4.1 | 17 % |
| Template-Toolkit/provider.t | 4,069.0 | 39.3 | 4,029.7 | 4,026.1 | 3.6 | 12 % |
| JSON-PP/099_binary15.t | 2,403.0 | 11.4 | 2,391.6 | 2,386.7 | 4.9 | 1 % |
| JSON-PP/099_binary14.t | 2,213.0 | 11.5 | 2,201.4 | 2,198.0 | 3.4 | 2 % |
| JSON-PP/099_binary13.t | 2,050.0 | 11.2 | 2,038.8 | 2,035.2 | 3.6 | 4 % |
| JSON-PP/099_binary12.t | 1,923.0 | 10.3 | 1,912.7 | 1,907.5 | 5.2 | 0 % |
| JSON-PP/099_binary11.t | 1,789.0 | 11.4 | 1,777.6 | 1,773.1 | 4.5 | 1 % |
| JSON-PP/099_binary10.t | 1,618.0 | 11.0 | 1,607.0 | 1,604.5 | 2.5 | 13 % |

(milliseconds.)  Over all 659 files, (whole - `perl -c`) - INIT..END is a
median 0.5 ms (p10 -0.6, p90 2.2), and the sums are 82.44 s against 81.95 s
(0.6 %): the two agree within the spread, the remainder being perl's start,
global destruction and exit, which the INIT/END clock leaves out by design.
The INIT/END clock is the one used.

## 2. Per distribution

Run-phase CPU seconds over the COMPARED files (precise pass); `other` = the dist's files that are not compared
(partial-equal, both-skip or excluded, by the coarse pass's classes); `late` = run-time requires on both sides plus
PCL's string evals; `CLOS` = SBCL's run-time constructor compiles; subprocesses are not in the run.  Sorted by excess.

| dist | C | other | perl | PCL | PCL - perl | ratio | late perl / PCL | CLOS compiles | GC | subproc perl / PCL | largest excess |
|---|---:|---:|---:|---:|---:|---:|---|---:|---:|---|---|
| JSON-PP | 63 | 2 | 20.04 | 69.23 | 49.20 | 3.5x | 0.43 / 3.21 | 0.16 | 2.36 | 0.00 / 0.00 | 099_binary15.t (5.80) |
| Template-Toolkit | 94 | 27 | 1.23 | 19.08 | 17.85 | 15.5x | 0.81 / 17.33 | 0.21 | 1.97 | 0.00 / 0.00 | foreach.t (1.30) |
| Capture-Tiny | 21 | 3 | 5.47 | 21.07 | 15.60 | 3.9x | 0.02 / 4.04 | 0.02 | 0.41 | 10.76 / 29.36 | 07-stderr-closed.t (1.48) |
| IO-Compress | 46 | 47 | 1.05 | 5.95 | 4.90 | 5.7x | 0.25 / 2.93 | 0.08 | 0.56 | 0.25 / 0.31 | 111const-deflate.t (0.60) |
| YAML-PP | 38 | 6 | 1.69 | 5.72 | 4.03 | 3.4x | 0.04 / 0.60 | 0.02 | 0.24 | 0.46 / 0.87 | 43.indent.t (1.20) |
| Pod-Simple | 64 | 10 | 0.42 | 2.95 | 2.52 | 6.9x | 0.10 / 1.91 | 0.21 | 0.14 | 0.00 / 0.03 | JustPod01.t (0.34) |
| Class-Method-Modifiers | 26 | 3 | 0.04 | 2.10 | 2.06 | 53.6x | 0.01 / 1.99 | 0.01 | 0.03 | 0.00 / 0.00 | 00-report-prereqs.t (0.27) |
| HTTP-Message | 14 | 9 | 0.07 | 1.72 | 1.65 | 23.1x | 0.04 / 1.62 | 0.02 | 0.24 | 0.00 / 0.00 | request.t (0.48) |
| TimeDate | 5 | 0 | 0.02 | 1.46 | 1.43 | 62.9x | 0.00 / 1.37 | 0.02 | 0.33 | 0.00 / 0.00 | lang.t (1.15) |
| Path-Tiny | 23 | 7 | 0.14 | 0.96 | 0.82 | 6.9x | 0.02 / 0.82 | 0.01 | 0.14 | 0.00 / 0.01 | 00-report-prereqs.t (0.27) |
| HTTP-Tiny | 26 | 6 | 0.07 | 0.89 | 0.82 | 12.1x | 0.02 / 0.74 | 0.01 | 0.05 | 0.00 / 0.00 | 00-report-prereqs.t (0.26) |
| URI | 48 | 15 | 0.15 | 0.95 | 0.80 | 6.5x | 0.08 / 0.66 | 0.07 | 0.04 | 0.02 / 0.05 | 00-report-prereqs.t (0.27) |
| Mojo-DOM58 | 4 | 1 | 0.22 | 0.82 | 0.61 | 3.8x | 0.01 / 0.23 | 0.00 | 0.04 | 0.00 / 0.00 | dom.t (0.36) |
| Text-Balanced | 9 | 5 | 0.07 | 0.64 | 0.57 | 9.7x | 0.00 / 0.52 | 0.01 | 0.02 | 0.00 / 0.00 | 09_gentag.t (0.16) |
| Role-Tiny | 20 | 3 | 0.01 | 0.47 | 0.46 | 33.4x | 0.00 / 0.39 | 0.02 | 0.00 | 0.00 / 0.00 | role-basic-composition.t (0.19) |
| YAML-Tiny | 10 | 3 | 0.17 | 0.55 | 0.38 | 3.2x | 0.03 / 0.42 | 0.01 | 0.03 | 0.00 / 0.00 | 30_yaml_spec_tml.t (0.23) |
| Log-Log4perl | 41 | 34 | 0.09 | 0.41 | 0.32 | 4.7x | 0.02 / 0.28 | 0.01 | 0.00 | 0.00 / 0.00 | 040Filter.t (0.20) |
| Try-Tiny | 5 | 6 | 0.03 | 0.32 | 0.29 | 9.4x | 0.01 / 0.25 | 0.00 | 0.02 | 0.00 / 0.00 | 00-report-prereqs.t (0.27) |
| HTTP-Date | 3 | 0 | 0.03 | 0.29 | 0.26 | 10.0x | 0.01 / 0.24 | 0.01 | 0.02 | 0.00 / 0.02 | 00-report-prereqs.t (0.25) |
| Sub-Uplevel | 4 | 6 | 0.01 | 0.26 | 0.24 | 19.5x | 0.01 / 0.23 | 0.00 | 0.02 | 0.00 / 0.00 | 00-report-prereqs.t (0.24) |
| Data-Dumper | 19 | 7 | 0.02 | 0.13 | 0.11 | 6.6x | 0.00 / 0.09 | 0.01 | 0.00 | 0.00 / 0.00 | pair.t (0.02) |
| Data-Dump | 13 | 2 | 0.01 | 0.08 | 0.08 | 16.6x | 0.00 / 0.01 | 0.05 | 0.00 | 0.00 / 0.00 | regexp.t (0.01) |
| Class-Inspector | 5 | 1 | 0.04 | 0.11 | 0.07 | 2.9x | 0.03 / 0.08 | 0.01 | 0.00 | 0.00 / 0.00 | 00_diag.t (0.05) |
| Getopt-Long | 8 | 0 | 0.01 | 0.08 | 0.07 | 5.8x | 0.01 / 0.05 | 0.02 | 0.00 | 0.00 / 0.00 | gol-compat.t (0.02) |
| Scalar-List-Utils | 19 | 19 | 0.01 | 0.07 | 0.05 | 4.9x | 0.00 / 0.03 | 0.02 | 0.00 | 0.00 / 0.00 | openhan.t (0.03) |
| File-Which | 3 | 0 | 0.03 | 0.09 | 0.05 | 2.7x | 0.03 / 0.08 | 0.00 | 0.00 | 0.00 / 0.00 | 00_diag.t (0.05) |
| Text-CSV | 23 | 15 | 1.26 | 1.31 | 0.05 | 1.0x | 0.03 / 0.34 | 0.12 | 0.10 | 0.00 / 0.00 | 00_pod.t (0.24) |
| Algorithm-Diff | 2 | 0 | 0.01 | 0.02 | 0.01 | 2.4x | 0.00 / 0.00 | 0.00 | 0.00 | 0.00 / 0.00 | base.t (0.01) |
| Safe-Isa | 2 | 0 | 0.00 | 0.00 | 0.00 | 2.2x | 0.00 / 0.00 | 0.00 | 0.00 | 0.00 / 0.00 | safe_does.t (0.00) |
| Sort-Versions | 1 | 0 | 0.00 | 0.01 | 0.00 | 1.6x | 0.00 / 0.00 | 0.00 | 0.00 | 0.00 / 0.00 | versions.t (0.00) |
| **board** | **659** | **237** | **32.42** | **137.73** | **105.31** | **4.2x** | **2.03 / 40.47** | **1.13** | **6.77** | **11.49 / 30.65** | |


## 3. The files with the most run-phase excess

The 60 compared files with the most run-phase excess (PCL - perl, CPU ms, best of 3); `late` and `CLOS` as in
section 2; `MB` = consed in PCL's run; `spread` = (max - min) / min of the three reps per side; `load` = the highest
1-minute load any rep of the file started at.  The whole table: `baselines/board-runtime-s541.tsv`.

| # | file | perl ms | PCL ms | excess ms | ratio | late perl / PCL ms | CLOS ms | GC ms | MB | subproc perl / PCL ms | spread perl / PCL | load |
|---:|---|---:|---:|---:|---:|---|---:|---:|---:|---|---|---:|
| 1 | JSON-PP/099_binary15.t | 2386.4 | 8189.0 | 5802.5 | 3.4x | 25.3 / 192.5 | 7.1 | 303.4 | 13070.4 | 0.0 / 0.0 | 1% / 0% | 1.16 |
| 2 | JSON-PP/099_binary14.t | 2197.8 | 7612.5 | 5414.7 | 3.5x | 25.2 / 193.3 | 6.5 | 282.8 | 11934.8 | 0.0 / 0.0 | 2% / 0% | 1.21 |
| 3 | JSON-PP/099_binary13.t | 2035.0 | 7064.2 | 5029.2 | 3.5x | 24.0 / 193.4 | 6.1 | 255.9 | 10865.2 | 0.0 / 0.0 | 4% / 0% | 1.22 |
| 4 | JSON-PP/099_binary12.t | 1907.3 | 6598.9 | 4691.6 | 3.5x | 23.4 / 192.9 | 6.2 | 234.4 | 9947.3 | 0.0 / 0.0 | 0% / 0% | 1.25 |
| 5 | JSON-PP/099_binary11.t | 1773.0 | 6162.2 | 4389.2 | 3.5x | 24.3 / 191.6 | 6.5 | 215.4 | 9057.1 | 0.0 / 0.0 | 1% / 1% | 1.20 |
| 6 | JSON-PP/099_binary10.t | 1604.4 | 5506.2 | 3901.9 | 3.4x | 24.5 / 194.3 | 6.1 | 182.5 | 7867.6 | 0.0 / 0.0 | 13% / 1% | 1.11 |
| 7 | JSON-PP/099_binary09.t | 1458.6 | 4991.7 | 3533.1 | 3.4x | 25.4 / 197.0 | 6.2 | 164.0 | 6956.4 | 0.0 / 0.0 | 1% / 0% | 1.16 |
| 8 | JSON-PP/099_binary08.t | 1307.3 | 4479.0 | 3171.7 | 3.4x | 26.7 / 197.6 | 6.2 | 146.8 | 6075.0 | 0.0 / 0.0 | 6% / 0% | 1.11 |
| 9 | JSON-PP/099_binary07.t | 1174.7 | 4023.0 | 2848.3 | 3.4x | 24.4 / 199.0 | 6.6 | 131.3 | 5319.6 | 0.0 / 0.0 | 3% / 1% | 1.05 |
| 10 | JSON-PP/099_binary06.t | 1033.2 | 3581.8 | 2548.6 | 3.5x | 25.1 / 192.5 | 7.0 | 114.8 | 4642.7 | 0.0 / 0.0 | 0% / 3% | 1.06 |
| 11 | JSON-PP/099_binary05.t | 880.9 | 3022.9 | 2142.0 | 3.4x | 25.8 / 197.1 | 6.0 | 89.6 | 3775.7 | 0.0 / 0.0 | 1% / 0% | 1.06 |
| 12 | JSON-PP/099_binary04.t | 723.9 | 2458.0 | 1734.1 | 3.4x | 24.5 / 193.0 | 6.1 | 72.6 | 2960.1 | 0.0 / 0.0 | 2% / 0% | 0.98 |
| 13 | Capture-Tiny/07-stderr-closed.t | 507.2 | 1991.8 | 1484.6 | 3.9x | 0.0 / 346.8 | 0.0 | 38.6 | 279.5 | 1030.0 / 2724.8 | 4% / 1% | 1.73 |
| 14 | Capture-Tiny/12-stdin-string.t | 519.2 | 2000.9 | 1481.7 | 3.9x | 0.1 / 337.9 | 3.9 | 39.1 | 283.8 | 1010.0 / 2748.3 | 4% / 1% | 1.40 |
| 15 | Capture-Tiny/11-stderr-string.t | 517.3 | 1988.1 | 1470.8 | 3.8x | 0.0 / 340.0 | 4.4 | 41.6 | 283.0 | 1010.0 / 2736.1 | 2% / 0% | 1.41 |
| 16 | Capture-Tiny/10-stdout-string.t | 510.1 | 1980.4 | 1470.2 | 3.9x | 0.0 / 321.3 | 3.9 | 38.9 | 281.6 | 1030.0 / 2705.0 | 4% / 1% | 1.53 |
| 17 | Capture-Tiny/06-stdout-closed.t | 504.4 | 1971.4 | 1467.0 | 3.9x | 0.0 / 333.6 | 0.0 | 36.2 | 280.5 | 1000.0 / 2804.9 | 5% / 2% | 1.87 |
| 18 | Capture-Tiny/08-stdin-closed.t | 498.2 | 1947.4 | 1449.2 | 3.9x | 0.0 / 354.2 | 0.0 | 39.7 | 279.6 | 1000.0 / 2782.5 | 6% / 2% | 1.62 |
| 19 | JSON-PP/099_binary03.t | 572.8 | 1987.6 | 1414.8 | 3.5x | 25.6 / 197.1 | 6.7 | 59.0 | 2310.9 | 0.0 / 0.0 | 2% / 2% | 0.98 |
| 20 | Capture-Tiny/15-stdin-tied.t | 510.3 | 1901.9 | 1391.6 | 3.7x | 0.0 / 348.2 | 0.0 | 39.6 | 270.0 | 1000.0 / 2699.3 | 3% / 0% | 1.33 |
| 21 | Template-Toolkit/foreach.t | 26.7 | 1324.7 | 1297.9 | 49.5x | 9.3 / 1252.2 | 2.9 | 253.9 | 894.7 | 0.0 / 0.0 | 2% / 0% | 0.90 |
| 22 | YAML-PP/43.indent.t | 504.7 | 1703.3 | 1198.6 | 3.4x | 0.1 / 2.8 | 0.9 | 49.9 | 1224.1 | 0.0 / 0.0 | 2% / 1% | 1.11 |
| 23 | TimeDate/lang.t | 3.9 | 1153.9 | 1150.0 | 296.6x | 1.8 / 1139.8 | 5.6 | 281.4 | 795.9 | 0.0 / 0.0 | 4% / 1% | 0.91 |
| 24 | Template-Toolkit/view.t | 37.0 | 1166.6 | 1129.6 | 31.5x | 9.4 / 1064.8 | 2.5 | 144.4 | 822.7 | 0.0 / 0.0 | 4% / 2% | 0.88 |
| 25 | JSON-PP/099_binary02.t | 426.1 | 1503.8 | 1077.6 | 3.5x | 26.9 / 191.7 | 6.8 | 45.9 | 1670.1 | 0.0 / 0.0 | 16% / 0% | 0.97 |
| 26 | Template-Toolkit/filter.t | 23.7 | 1075.1 | 1051.5 | 45.5x | 3.1 / 991.7 | 1.7 | 101.8 | 788.2 | 0.0 / 0.0 | 7% / 0% | 0.81 |
| 27 | Capture-Tiny/03-tee.t | 374.1 | 1340.0 | 965.9 | 3.6x | 0.0 / 218.6 | 0.0 | 18.3 | 173.1 | 870.0 / 2431.6 | 2% / 2% | 1.95 |
| 28 | Capture-Tiny/24-all-badtied.t | 342.1 | 1254.6 | 912.5 | 3.7x | 0.0 / 211.9 | 0.0 | 21.7 | 169.7 | 660.0 / 1887.1 | 1% / 1% | 1.41 |
| 29 | Capture-Tiny/20-stdout-badtie.t | 336.3 | 1245.8 | 909.5 | 3.7x | 0.0 / 216.8 | 0.0 | 16.9 | 168.2 | 670.0 / 1867.9 | 2% / 1% | 1.45 |
| 30 | Capture-Tiny/21-stderr-badtie.t | 343.9 | 1252.8 | 908.9 | 3.6x | 0.0 / 217.3 | 0.0 | 20.7 | 169.4 | 670.0 / 1881.5 | 2% / 0% | 1.46 |
| 31 | Capture-Tiny/22-stdin-badtie.t | 336.0 | 1237.9 | 901.9 | 3.7x | 0.0 / 218.7 | 0.0 | 17.4 | 169.9 | 680.0 / 1869.1 | 4% / 2% | 1.43 |
| 32 | YAML-PP/20.dump.t | 298.9 | 1146.4 | 847.5 | 3.8x | 4.8 / 156.9 | 0.9 | 35.2 | 841.2 | 0.0 / 0.0 | 1% / 0% | 1.14 |
| 33 | JSON-PP/099_binary01.t | 292.1 | 1109.2 | 817.1 | 3.8x | 25.7 / 194.4 | 6.1 | 36.5 | 1151.1 | 0.0 / 0.0 | 5% / 0% | 0.88 |
| 34 | Template-Toolkit/try.t | 21.0 | 715.6 | 694.6 | 34.1x | 8.9 / 665.1 | 2.8 | 100.9 | 498.5 | 0.0 / 0.0 | 5% / 1% | 0.85 |
| 35 | Template-Toolkit/vmethods/list.t | 22.8 | 624.8 | 602.1 | 27.5x | 9.2 / 576.4 | 3.1 | 74.4 | 433.3 | 0.0 / 0.0 | 2% / 1% | 0.89 |
| 36 | IO-Compress/111const-deflate.t | 11.9 | 610.3 | 598.4 | 51.3x | 6.4 / 539.6 | 0.0 | 18.2 | 102.8 | 0.0 / 0.0 | 8% / 0% | 0.62 |
| 37 | Template-Toolkit/directry.t | 24.0 | 584.5 | 560.5 | 24.4x | 10.1 / 532.6 | 4.0 | 82.6 | 410.0 | 0.0 / 0.0 | 2% / 1% | 0.77 |
| 38 | Template-Toolkit/vars.t | 18.4 | 576.7 | 558.3 | 31.4x | 1.6 / 517.0 | 1.0 | 35.2 | 439.8 | 0.0 / 0.0 | 1% / 0% | 0.87 |
| 39 | Template-Toolkit/stash.t | 17.1 | 529.5 | 512.5 | 31.0x | 6.7 / 493.1 | 1.4 | 88.7 | 355.4 | 0.0 / 0.0 | 1% / 1% | 0.81 |
| 40 | JSON-PP/099_binary00.t | 152.9 | 639.7 | 486.8 | 4.2x | 26.2 / 191.3 | 6.2 | 23.2 | 590.7 | 0.0 / 0.0 | 3% / 1% | 0.87 |
| 41 | HTTP-Message/request.t | 12.0 | 494.4 | 482.5 | 41.3x | 9.8 / 484.5 | 3.1 | 14.5 | 100.0 | 0.0 / 0.0 | 1% / 1% | 1.27 |
| 42 | Template-Toolkit/vmethods/text.t | 21.1 | 468.9 | 447.9 | 22.3x | 9.1 / 423.3 | 2.7 | 37.6 | 324.5 | 0.0 / 0.0 | 4% / 1% | 0.90 |
| 43 | Template-Toolkit/varsv1.t | 21.9 | 468.4 | 446.5 | 21.4x | 9.2 / 425.9 | 1.0 | 37.2 | 341.1 | 0.0 / 0.0 | 2% / 1% | 0.88 |
| 44 | IO-Compress/999pod.t | 129.0 | 561.2 | 432.2 | 4.4x | 13.1 / 318.7 | 3.1 | 14.8 | 177.1 | 0.0 / 0.0 | 2% / 1% | 0.65 |
| 45 | HTTP-Message/message-old.t | 9.8 | 441.2 | 431.3 | 44.8x | 8.3 / 430.1 | 6.0 | 101.4 | 198.9 | 0.0 / 0.0 | 2% / 3% | 1.29 |
| 46 | YAML-PP/21.emit.t | 192.0 | 611.6 | 419.6 | 3.2x | 0.1 / 2.8 | 0.8 | 24.2 | 430.8 | 0.0 / 0.0 | 13% / 1% | 1.14 |
| 47 | Template-Toolkit/string.t | 22.4 | 425.3 | 402.9 | 19.0x | 8.9 / 371.8 | 3.0 | 31.3 | 305.0 | 0.0 / 0.0 | 6% / 2% | 0.83 |
| 48 | Template-Toolkit/provider.t | 26.0 | 417.7 | 391.7 | 16.1x | 12.6 / 383.7 | 1.2 | 62.2 | 297.7 | 0.0 / 0.0 | 12% / 2% | 0.82 |
| 49 | Template-Toolkit/include.t | 21.4 | 411.9 | 390.5 | 19.3x | 11.6 / 375.2 | 1.0 | 37.6 | 283.6 | 0.0 / 0.0 | 14% / 0% | 0.91 |
| 50 | YAML-PP/12.load-json.t | 135.6 | 525.1 | 389.5 | 3.9x | 4.5 / 159.9 | 0.9 | 25.1 | 409.2 | 0.0 / 0.0 | 1% / 1% | 1.08 |
| 51 | HTTP-Message/message-parts.t | 5.5 | 393.2 | 387.7 | 70.8x | 2.9 / 386.1 | 2.3 | 94.9 | 175.2 | 0.0 / 0.0 | 7% / 3% | 1.29 |
| 52 | Template-Toolkit/service.t | 20.1 | 384.9 | 364.8 | 19.2x | 13.0 / 343.2 | 1.1 | 27.1 | 264.8 | 0.0 / 0.0 | 4% / 1% | 0.80 |
| 53 | Mojo-DOM58/dom.t | 198.9 | 556.8 | 357.9 | 2.8x | 0.0 / 0.0 | 0.8 | 11.9 | 346.7 | 0.0 / 0.0 | 3% / 1% | 1.24 |
| 54 | Template-Toolkit/directive.t | 19.5 | 362.9 | 343.4 | 18.6x | 11.6 / 331.4 | 2.9 | 30.2 | 257.2 | 0.0 / 0.0 | 9% / 1% | 0.66 |
| 55 | Pod-Simple/JustPod01.t | 12.3 | 354.8 | 342.5 | 28.9x | 11.0 / 335.5 | 6.1 | 5.7 | 42.5 | 0.0 / 0.0 | 2% / 3% | 1.24 |
| 56 | YAML-PP/56.force-flow.t | 155.2 | 492.1 | 336.9 | 3.2x | 0.1 / 3.0 | 0.9 | 22.8 | 340.0 | 0.0 / 0.0 | 1% / 1% | 1.16 |
| 57 | Template-Toolkit/table.t | 14.2 | 341.1 | 327.0 | 24.1x | 9.4 / 321.5 | 2.8 | 64.9 | 229.6 | 0.0 / 0.0 | 21% / 2% | 0.84 |
| 58 | IO-Compress/102tied-deflate.t | 54.3 | 361.4 | 307.1 | 6.7x | 8.3 / 83.9 | 2.7 | 50.5 | 865.0 | 0.0 / 0.0 | 11% / 0% | 1.29 |
| 59 | IO-Compress/102tied-gzip.t | 54.1 | 353.6 | 299.6 | 6.5x | 8.1 / 66.9 | 2.3 | 46.1 | 862.4 | 0.0 / 0.0 | 4% / 1% | 1.29 |
| 60 | Template-Toolkit/chomp.t | 18.2 | 316.3 | 298.1 | 17.4x | 11.6 / 288.5 | 1.0 | 31.7 | 227.5 | 0.0 / 0.0 | 2% / 1% | 0.76 |


## 4. THINGS THAT ARE SLOW AT RUN TIME, ranked by measured seconds over the board

Seconds are PCL - perl run-phase CPU over the 659 compared files (section 2's
pass), from the instrument's own columns where the component has one (rows 1,
5, 6, 9, 11), else the profile share x the profiled files' runs, scaled to
their distribution (said so in the row).  "files / dists" = where the
component is above 5 ms.  Every measurement named TAKEN ran on the quiet box
in `scratch/s541a/legs/listbench.log` (micro programs: the measured section
timed inside the program, best of 3; the A/B: `--run`, best of 3 per side).
Profiles: `sb-sprof` `:cpu` and `:alloc` over the run, 29 files of 16
distributions, from the program's fasl (`scratch/s541a/prof-c1fe4930/`; legs
`prof-profA.log`, `prof-profB.log`).

| # | what | board s | files / dists | profile evidence | lever | discriminating measurement | size | task |
|---:|---|---:|---|---|---|---|---|---|
| 1 | **A string eval's code is COMPILED by SBCL on every execution** (8,905 evals in the board's runs): Template-Toolkit compiles each template to Perl and evals it, Sub::Quote / Role::Tiny / Class::Method::Modifiers generate subs by string eval, ExtUtils::MakeMaker's `parse_version` evals each VERSION line (every `00-report-prereqs.t`), Date::Language's tables (TimeDate `lang.t`), Capture::Tiny's 390 per file | **28.1** | 207 / 23 | `compile-in-lexenv` inclusive: TimeDate `lang.t` 96 %, CMM `000-load.t` 92 %, TT `filter.t` 77 %, Role-Tiny 69 %, Text-Balanced 60 %; TT `foreach.t` 45 evals = 1.22 s | compile eval'd code at a cheap policy (#2421); load a cached eval's fasl instead of compiling its text (#1410); don't recompile an already-cached text per execution (#2191) | **TAKEN** (#2421's lever as a preload that binds `(compilation-speed 3) (speed 0)` inside `%p-eval-1` only, `micro/cheap-eval-policy.lisp`): TT `foreach.t` 1,326 -> 621 ms (-53 %), `view.t` 1,166 -> 701 (-40 %), `filter.t` 1,074 -> 630 (-41 %), TimeDate `lang.t` 1,143 -> 188 (-84 %), CMM `00-report-prereqs.t` 293 -> 262 (-11 %), the run of the templates' code at speed 0 included; controls JSON-PP `099_binary05.t` -1 %, HTTP-Message `request.t` +1 %.  Capture-Tiny `06-stdout-closed.t` did NOT move (-1 %): its 390 evals compile outside `%p-eval-1` -- for #2191 to place.  `micro/m-evalstr.pl` (300 distinct evals): perl 1.5 ms, PCL 1.16 s | one session (#2421); several (#1410) | #2421, #1410, #2191 |
| 2 | **JSON::PP's encode / decode** (the `099_binary*` round trips: the `->ascii` encoder's per-character `map { ... sprintf('\u%04x') } unpack('U*')`, the decoder's per-character `next_chr`) | **~44** (16 files; the dist's excess 49.2 minus its late compiles) | 16 / 1 | a SPREAD, as s540a found: `box-set` + `%make-p-box` 10.6 % self, `gethash/equal` 7.6 % (field reads, the own-method cache), `%p-str-eq-slow` 4.4 %, `%p-flatten-list` 3.6 %, `p-map` 4.0 %, `p-sprintf` 2.6 %, `utf8::decode` 2.8 %; allocation: `%make-p-box` 28 %, `%concatenate-to-string` 24 %, `vector-subseq` 18 % | none single; encode first (5x vs decode 2.4x) | **TAKEN** (`micro/m-jsonpp.pl`, 40 round trips of a 200-string array): perl 168 ms / PCL 543 ms (3.2x); encode 56 -> 279 ms (5.0x), decode 112 -> 263 ms (2.4x).  s540a read 4.6x on 05806e16 | several | #4967 |
| 3 | **`Symbol::gensym` materialises the stash view of `Symbol::`** on every call (`delete $$genpkg{$name}`) -- every `IO::Handle->new`, `IO::File->new`; Capture-Tiny's captures | **~4.6** (22 % of Capture-Tiny's 21.1 s run) | 21 / 1 | `p-stash` 21-22 % inclusive, `%p-stash-add-variable-names` 13-14 %, `package-iter-step` 11 % of Capture-Tiny `03-tee.t` / `06-stdout-closed.t` | a stash ELEMENT operation that does not build the view | **TAKEN** (`micro/m-gensym.pl`): 20,000 x `gensym` perl 7.2 ms / PCL 1,517 ms (76 us a call, 210x; s540a: 81 us) | one session | #4964 |
| 4 | **A Perl `fork`** (Capture-Tiny's tee: fork + exec per capture) | **~3.5** in the run (13-20 % of Capture-Tiny's run) + the children OUTSIDE it: PCL 29.4 s vs perl 10.8 s | 21 / 1 (subprocesses: 21 / 6) | `sb-posix:fork` 13 % (`06`), `%p-fork-process` 20 % (`03-tee`), `%p-flush-open-streams` 3-5 % | no thread restart in a child that will exec; a cheaper `system` | **TAKEN** (`micro/m-fork.pl`): 200 x fork + `exec 'true'`: wall perl 195 ms / PCL 908 ms (+3.6 ms a fork, 4.7x) | one session | #4965 |
| 5 | **A module first `require`d at RUN time**: the cache entry's validation (every dependency's truename / realpath / manifest) and the fasl load | **6.0** (PCL 8.01 s, perl 2.03 s for the same requires) | 278 / 25 | Pod-Simple `JustPod01.t`: fasl load 78.5 %, `query-file-system` / `truename` / `realpath` 25 %; Log4perl `040Filter.t`: `%p-manifest-at` 24 %, `%p-dep-moved-p` 18 %, `%p-shim-lib-dir-p` 14 %; HTTP-Tiny `160_cookies.t`: `fop-load-code` 21 % | memoise the dependency resolution per process; validate a directory once a run; then the fasl's own top-level work | **TAKEN** (`micro/m-latereq.pl`, 19 core modules required in a sub, warm cache): perl 1.07 ms a module, PCL 14.4 ms (13.5x) | one session (the validation half) | **#5065** (new) |
| 6 | **A string eval's TRANSPILE that misses the eval cache** -- the pl2cl server (~0.13-0.15 s to start) in every run of the file, because a FAILED transpile is never cached: CMM's lvalue probe `eval 'return 1; &_sub = 1'` (4 times a run), Pod::Simple::BlackBox's `my_qr` evals (failing by #5061) | **4.4** | 30 / 10 | the eval-transpile wall (`evalx`) in every warm rep; a hook on `p-transpile-string` (`dev/whoeval.lisp`) shows the same texts requested again on every run | cache a failed transpile as a negative entry (same key, same error) | **TAKEN** (`dev/evalprobe2.sh`): three runs on one fresh cache -- the failing texts miss on runs 2 and 3, the good ones hit; CMM `003-basic.t` warm: 4 requests of one text | one session | **#5062** (new); the bug behind Pod-Simple's: **#5061**; CMM's probe fails by **#5068** |
| 7 | **An `s///` whose pattern is INTERPOLATED** re-translates it (`perl-regex-to-ppcre`: six `regex-replace-all` passes) on every call -- only a constant pattern is cached; YAML::PP's emitter / lexer, Mojo::DOM58's HTML handling | **~2.5-3** (YAML-PP ~2 s, Mojo 0.3 s; scaled from the shares) | (YAML-PP 38 files, Mojo, Data-Dumper, Text-CSV) | `perl-regex-to-ppcre` inclusive: YAML-PP `43.indent.t` 43.8 %, `20.dump.t` 32.2 % (`21.emit.t`: `%p-subst-compiled` 55 %), Mojo `dom.t` 50.4 %, Data-Dumper `pair.t` 23.8 %; Text-CSV `55_combi.t` 10 % of allocation | a per-site memo of (interpolated string, flags) -> translation, as `m//` has | **TAKEN** (`micro/m-interpre.pl`): 20,000 x `s/$p/Z/`: perl 4.6 ms / PCL 63.5 ms (13.7x); 20,000 x `/$p/` (cached path): 2.65 / 7.2 ms (2.7x) | one session | **#5063** (new) |
| 8 | **A match or `s///` on a LONG string pays its whole length** (cl-ppcre's constant-prefix `search`, element by element; the copy per `s///`); YAML::PP's lexer consuming its buffer in place | **~1.7** (YAML-PP ~1.6 s, Mojo 0.1 s) | YAML-PP, Mojo | `search` inclusive: YAML-PP `43.indent.t` 29.6 %, `21.emit.t` 28.1 %, `20.dump.t` 22.8 %; Mojo `dom.t` 20.8 % | a typed prefix search; `s/\A...//` as an in-place chop | **TAKEN** (`micro/m-longmatch.pl`, `m-longsubst.pl`): 2,000 x a late prefix in 50 KB: perl 0.87 / PCL 61.6 ms (71x); 2,000 x `s///` on a 50 KB copy: 3.55 / 314.5 ms (89x) -- as s540a | one session for the search | #4966 |
| 9 | **SBCL compiles CLOS constructors at run time** -- the first `make-instance` of each cl-ppcre node class (a program's first regex of a shape) | **1.13** | 44 / 8 | bucket `rcompile` (section 1a); backtrace `convert-compound-parse-tree` -> `make-instance` -> `install-optimized-constructor` -> `pcl-compile` | instantiate cl-ppcre's node classes when the core is built, so the constructors are saved compiled | **TAKEN** (`micro/ppcre-first.lisp`, x3): 40 patterns, pass 1 4.0 ms / 6 native compiles, pass 2 0.0 ms / 0; `m-regexfirst.pl`: 40 `qr//` built in 7.2 ms (perl 10 us) | small (core build) | **#5064** (new) |
| 10 | **A module re-transpiled AND fasl-built in every warm run** (HTTP-Message `request.t`: `lib/File/Glob.pm`) | **0.55** | 2 / 1 | column `r_build` 0.414 / 0.420 / 0.419 s in the three reps; `PCL_FASL_DEBUG`: `module Glob.pm -> fasl-build` each run | find why the entry never validates | NAMED: two warm runs of `request.t`, `r_build` must read 0 on the second | find the cause first | **#5066** (new) |
| 11 | **The eval sub table rebuilt on every eval** after a sub was installed (#2870's `%p-proto-index`) | < 0.5 (IO-Compress) | IO-Compress | `111const-deflate.t`: `%p-build-proto-index` 15 %, `%p-sub-bare-name` 8 %, `%pcl-invert-case` 8 % | update the index at sub installation | NAMED (in the task): 500 x (install a prototyped sub; eval) with 2,000 subs defined | small | **#5067** (new) |
| -- | **Inherited method calls** (by name per call) | not separable at board scale | Text-CSV, JSON-PP, Pod-Simple | Text-CSV `55_combi.t`: `p-find-overload` 8 %, `p-get-class` 7 % (#4524's shape); JSON-PP: `%pcl-own-method-cached` 2.9 % | the s534a design | **TAKEN** (`micro/m-method.pl`): 1 M inherited calls perl 74 / PCL 542 ms (7.3x), own-class 71 / 115 ms (1.6x) | awaiting the USER | #4523 - #4525 |
| -- | GC | 6.8 (inside every row above) | 200 / 18 | -- | -- | -- | -- | -- |

Every new row is filed (#5061-#5068: four levers, one cache miss, the small
index rebuild, two correctness findings); the rows owned by an existing task
carry its number.  The string-eval rows (1 and 6) together are **32.5 s, the
largest single item of the board's run phase**, and row 1's lever measured
-40..-84 % on the files where the evals ARE the run.

## 5. The first measurement's run-phase rows, and what s540d / MAIN-READY-92 changed

s540a's run-phase rows were small differences of load-inflated totals; with a
clean run clock:

| s540a row | then (05806e16) | now (c1fe4930, quiet, best of 3) | verdict |
|---|---|---|---|
| JSON::PP encode / decode 4.6x, a spread profile (#4967) | `micro/jsonpp.pl` 0.18 / 0.84 s | 0.168 / 0.543 s, **3.2x** (encode 5.0x, decode 2.4x); the board's 16 `099_binary*` 3.4-3.8x; the profile is the same spread (row 2) | **CONFIRMED**, smaller; the board's largest run-phase item after the evals |
| `fork` +3.4 ms of CPU (#4965) | 200 forks 0.18 / 0.85 s CPU | 200 forks: WALL 0.195 / 0.908 s (+3.6 ms a fork); the micro's own CPU (`times`, parent + children) 0.180 / 0.213 s | **CONFIRMED in wall, not in CPU**: the fork's extra is mostly waiting (the service threads' stop and restart), and Capture-Tiny's children cost 2.7x perl's outside the run |
| `Symbol::gensym` 81 us (#4964) | 20,000 in 1.63 s | 20,000 in 1.517 s (**76 us**, 210x) | **CONFIRMED** (row 3: ~4.6 s of Capture-Tiny's run) |
| a match / `s///` on a long string (#4966) | 70 ms / 0.38 s | 61.6 ms / 314.5 ms (71x / 89x) | **CONFIRMED** (row 8: YAML-PP) |
| string-eval compile (#2421) "small on the board" (42 s of `ecompile`, all phases) | -- | **28.1 s of the RUN** (+4.4 s of eval transpiles): the largest run-phase item; the cheap-policy lever -40..-84 % on the files where evals are the run | **REFUTED as "small"**: once compile and load are out of the way it is the first row |

What s540d shipped (literal `use constant` fold #4982, `tr///` once per op
#4981, `$|` mirror #4983, `{...}` presized #4984) and MAIN-READY-92's cheaper
sub-call frame, on this tree (`listbench.log`, inside-program times):

| lever | micro | perl | PCL | where its construct is on the board (`scratch/s541a/constructs.txt`, lines in lib/ + t/) |
|---|---|---:|---:|---|
| the sub-call frame (MAIN-READY-92) | 2 M calls of a two-argument sub | 182 ms | **66 ms (0.36x)** | everywhere; no profile shows `p-sub` / `p-flatten-args` above 4 % |
| #4982 constant fold | 3 M `DEBUG > 2 and print` | 31 ms | 48 ms (1.58x) | literal constants: IO-Compress 107, TT 53, JSON-PP 28, Log4perl 28, Getopt-Long 11 |
| #4983 `$|` | 1 M prints after `$| = 1` | 31 ms | 55 ms (1.78x) | Text-CSV 14, Log4perl 10, YAML-PP 9 |
| #4984 `{...}` | 200,000 ten-key constructors | 81 ms | 101 ms (1.25x) | 8+-key constructors on one line: JSON-PP 2 |
| #4981 `tr///` | 400,000 `tr` (count-only + translating) | 19.3 ms | **143 ms (7.4x)** | TT 15, Pod-Simple 16, Text-Balanced 12, URI 12, YAML-PP 12; Data::Dumper's `tr/\0-\377//c` per value |

None of the five constructs is in any of the 29 profiles' top functions: on the
board they no longer show.  `tr///` is still 7.4x perl per call after #4981 --
the remaining gap is its per-call path, not the table expansion -- but the
board's `tr`-heavy distribution (Data-Dumper, 19 files) runs 0.13 s in all.

## 6. Possible correctness findings: PCL's run phase under half of perl's

**26 compared files** run in under half of perl's time.  21 of them run under
1 ms on perl's side -- inside the clocks' noise (a few hundredths of a ms; four
of them read a PCL run below zero) -- and every one of the 26 reached perl's ok
count, so none is a silent skip by the instrument's own test.  The five with a
perl run of 1 ms or more were READ:

| file | perl ms | PCL ms | why |
|---|---:|---:|---|
| Path-Tiny/parent.t | 10.3 | 3.3 | 11 subtests holding ~150 `is()`: Test::Builder's own work |
| Capture-Tiny/19-relayering.t | 3.8 | 1.5 | perl's run holds 2.8 ms of run-time `require`s (its late column) that PCL does not need at run time; 5 `is()` |
| Mojo-DOM58/entities.t | 3.2 | 1.2 | 16 `is()` over entity decoding |
| Pod-Simple/search60.t | 1.8 | 0.5 | a few `ok()` and one search call |
| Class-Method-Modifiers/081-sub-and-modifier.t | 1.2 | -0.1 | ~40 `is()`; the modifiers are installed at compile time on both sides |

**What Test::More's own work costs** (`micro/m-tapcost.pl`, `listbench.log`):
2,000 `is()` perl 60.2 ms / PCL 3.5 ms (**30 us against 1.7 us** a row), 20
subtests of 10 `is()` 11.5 / 0.45 ms (0.57 ms against 22 us a subtest).  PCL's
TAP layer is native (`cl/pcl-test.lisp`), so the harness is ~17x cheaper; the
board's compared files print 81,720 top-level oks in the precise pass's first
three legs alone -- some 2.5 s of perl's 32.4 s is Test::Builder, against
~0.15 s in PCL.  The module's own work is therefore a little MORE than 4.2x
perl's: these files are the same caveat s540a found in Text-CSV `55_combi.t`,
and no file here is a skip.

**Two correctness findings fell out of the profiles' cache misses** (row 6):
#5061 (a string eval with a lone surrogate in its text breaks every later
uncached eval of the process -- Pod::Simple::BlackBox's load does it) and
#5068 (`&lvsub = 1` rejected; Class::Method::Modifiers's lvalue probe).

## 7. What this measurement does not tell

A test file's run phase is a poor sample of a program's steady state.  It is
SHORT (the median compared file runs a few milliseconds under perl), so PCL's
first-use costs -- the first regex of each shape (CLOS constructors), the
first string eval (the transpile server's start), modules a test `require`s at
run time -- weigh far more than in a long-running program, and the per-call
costs that dominate a real workload (method dispatch at scale, string building,
large hashes, I/O loops) are under-sampled: the bench rows
(`tools/bench-exec.pl`), the everyday corpus and s540b's micro rows measure
those.  The Test::More layer is NATIVE in PCL (`cl/pcl-test.lisp`), so a file
with thousands of assertions makes PCL's run look faster than the module's own
code is (section 6).  Every run here is ONE process from a warm cache with the
transpile kept: the cold first run, the transpile and the load phase are
s540a's measurement, not this one.  The run was not repeated in-process (the
brief's "repeat it N times in one warm state, where the file allows it"): a
test file's state -- its plan, its counter, its globals, its output -- makes a
second in-process run a different program, so every rep is a fresh process
and the warm-up inside it is part of the run, as it is for perl.  Perl's
string-eval compile could not be separated from perl's run (no hook), so the
late-compile column is PCL's evals + both sides' run-time requires.  And
CPU seconds on this laptop are only comparable at the same load: every number
here was taken at a 1-minute load below 2 (the rows carry it).
