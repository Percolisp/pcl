# How long to the end of the bug queue, and how long to production? (s513, Fable, 2026-10-10)

The USER's question, asked while three batches ran: *"considering the speed of fixing bugs and making CPAN
modules work, how long will it take to go over the bug queue? How long before we can call PCL usable for
production?"*  Every number below was measured on main `dbcf19dc` (code `e0b61a99`, gen v2-5184) on
2026-10-10 between 02:30 and 03:30; the measurement commands are in the session's running notes
(`briefs-and-rules-for-claude-subagents/s513/PAUSE-s513.md`, the 03:xx block).  Estimates are
extrapolations of measured rates and say what they assume.

## 0. The answer in short

**The bug queue does not end; it is a frontier.**  770 tasks are open.  In the six days since the task
store entered the repository, 81 tasks were closed and 109 were filed; of the 751 that were open on
2026-10-04, 28 are closed today.  At the measured rates (about 13 closures and 18 filings per day with
three agents a night and one review session a day) the open count drifts up, not down, because every
fixed batch is probed against perl and every probe round files two to five new findings.  That is the
process working, not failing: the findings are real divergences that were there before anyone looked.
The queue's *shape* can be made to converge, by naming a SUPPORTED SUBSET and driving the known
silent-wrongs inside it to zero; section 5 says how long that takes.

**Production has three tiers, and they are at different distances:**

| tier | what it means | state today | distance, at the measured rates |
|---|---|---|---|
| **1. Your own scripts**, pure Perl, core modules, run beside perl once to check | the everyday battery's shape | **114 of 122 ordinary programs byte-identical**; the 8 others named, 5 of them ruled non-support; warm start 40 ms / 28 MB; first run ~3 s per 1,000 lines | **Usable now, with a diff-test gate** (run perl beside, compare stdout and exit).  Unsupervised use (no perl beside) when the known silent-wrongs inside the subset reach zero: **2–3 months**. |
| **2. Pure-Perl CPAN applications** (Moo, JSON::PP, Getopt::Long, Try::Tiny, Class::Method::Modifiers, Text::Balanced, Data::Dump …) | the CPAN board's shape | board **1,941 ok / 251 not-ok over 183 files; 75 files pass whole**; Moo works (object construction 25x slower than perl) | **3–5 months**, and it depends on two USER decisions that are currently parked or deferred: `DESTROY` at scope exit (#2370) and `caller()` fidelity (#233), which between them hold the largest remaining row families and the RAII idiom production code relies on. |
| **3. Anything on CPAN**, including XS | Encode, Storable, Digest::MD5, Hash::Util, Unicode::Normalize, File::Glob, Compress::Zlib, DBI, JSON::XS | **none of these load**: the XS bridge (pclxs) is parked and the core XS modules have no pure-Perl shims; `use Encode` fails, which blocks most non-ASCII text processing | **Not estimable from current rates** because the work is parked.  The shim route for the six core XS modules that matter most is 6–10 agent-sessions (section 4a); the bridge route is months. |

The single most valuable next piece of work for production, by the measurements, is a pure-Perl
**Encode** shim (section 4a): it is the one module a production text-processing program cannot do
without, PCL's string model already carries the byte/character distinction, and it is one to two
sessions.

## 1. Velocity, measured

| measure | value | how measured |
|---|---|---|
| commits per week, W34–W41 | 163, 137, 275, 233, 162, 194, 172, 291 | `git log --date=format:%G-W%V` |
| review sessions per week, W35–W41 | 9, 10, 11, 7, 6, 8, 6 | `docs/session-log.md` headers |
| agents per review session | 2–3 (the cap is set per session by the USER) | SHARED-BOX |
| merges per night | up to 8 (the s513 night) | PAUSE-s513 |
| gate (`Pl/t`) rows | 5,600 (2026-08-21) → **10,091** (2026-10-10): +4,491 in 7 weeks, ~90 a day | CLAUDE.md history |
| perl-tests sweep passing | 18,363 (08-21) → 18,714 (late Sept) → **18,740** (10-10) | session log anchors |
| everyday battery | 85 of 122 (09-21) → 114 of 122 (10-05) → **114 (flat since)** | `baselines/everyday-history.tsv` |
| CPAN board not-ok rows | 674 (08-05) → **251** (10-10): −423 in 9.5 weeks, ~6 a day | board14 baselines, s513f's `board-tree.tsv` |

Two readings.  First, the regression-test count doubling in seven weeks is the real output of the
period: every row is a perl-vs-PCL fact that was probed, and that is what makes the next change safe.
Second, two of the three steering populations are **saturated against ruled non-support**: the everyday
battery has been flat at 114 since 2026-10-05 because five of its eight remaining rows are ruled
(DESTROY at scope exit, format/write, XS digests, Unicode case folding) and the sweep gained 26 rows in
two weeks because most of its 470 blessed failures are ruled or parked (section 3).  Gains now come from
the CPAN board and from ordinary programs outside the corpus, which is where the review probes find
their two to five findings per batch.

## 2. The queue, measured

**Size and flow.**  1,593 tasks ever filed; 823 closed; **770 open** (724 pending, 41 open, 5 in
progress).  Filed per week and how many of each week's cohort are still open:

| week | filed | still open | closed |
|---|---|---|---|
| W34 | 98 | 40 | 59 % |
| W35 | 116 | 54 | 53 % |
| W36 | 256 | 100 | 61 % |
| W37 | 247 | 141 | 43 % |
| W38 | 181 | 116 | 36 % |
| W39 | 106 | 70 | 34 % |
| W40 | 99 | 54 | 45 % |
| W41 (partial) | 74 | 43 | 42 % |

Since the store entered the repository (2026-10-04, six days): **109 filed, 53 of them already closed**
(49 % within the week); of the **751 open on 10-04, 28 closed** (3.7 % of the backlog per week).  The
median open task was filed on 2026-09-13 (four weeks old); the oldest dated one on 2026-08-21; 112 open
tasks predate the `created` field.

**What the 770 are** (subject keywords, first match wins):

| class | open | note |
|---|---|---|
| silent-wrong divergences | **105** | 32 filed in the last 14 days — the discovery rate is ~2.3 a day, almost all from review probes |
| CPAN module / shim | 106 | a module behaves differently; includes the board's per-dist families |
| instruments, tooling, baselines, runners, CI | 64 | not bugs |
| ruled or proposed non-support / refusals | 32 | a decision, not a fix |
| PPI lexer/parser bugs worked around | 21 | upstream; each logged in `docs/ppi-upstream-bugs.md` |
| perf levers | 17 | by measured gain; most under the 20 %-of-a-row bar |
| design / plan / release | 15 | |
| parked or documented divergences | 13 | USER decisions |
| XS / pclxs | 10 | parked |
| other (ordinary bugs the keywords miss) | 387 | |

Roughly **600 of the 770 are bug-shaped** (the first two rows, the PPI row and most of "other"); the
rest are tooling, decisions and parked items.

**Rates.**  Closures ≈ 13.5 a day (81 in six days); filings ≈ 18 a day; of the filings, about half close
within the week (they are found while the batch that caused or exposed them is open) and the other half
join the backlog, which burns at ~5 a day.

## 3. The populations, measured

**perl-tests sweep** (108 files of perl's `t/op` and friends): 18,740 pass / 638 fail.  470 failures are
blessed with a cause; the largest causes:

| rows | cause |
|---|---|
| 57 | pack/unpack (PARKED by the USER 2026-09-06; 47 + 10 the `U` template) |
| 64 | warnings-gated diagnostics are absent (#221, ruled non-support) |
| 43 | `caller()` fidelity (#233 + #1432; DEFERRED by the USER s488) |
| 24 | error message text and format (ruled: not a goal) |
| 15 | errors for invalid Perl input (principle 9: PCL assumes valid Perl) |
| 11 | `DESTROY` called by the garbage collector, not at scope exit |
| 10 | UNITCHECK inside a string eval |
| 9 | `printf %n` |
| ~240 | everything else: 140 distinct causes of 1–8 rows each |

So about half of the blessed failures are decisions already taken, and the fixable residue is ~240 rows
spread over 140 causes: the long tail, one or two rows per fix.

**perl's own `t/` (the companion, 529 files):** OK 122, DIFF 249, XDIFF (expected) 103, NOTAP 31,
TIMEOUT 11, TRANSPILE-FAIL 10; PCL 94,418 ok rows against perl's 564,807.  The row gap is dominated by a
few enormous regex files (`re_tests` and its siblings, ~40,000 rows each) that hang at one row (#326) —
this population measures the engine's corners, not production code.

**CPAN board (14 dists, 183 files):** 1,941 ok / 251 not-ok; 75 files PASS, 32 PARTIAL, 76 FAIL.

| dist | files | ok | not-ok | verdicts | what holds it |
|---|---|---|---|---|---|
| Text-Balanced | 14 | 1,052 | 42 | 5 PASS 4 PARTIAL 5 FAIL | `@_` aliasing (#2860 closed s513a), pos/`\G` family |
| Class-Method-Modifiers | 29 | 69 | 10 | 21 PASS | method-modifier corners |
| Data-Dump | 15 | 139 | 2 | 12 PASS | nearly done (+7 rows by #2982 tonight) |
| Role-Tiny | 23 | 127 | 24 | 11 PASS 6 FAIL | #3001 (composition), filed tonight |
| Capture-Tiny | 24 | 37 | 12 | 7 PASS **17 FAIL** | #1509 (its block-form API from a computed export list) — the facts overlay batch s513g removes the drop; the files then stop on #3023 (`SEEK_END` on a `sysopen` handle) |
| Sub-Uplevel | 10 | 35 | **123** | 3 PASS | `caller()` fidelity — DEFERRED by the USER |
| Algorithm-Diff | 2 | 134 | 23 | 2 PARTIAL | |
| Try-Tiny | 11 | 63 | 8 | 6 PASS 3 FAIL | |
| Scalar-List-Utils | 38 | 0 | 0 | **38 FAIL** | the XS boundary: the dist tests its own XS build; PCL's `List::Util` / `Scalar::Util` shims serve programs and pass the everyday rows |
| Mojo-DOM58 | 5 | 1 | 0 | 1 PASS 4 FAIL | |
| the rest (Class-Inspector, File-Which, Safe-Isa, Sort-Versions) | 12 | 284 | 7 | mostly PASS | |

**Everyday battery (122 ordinary programs):** 114 identical.  The eight: DESTROY at scope exit (ruled),
`format`/`write` and `formline` (ruled), Archive::Tar compression (tie on a filehandle, #155), Digest
MD5/SHA (XS, parked), `readonly("lit")` (#1391), Carp's ` at FILE line N` (#1361), `uc("straße")`
(Unicode case folding, deferred).

**Which common modules load today** (`use M` under `./pcl`, 2026-10-10):

| loads | does not load |
|---|---|
| Data::Dumper, Digest::SHA (pure-Perl fallback), Time::HiRes (shim), IO::Socket::INET, JSON::PP, HTTP::Tiny, Getopt::Long, File::Temp, File::Path, File::Find, Term::ANSIColor, Time::Local, Text::Wrap, Text::ParseWords, List::Util, Scalar::Util, POSIX, Math::BigInt, Tie::File, Moo | **Encode**, **Storable**, Digest::MD5, Sys::Hostname, **File::Glob** (Compress::Zlib fails at it first, then at Compress::Raw::Zlib = zlib, XS -- corrected s514e), Hash::Util, Unicode::Normalize, DB_File — every one XS with no shim; `threads` (by design) |

## 4. What production needs that the test populations do not measure

Each item: the state, the owner task, and a size.  "Session" = one agent-day with review.

**4a. The core XS modules without a shim** — the biggest gap for production code, and the cheapest to
close for the modules that matter:

| module | why production needs it | shim size | note |
|---|---|---|---|
| **Encode** | any program decoding or encoding text; `use Encode` is in most non-ASCII programs | 1–2 sessions for UTF-8 / Latin-1 / ASCII (`encode`, `decode`, `encode_utf8`, `decode_utf8`, `is_utf8`, `:encoding(...)` layers already exist in the runtime) | PCL's string box already carries the byte/character distinction, so the shim is a flag flip plus a byte↔character conversion the runtime has; the long tail (CJK encodings) is a separate table set |
| **Storable** | caches, session files, `dclone` | 1–2 sessions for `freeze`/`thaw`/`dclone`/`store`/`retrieve` in pure Perl (PCL's own format; perl-file compatibility is a bigger, separate question) | `dclone` alone is a few hours |
| File::Glob | `use File::Glob ':bsd_glob'` is common; Compress::Zlib fails at it FIRST -- CORRECTED s514e (2026-10-10): with File::Glob shimmed it then dies at Compress::Raw::Zlib, which is zlib itself (XS); the compression family needs a zlib shim or pclxs, its own batch | hours: the builtin `glob` under the module's names (shipped s514e) | the zlib task is filed by s514e |
| Digest::MD5 | checksums | 1 session (a pure-Perl MD5 exists on CPAN; slow but correct) or un-park pclxs, whose Digest::MD5 passed 256/256 | |
| Sys::Hostname | logging | hours | |
| Hash::Util | `lock_keys` in defensive code | 1 session; needs a lock bit on the hash | |
| Unicode::Normalize | NFC/NFD of input | 1–2 sessions; tables from the oracle perl like `cl/pcl-uniprops.lisp` | |

**4b. `DESTROY` at scope exit** (`docs/not-supported.md`, design `docs/scope-exit-destroy-design.md`;
at-exit stage #2371 on the list, scope-owned stage **PARKED by the USER s495, #2370**).  Perl code
releases locks, closes handles, removes temp files and decrements counters in `DESTROY`, relying on
deterministic destruction; under PCL that runs at garbage collection or exit.  For a short script the
difference is invisible; for a long-running process it is a resource leak with no error message.  This is
the one parked ruling I recommend the USER revisit before Tier 2 is called production: the design is
staged and the sound first stage (at exit) is small.

**4c. Diagnostics and locations.**  Warnings-gated diagnostics are absent (#221, ruled): a production
log never sees "Use of uninitialized value".  Carp messages carry no ` at FILE line N` (#1361) and
`caller()` has four elements, not eleven (#233, deferred): log lines lose their location.  Error TEXT is
ruled not a goal; errors do fail in the same places (the s494 ruling).

**4d. Semantics still partial.**  `@_` aliasing through a method or coderef call copies the argument
(#2636, silent); `tie` on a filehandle is announced, not implemented (#155 — IO::Zlib, Archive::Tar
compression, some test tools); `local` on a nested element restores wrongly (#1190); `pos` on an
aggregate element (#396).  Each is a session; each is a known silent-wrong class.

**4e. Performance envelope.**  Warm start 40 ms and 28 MB RSS for hello-world (perl: 5 MB); the first
run of a program pays its transpile and compile, 0.5 s for 40 lines and ~3 s per 1,000 lines (#2423),
then reads its compiled file.  The bench table: most rows faster than perl (loops 0.2–0.4x, recursion
0.3x, hashes 0.3–0.7x, `foreach` reads 0.25x); slower rows that matter for production code: Moo object
construction 25x (`moo-objs`), `pack`/`unpack` 150x (parked), `print` 2.4x after tonight's round,
`lc`/`uc` on bytes 3.2x, JSON::PP round-trip 1.6x, sub-call overhead 1.7x, method return 1.1x.  A
Moo-heavy service constructing many objects would feel the 25x.

**4f. Not measured at all — and it should be before any production claim:** a soak test (a server-shaped
program for an hour: RSS over time, file descriptors, GC pauses); signal handling under load; `fork`
and pipes beyond the everyday rows; behaviour at memory pressure.  Filed as #2945.

**4g. Platform.**  Linux x86-64 only in CI (`ubuntu-latest`; the install matrix is one Ubuntu 24.04
image); SBCL ≥ 2.5.2; PPI ≥ 1.291.  The USER's own rule (2026-09-06) requires a macOS leg before a
platform-touching change ships; a production claim needs that leg to exist.  No Windows.

**4h. Process.**  v0.1.0 is tagged; CI is green on every merge; the installer has an end-to-end test
and a container test; every merge runs the gate, the sweep, the board, the companion and the everyday
battery.  What does not exist yet: a **supported-subset statement** (which modules and features a
program may use and expect perl's answers), a release cadence beyond the first tag, and a user-facing
diff-test command (the everyday tool does it for the corpus; a program's author needs `pcl --oracle
prog.pl`, which runs perl beside and diffs — a few hours of tooling).

## 5. The estimates, and what they assume

**"Go over the bug queue."**  Two different things:

- *Triage* — read all 770, merge duplicates, mark each IN or OUT of the supported subset, park the OUT
  ones with their reason: **two Fable sessions**.  Worth doing first, because it turns "770 open" into a
  number that can reach zero.
- *Fix the bug-shaped half* (~600) at 13.5 closures a day: **~45 working days — if nothing new were
  filed**, which does not happen; the review probes alone file ~2.3 silent-wrongs a day.  Realistically
  the open count stays between 600 and 800 for months while its *composition* improves.  The number to
  steer by is **known silent-wrongs inside the subset**: 105 today, found at ~2.3 a day and closed at
  roughly 5 a day (about 8 of the s513 night's 20 closures were silent-wrongs) → net −2.7 a day →
  **about 40 working days, two months**, to zero inside a fixed subset.  Opening a new population
  (another CPAN dist, the Rosetta corpus #2104) resets part of that clock, by design.

**"Usable for production."**

- **Tier 1 now**, with the diff-test gate, for scripts that use the modules in section 3's "loads"
  column and none of 4a/4b/4d.  Tier 1 **unsupervised** (no perl beside): when the subset's silent-wrongs
  are zero and Encode exists — **2–3 months**.
- **Tier 2 in 3–5 months**: the board's pure-Perl dists at PASS needs the Capture-Tiny chain (in
  flight), Role-Tiny (#3001), the Try-Tiny and Class-Method-Modifiers corners, and the two USER
  decisions (4b DESTROY, 4c caller).  Without those decisions Sub-Uplevel's 123 rows and the RAII idiom
  stay out, and "production" has to be stated with that exclusion.  Moo's 25x construction cost needs
  its design (not scheduled).
- **Tier 3 is not on a measurable path** while pclxs is parked.  The shim route (4a, 6–10 sessions) gives
  the six core XS modules production code actually needs without the bridge; DBI, JSON::XS and the rest
  of XS CPAN stay out until the bridge is restarted (it reached ABI-6 with Digest::MD5 fully passing).

**Assumptions behind every number:** three agents a night and one review session a day (the rate scales
with the cap — at two agents multiply the durations by 1.5); the discovery rate stays ~2–5 findings per
reviewed batch; no new steering population is opened without resetting the clock; the USER's parked and
deferred rulings stand unless revisited.

## 6. What I recommend, in order

1. **Encode shim** (4a) — one to two sessions, the largest single production unblocker.
2. **Triage pass** (5) — two Fable sessions; output: `docs/supported-subset.md` and every open task
   marked IN/OUT.
3. **`pcl --oracle`** — the diff-test on-ramp as a shipped command: **ALREADY SHIPPED as `pcl --check` (task #2194, s494k; `docs/pcl-check.md`) -- this item was stale when written (corrected s514).**
4. **Revisit #2370** (scope-owned DESTROY) with the staged design — the USER's call; at-exit (#2371)
   first either way.
5. **Storable, File::Glob, Sys::Hostname, Digest::MD5 shims** — three to four sessions, in that order.
6. **The soak test** (#2945) before any production wording in the README.
7. Keep the interleaved cadence (one perf round per correctness round); the perf table is already
   ahead of perl where ordinary programs spend their time, and Moo construction is the one row that
   needs a design.

## 7. Where the numbers came from

Task store: `~/.claude/tasks/pcl/*.json` (statuses, `created`), `git show 9fda1646:…` for the 10-04
snapshot.  Sweep: `baselines/fail-baseline.tsv` (470 rows, cause column).  Companion:
`baselines/perl-suite-run.tsv`.  Board: s513f's `scratch/s513f/board-tree.tsv` (the tree of main
`dbcf19dc`).  Everyday: `baselines/everyday-history.tsv`, `baselines/everyday-baseline.tsv`.  Modules:
`probe.sh ./pcl -e 'use M; print "ok"'` for 31 modules.  Timings: `/usr/bin/time -f '%e s %M KB'`
under `probe.sh`, warm, three runs.  Velocity: `git log`, `docs/session-log.md`.
