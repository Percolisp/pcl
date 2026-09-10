# The critical-infrastructure milestone — measured

*Task #1607, measured in session s481b (2026-09-10) on main `f330e885`
(generation v2-1220; the tree this doc lands on is v2-1280), perl 5.40.3 as the oracle.  This is the list the README's
hedge quotes: **Percolisp is an experiment until the critical infrastructure
runs.**  The USER prunes or extends the list; the count that run is the public
progress meter.*

## 0. The rule

**Perl is the oracle and ROWS are the measure.**  A module *runs* when its OWN
`t/` produces, under PCL, the verdicts real perl produces on the same machine,
file by file and assertion by assertion.  PASS/PARTIAL/FAIL labels are not the
measure — a file can be labelled PASS on two of perl's twenty rows — so every
row below carries `ok / not-ok` counts for both sides.

Three commands per distribution, logs under `scratch/s481b/` of the measuring
worktree (`<dist>.transpile.log`, `<dist>.tsv`, `<dist>.rows.tsv`,
`<dist>.perl.tsv`):

* **transpile** — every `lib/**/*.pm` through `pl2cl --module`, counting files,
  hard errors, `PCL: statement dropped` announcements and ruled refusals;
* **PCL's own `t/`** — `tools/cpan-scoreboard.pl --jobs 4 --timeout 120`, the
  runner the 14-dist CPAN board uses;
* **perl beside it** — the same files, the same `@INC` rule, CWD = the dist root.

A file that produces no rows **under perl too** (an author test, a missing
optional dependency, a network test) is `PERL-SKIP` and is never counted as a
PCL failure.  Those files are named per row.

**The threshold used for "runs":** every `t/` file that perl does not skip
produces under PCL the same ok / not-ok counts perl produces.  Nothing weaker.
A module a deployment depends on is not "running" at 90 % of its own
assertions — and the two nearest misses below (Try::Tiny at 93 of perl's 104,
Data::Dumper at 406 of 829) are exactly why a softer threshold would be
arbitrary.

**One measurement caveat that costs rows and is not the compiler's fault:**
`tools/run-dist-t.pl` does not `chdir` to the dist root (task **#1575**), so a
test using a relative path fails for a reason that is not PCL's.  It is called
out where it bites (JSON::PP's sixteen `099_binary*.t` files).

## 1. The list, module by module

### Group A — pure Perl

| module (dist) | transpiles? | PCL's own t/ | perl beside it | blocker | est. |
|---|---|---|---|---|---|
| **Getopt::Long** (Getopt-Long-2.58) | 2 pm, 0 drops, 0 refusals | 8 PASS / 0 / 0 — **109 ok / 0** | 8 PASS — 109 ok / 0 | **none — RUNS** | — |
| **Try::Tiny** (Try-Tiny-0.32) | on the CPAN board | 6 PASS / 2 PARTIAL / 3 FAIL — 93 ok / 5 | 10 PASS, 1 PERL-SKIP — 104 ok / 0 | NS `given`/`when` (a whole file), #1571 (`Sub::Util::subname` of a `try` block; one `finally` ordering row), NS error-message text | S |
| **Data::Dumper** (Data-Dumper-2.183, PP half) | 2 pm, 0 drops | 19 PASS / 2 PARTIAL / 5 FAIL — 406 ok / 66 | 25 PASS, 1 PERL-SKIP (`huge.t`, wants 10 GiB) — 829 ok / 0 | **`re::is_regexp` is missing** — four files stop at it; `Data::Dumper::Dumpxs` is the XS half (deparse.t); `Can't handle 'Regexp' type` (qr.t) | S–M |
| **HTTP::Tiny** (HTTP-Tiny-0.090) | **DID NOT COMPILE AT ALL** before this batch; now 1 pm, 0 died, 0 drops, 0 refusals | 1 PASS / 0 / 31 FAIL — 1 ok / 1 (unchanged by the fix) | 28 PASS, 4 PERL-SKIP (live network) — 455 ok / 0 | was `while ()` → "Parser2: empty expression", **FIXED HERE**; the blocker is now **#1574** — every file dies at `require IO::Socket` with "Undefined subroutine &IO::Socket::UNIX::AF_UNIX" | M |
| **Path::Tiny** (Path-Tiny-0.150) | 1 pm, 0 drops | was 1 PASS / 0 / 29 FAIL — 1 ok / 0; **after the fix 9 PASS / 2 PARTIAL / 19 FAIL — 41 ok / 40** | 29 PASS, 1 PERL-SKIP (`zz-atomic.t`, wants Test::MockRandom) — 1779 ok / 0 | was `File::Spec->canonpath` missing from PCL's shim, **FIXED HERE**; the residue is **`subtest` (#1576)** (`t/parent.t`: "Undefined subroutine &main::subtest") plus per-file failures | M |
| **JSON::PP** (JSON-PP-4.18) | 2 pm, 0 drops | 22 PASS / 19 PARTIAL / 24 FAIL — 815 ok / 95 | 65 PASS — 25891 ok / 0 | **#1609** — the negated POSIX class `[:^cntrl:]` is untranslated, so every control character encodes as a RAW byte and the JSON is invalid; plus #1575 for the sixteen `099_binary*.t` | M |
| **Moo** (Moo-2.005005) | 13 pm, 0 drops | 37 PASS / 5 PARTIAL / 29 FAIL — 380 ok / 26 | 71 PASS — 841 ok / 0 | **#1611** (Test::Fatal's `exception` collides with a runtime export), an `s///e` replacement the compiler refuses (croak-locations.t), `Class::XSAccessor` unloadable, DEMOLISH / global-destruction rows | M–L |
| **Test::More** (Test-Simple-1.302199) | 71 pm, 0 drops | 2 PASS / 0 / 2 FAIL — 4 ok / 0 | 4 PASS — 86 ok / 0 | the dist's real suite lives under `t/Test2/`, `t/Legacy/` … which the scoreboard's `t/*.t` glob does not reach; the feature gap is **`subtest` (#1576)** | M–L |
| **File::Temp** (File-Temp-0.2312) | 1 pm, 0 drops | 6 PASS / 0 / 5 FAIL — 46 ok / 1 | 10 PASS, 1 PERL-SKIP (`lock.t`, no O_EXLOCK) — 132 ok / 0 | the glob-as-object idiom `${*$fh}{…}` → `Cannot dereference non-reference: #<fd-stream …>` inside `(SETF P-CAST-$)` | M |
| **Text::CSV_PP** (Text-CSV-2.04, PP half) | 2 pm, 0 drops | 0 PASS / 0 / 38 FAIL — 0 ok / 31 | 37 PASS, 1 PERL-SKIP — 52556 ok / 0 | **Encode** (XS) — `use Text::CSV` dies "Can't locate loadable object for module Encode" and nothing else is reached | (group B) |
| **YAML::Tiny** (YAML-Tiny-1.74) | 1 pm, 0 drops | 1 PASS / 1 PARTIAL / 11 FAIL — 4 ok / 1 | 12 PASS, 1 PERL-SKIP — 57 ok / 1 | its `t/lib` bridge needs **`subtest` (#1576)** and reaches an `XSLoader::load()` with no arguments | M |
| **Pod::Usage** (Pod-Usage-2.05) | 3 pm, 0 drops | 4 PASS / 0 / 2 FAIL — 6 ok / 42 | 6 PASS — 49 ok / 0 | **Encode** (XS), reached through Pod::Simple / Pod::Text | (group B) |

*Pod::Usage keeps its tests in `t/pod/`, which the scoreboard's `t/*.t` glob
does not reach; those six files were run one at a time on both sides.*

### Group B — XS through pclxs

**The whole group is closed by one thing today, and it is upstream.**
`tools/pcl-xs-install` refuses every distribution:

```
pcl-xs-install: xs-pin says abi 6 but /home/bernt/pclxs is abi 8.
  Run tools/build-pclxs --pin-here after checking the adapter.
```

`~/.pcl-cache/xs` does not exist (`--list`: "cache is empty"), so no XS
artifact can be built or installed, and every XS module fails at
`XSLoader::load` with *"Can't locate loadable object for module …"*.  pclxs is
under separate development and its 13 rows in PCL's gate are the standing
failure; this table records where each module stops and fixes nothing.

| module | Perl half transpiles? | `pcl-xs-install` | load under PCL | blocker | est. |
|---|---|---|---|---|---|
| **DBI** (DBI-1.653) | **`DBI.pm` transpiles CLEAN** — 8682 lines, 0 drops, 0 refusals | refused (ABI) | **dies at READ time: `Package *dbi does not exist`** | **#1610** — `local(*DBI::DIR, $@)` emits a bare CL symbol and the whole emitted file becomes unreadable.  The pure-Perl route (`DBI_PUREPERL=2` + DBD::DBM) needs **no XS at all** and runs under perl with `-I` alone, so #1610 is the only thing between PCL and a running DBI program | M |
| **Encode** (Encode-3.24) | — | refused (ABI) | "Can't locate loadable object" | pclxs ABI — **the highest-value one: it also blocks Text::CSV_PP and Pod::Usage** | L |
| **Storable** (Storable-3.41) | — | refused (ABI) | "Can't locate loadable object" | pclxs ABI | L |
| **Time::HiRes** (Time-HiRes-1.9764) | — | refused (ABI) | "Can't locate loadable object" | pclxs ABI | M |
| **Digest::SHA** (Digest-SHA-6.04) | — | refused (ABI) | **loads**, but `Digest::SHA::sha1_hex` is undefined | pclxs ABI; Digest::MD5's recipe (#115) is the template | M |
| **JSON::XS / Cpanel::JSON::XS** | — | refused (ABI) | Cpanel: "Can't locate loadable object" | pclxs ABI | M |
| **POSIX** | shim `lib/POSIX.pm` | n/a | **loads**; `POSIX::floor` correct, **`POSIX::strftime` undefined** | a shim gap (module fact — `lib/POSIX.pm`) | S |
| **Socket / IO::Socket** | shim `lib/Socket.pm` | n/a | `Socket` loads; `AF_INET`, `AF_UNIX`, `inet_aton` all correct; **`require IO::Socket::INET` dies** in `Symbol::gensym` — `#S(p-typeglob …) is not a hash-table` | NS *Live symbol-table hashes* (`%Symbol::`).  **#1574's diagnosis needs correcting**: `lib/Socket.pm` DOES export `AF_UNIX` and `IO::Socket::UNIX` loads on its own | M |
| **List::Util / Scalar::Util** | shims | n/a | load; `reduce`, `blessed` correct | already on the 14-dist board (#1571 / #1573) | S |

### Group C — frameworks

| dist | transpiles? | PCL's own t/ | perl beside it | blocker | est. |
|---|---|---|---|---|---|
| **Log::Log4perl** (1.58) | 51 pm, 0 died, 0 drops, 0 refusals | 13 PASS / 1 PARTIAL / 61 FAIL — 117 ok / 4 | 58 PASS, **17 PERL-SKIP** (Log::Dispatch, DBI, XML::DOM, RRDs … are absent) — 752 ok / 0 | **#1616** — a hash deref of a plain scalar raises a raw SBCL type error where perl (without `strict refs`) returns undef.  Sampled `002Logger.t`, `003Layout.t`, `004Config.t`: all three die at it with zero rows | M |
| **Template Toolkit** (3.106) | 46 pm, **4 DIED** — three `PCL: cannot compile the s///e replacement` (Filters.pm, Plugin/String.pm, VMethods.pm) and one `cannot compile interpolated regex reference '${ … }'` (Parser.pm) | 2 PASS / 3 PARTIAL / 112 FAIL — 77 ok / 15 | 9 PASS / 59 PARTIAL / 31 FAIL — 414 ok / 26 | the four refusals above.  **Caveat: the perl side is degraded too** — the dist is unbuilt (no `Makefile.PL` run, so no `blib` and no `Template::Stash::XS`), so 108 of its 117 files are not clean under perl either; the comparison is honest but the denominator is small | L |
| **DateTime** (1.67) | 9 pm, 0 died, 0 drops | 1 PASS / 0 / 50 FAIL — 1 ok / 1 | **1 PASS / 0 / 50 FAIL — 1 ok / 3** | **NOT MEASURABLE on this machine**: the dist's own dependencies (`namespace::autoclean`, `Specio`, `Params::ValidationCompiler`) are not installed, so real perl fails 50 of 51 files as well.  DateTime also has an XS half; the pure-Perl mode (`PERL_DATETIME_PP=1`) needs the same dependency tree | ? |
| **Mojolicious** (9.49) | **112 pm, 0 died, 0 drops, 0 refusals** | its tests live in `t/*/` (107 files), which the scoreboard's `t/*.t` glob does not reach; not run, because `require Mojolicious` fails | — | four separate loads, measured per module: `Mojo::Util` and `Mojolicious::Controller` → **Digest::MD5** (XS); `Mojo::File` → **#1617** (core `File::Copy` is refused); `Mojo::IOLoop` → `getaddrinfo` is not exported by `lib/Socket.pm`; `Mojolicious::Routes` → an argument-less `XSLoader::load()` outside an eval.  `Mojo::Base` alone LOADS | L |
| **Plack** (1.0054) | **70 pm, 0 died, 0 drops, 0 refusals** | tests in `t/*/` (137 files); not run | — | `Plack::Util` LOADS under PCL; `Plack::Request` needs `HTTP::Headers::Fast`, which is **not installed for perl either**, so the dist cannot be measured here without pulling its dependency tree | L |

**The compiler is not the problem for the two big frameworks.**  Mojolicious's
112 modules and Plack's 70 transpile with zero hard errors, zero dropped
statements and zero ruled refusals; what stops them is XS and two shim gaps.

## 1a. What this batch fixed, and what it bought

Two blockers were small enough to ship under the filler rule (no new
mechanism, guard rows inverse-verified against `f330e885`):

* **`while ()` is perl's infinite loop.**  PCL died "Parser2: empty
  expression", a HARD transpile error that loses the whole file.  The C-style
  `for` arm already spelled the rule ("empty cond → constant true"); `while`
  now reads it too.  `HTTP/Tiny.pm` went from *does not compile* to 1 pm file,
  0 drops, 0 refusals.  Guard `Pl/t/while-empty-01.t` (9 rows).
* **`File::Spec->canonpath`.**  PCL's Unix-only `File::Spec` shim invites
  exactly this ("keep it in sync with the real File::Spec::Unix as methods are
  needed") and `Path::Tiny::_path` — the constructor every Path::Tiny object
  goes through — calls it, so all 30 files died at load.  Twelve paths now
  answer byte-identically to real File::Spec 5.40.3.  Guard
  `Pl/t/file-spec-01.t` (4 rows).

Re-measured after the fixes, same commands:

* **Path::Tiny 1 PASS / 0 / 29 FAIL, 1 ok → 9 PASS / 2 PARTIAL / 19 FAIL,
  41 ok.**  `t/basename.t` is now 6 ok / 0, exactly perl's.
* **HTTP::Tiny compiles but its row count does not move** (1 ok, 1 not-ok):
  the file-level blocker was replaced by a load-level one, #1574's
  `require IO::Socket`.  Worth stating plainly — a fix that removes the first
  wall reveals the second, and only the second number is progress.

Neither module *runs* yet — the remaining causes are named in §1 — but both
moved from "the compiler could not read it" to "a suite with failures", which
is where the next session can work.

## 2. The count

**One of twenty-one runs today: `Getopt::Long`.**

That is the count over the twenty-one modules the README's hedge names.  This
table measured twenty-six (the README's list plus File::Temp, Text::CSV_PP,
YAML::Tiny, Pod::Usage and the List::Util / Scalar::Util shims), and the answer
is the same one: **1 of 26**.

The threshold is §0's: every `t/` file that perl does not skip produces the same
ok / not-ok counts under PCL.  Getopt::Long meets it exactly — 8 files, 109
assertions, 0 failures on both sides, not one row of difference.

**On the other side of the line, nearest first** (the number is *assertions perl
produces that PCL does not*, over the files perl does not skip):

| module | short by | why it is not "runs" |
|---|---|---|
| Try::Tiny | **11** | one whole file refused (`given`/`when`), three `subname` rows, one `finally` ordering row |
| Pod::Usage | 43 | one file gives 42 not-oks instead of 42 oks |
| YAML::Tiny | 53 | eleven of thirteen files produce nothing |
| Test::More | 82 | two of four files produce nothing (and its real suite is not even in `t/*.t`) |
| File::Temp | 86 | five files produce nothing, four stop after one or three rows |
| Template Toolkit | 337 | four modules do not compile |
| Data::Dumper | 423 | four files stop at the first blessed structure |
| HTTP::Tiny | 454 | compiles now, but every file dies at `IO::Socket` |
| Moo | 461 | twenty-nine files produce nothing |
| Log::Log4perl | 635 | forty-four of its fifty-eight countable files produce nothing |
| Path::Tiny | 1,738 | nineteen files still produce nothing |
| JSON::PP | 25,076 | forty-three of sixty-five files short or empty |
| Text::CSV_PP | 52,556 | the module does not load at all |
| DBI, Mojolicious, Plack, Encode, Storable, Time::HiRes, Digest::SHA, JSON::XS, POSIX, Socket | — | do not load, so there is no row count to be short by |
| DateTime | — | not measurable here: perl fails 50 of 51 files too (absent dependencies) |

Total shortfall over the measurable modules: **81,955 assertions**, of which
77,632 are two modules (Text::CSV_PP and JSON::PP) and therefore two causes.

## 3. Blockers, ranked

Ranked by the assertions behind them, with the owning task.  This is the queue
the count moves along.

1. **The XS route is shut — `xs-pin` says abi 6, `~/pclxs` is abi 8** —
   *52,599 assertions* (Text::CSV_PP 52,556 + Pod::Usage 43) **plus every
   group-B module's own suite**.  Nothing can be installed with
   `tools/pcl-xs-install` and every `XSLoader::load` fails.  Encode alone is
   worth more than any compiler fix on this list.  Owner: pclxs (separate
   development) + the pin.
2. **#1609 — the negated POSIX class `[:^cntrl:]` is untranslated** —
   *up to 25,076* (JSON::PP).  Every JSON::PP encode of a control character
   produces invalid JSON.  Of those rows 24,576 sit in sixteen files that also
   need #1575 below, so the two must land together to be visible.
3. **#1576 — `subtest`** — *≥ 1,873* (Path::Tiny 1,738 in part, YAML::Tiny 53,
   Test::More 82).  Modern suites are written in it; a file that uses it
   produces no TAP at all.
4. **#1616 — a hash/array deref of a plain scalar dies** — *635*
   (Log::Log4perl).  Under `no strict refs` perl returns undef and runs on.
5. **#1611 — a user sub whose name is a runtime export is hijacked** — *≥ 200*
   (Moo, through Test::Fatal's `exception`).  Silent-wrong for 34 names.
6. **#1574 — `require IO::Socket`** — *454* (HTTP::Tiny), and everything that
   opens a socket.  See #1615: the export list is not the cause.
7. **#1612 — `re::is_regexp` is missing** — *423* (Data::Dumper).
8. **The `s///e` replacement refusal and `${ … }` interpolated regex
   references** — *337* (Template Toolkit's Filters/VMethods/Plugin::String/
   Parser), and Moo's `croak-locations.t`.
9. **#1614 — a glob used as per-handle storage (`${*$fh}{K}`)** — *86*
   (File::Temp), and the IO::Handle family.
10. **#1610 — `local *Pkg::NAME` makes the emitted file unreadable** — it is the
    ONE thing between PCL and a running DBI program, because `DBI_PUREPERL=2`
    with DBD::DBM needs no XS at all.  No row count, because nothing loads.
11. **#1617 — core `File::Copy` cannot be loaded** — Mojo::File and every
    program that copies a file.
12. **#1575 — the dist runner does not `chdir` to the dist root** — *24,576
    planned rows* in sixteen JSON::PP files.  A RUNNER fix, not a compiler one,
    and the cheapest item on this list.

Two shim gaps with no rows behind them yet but a low price: **#1613**
(`POSIX::strftime`) and `getaddrinfo` in `lib/Socket.pm` (Mojo::IOLoop).
