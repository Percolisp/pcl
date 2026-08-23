# s436 (Opus 5, 2026-08-23) — Q2 CLOSED, #471, #457, and the flip's real worst case

Three pieces, three commits.  **Q2's four unrun bar legs are run and clean**;
**#471** (the compiler-side memory cap) is the ruled filler; **#457** was
PROMOTED out of Q7 by its own measurement — the flip's biggest single price in
the whole project turned out to be one PPI token, and fixing it takes that
price back.

Asks are in §6.

## 1. Q2 leg 1 — the four-population emission A/B: clean, and it found a hole

`tools/emission-ab.pl --ref 9138404` (the commit before the flip) over the
three list populations, plus `corpus-diff` for the fourth (re-run, unchanged
from s435: 4 files, SILENT-DROP 5):

| population | files | DIFF | flip sites |
|---|---:|---:|---:|
| `lib/**.pm` | 22 | 0 | 0 |
| `cpan-tests/modules/**` (`.pm` + `.t`) | 402 | 52 | 107 |
| `$PCL_PERL_SUITE_T/*/*.t` | 604 | 24 | 79 |

**Every DIFF was shape-checked mechanically, not by eye.**  A walker over each
pair requires that the two files agree line for line except where side A has
` nil<closers><suffix>` and side B has the `p-die` spanning to
`")<closers><suffix>` — i.e. exactly the flip's one-line substitution and
nothing else.  186 flip sites, 76 files, **0 unexplained lines**.  (The first
two versions of that checker were wrong in ways that read as failures; the
shape it settled on is in the session scratch.  Worth knowing: the `nil` can
carry a TAIL — `1 while …` emits ` nil) 1)` — so a checker anchored at
end-of-line is too strict.)

**THE HOLE (#473, new).**  The A/B is a different instrument from the census —
it byte-compares two compilers over a file LIST, so it sees every file the flip
touched, whatever the census defines — and the two disagree:

* the 9 cpan `.pm` DIFFs reproduce the census's 9 rows COUNT FOR COUNT (15
  sites), and the 24 suite DIFFs reproduce its 23 rows count for count;
* **43 cpan-tests `.t` files / 92 sites** are in no population the census
  defines (`tools/drop-census.pl`'s `cpan-tests` population is `**/*.pm` only —
  module mode, the emission the runtime caches);
* **`t/japh/abigail.t` / 2 sites** — the companion population follows
  `run-perl-suite.pl`'s `@DEFAULT_DIRS`, where `japh` is excluded.

Neither is a regression (both were dropping silently before), but 94 sites that
now DIE are uncounted, which is the sin #462 closed for the module populations.
The census header says so until #473 decides; the recommendation there is
program-mode for the `.t` half and documentation for japh.

## 2. Q2 legs 3 + 4 — the confirming sweep, and 13 snapshot rows edited

**Sweep (`--jobs 8`, no file arguments, so its own gate ran): TOTAL passing
baseline 18311, current 18311 (+0); 0 new, 0 fixed; drops census 5 = current 5;
GATE clean.**  s435's edited baselines reproduce exactly — substr.t 347/3 (ran
353 of 397), method.t 72/26 (ran 124 of 163).

**`docs/perl-suite-run.tsv`: the 13 rows edited BY HAND** with an s436 header
block naming each cause.  Each was measured three times (s435's `--all --quick`
+ two `--jobs 1` passes here) and the 10 the runner re-ran ALONE all came back
"REAL MOVE (both runs agree)".

**One correction to s435's §4: the companion price is −114 C_ok over 9 files,
not −117 over 11.**  `io/pvbm.t` does not move — three serial runs give its
blessed 23/5; s435's `--all --quick` reading of −3 was CONTENTION, exactly the
failure mode #366 exists to catch, and it slipped through because that file was
below the parallel run's own re-run threshold.  Two files change STATUS
(op/signatures.t, re/reg_eval.t: XDIFF → DIFF) and are deliberately NOT
re-blessed into `perl-suite-expected-rows.tsv` — those rows are lost to an
aborted form, not to the registered not-supported reason, and blessing them
would hide the flip's price behind an "expected divergence" label.

**A measurement-instrument fix went in with them.**  Since the flip the
commonest aborted-form condition begins with an absolute path, and the
directory prefix alone overran the signature's 90-character budget: every
drop-caused abort came back as `…/perl-N.N.N/t/o`, naming nothing — the one
thing a signature exists to do.  `run-perl-suite.pl` now collapses a DEEP
absolute path to its basename (two or more components, only where a path can
start, so `s/a/b/` inside quoted source text and a shallow `"/dev/tty"` are
untouched).  **Verdict-neutral by construction**: `read_snapshot` compares
status + `C_ok` + `C_notok` and never `$sig`, and the collapse can never empty
a non-empty signature.  `PCL_SHOW_SBCL=1` before/after: identical.  The rule is
now in `docs/test-infrastructure.md`.

## 3. Q2 leg 2 — the board re-run, and the flip's real worst case

Base-vs-HEAD board A/B on the three dists with censused drops (the blessed
`docs/cpan-board14-s378.tsv` is far older than the tree, so a diff against it
would have attributed other sessions' progress to the flip):

* **Mojo-DOM58 and Sub-Uplevel: byte-identical**, base and head.
* **Text-Balanced: every one of its 9 real test files went PASS/PARTIAL →
  FAIL, 958 passing rows → 0.**

Not test rows — **`use Text::Balanced` itself dies**:

```
not ok 1 - use Text::Balanced;
#     Error:  PCL: statement not supported at …/Text/Balanced.pm line 118:
#             $escs .= substr($escs,-1) x (length($dels)-length($escs));
```

The plan predicted movement "where line 118/397 is reached".  What it did not
have is that **line 118 is inside `gen_delimited_pat`, which the module's own
top level CALLS at line 308** — so the drop is reached during `use`.  That
refines the s433 ruling's sentence "a module with a drop still loads": the LOAD
is fine, but a load-time call into a sub whose body carries a drop takes the
module with it.  With Text::Balanced that is 958 rows from one token, the
largest single price the flip has anywhere.

Per the standing rule (a loss out of proportion PROMOTES its drop's owner
task), **#457 was promoted out of Q7 and fixed in this session** — §4.

## 4. #457 — `)-name` is subtraction, not a negative bareword

`Pl::Parser2::_repair_minus_word`, beside `_repair_glob_multiply`, same
predicate: after a token that `_ends_term`, a `PPI::Token::Word('-name')` is
split back into `- name` on the raw stream and the document is reparsed.  The
third sibling of PPI bugs §12 (`)*name`) and §15 (`)-1`).

* **The condition is a NEGATIVE**, which is what makes it safe: perl's
  `-bareword` string form can only start where a TERM can.  Probed against perl
  and identical: `(-foo => 1)`, `foo(-bar)`, `$h{-x}`, `1, -bar`, `"x" . -foo`,
  `-M "/path" ? …`, plus the positives `length("abc")-length("a")`,
  `$a[1]-length("ab")`, `$h{a}-length("abc")` and both Text::Balanced lines.
* **Rule 13 paid in the same commit**: `docs/ppi-upstream-bugs.md` §25 and two
  rows in `docs/ppi-bug-report.t` (the bug row FAILS on 1.291, the spaced
  control passes; plan 29 → 31).
* Guard `Pl/t/minus-word-01.t` (10 rows, every one a `both_agree` against live
  perl).
* **Emission A/B against the flip commit: `lib` 0/22, `cpan-tests` 0/402,
  perl's `t/` 0/604, `corpus-diff` identical over 111** — the shape really does
  occur nowhere in the repo populations, which is why nothing in the tree could
  have guarded it and why the guard file above is the guard.
* **Board: Text-Balanced back to the base run, row for row** (958 passing).

## 5. #471 — the compiler side gets a memory cap

Ruled shape, at the `pl2cl` seam.  `pl2cl` re-execs itself ONCE through
`sh -c 'ulimit -v N; exec …'` before PPI is loaded, replaying `/proc/self/cmdline`
so perl's own switches survive (`$^X, $0, @ARGV` silently drops the
`-I<root>` the suite runner spawns us with).  `PCL_MEM_CAP_MB` /
`PCL_NO_MEM_CAP` / `PCL_SHOW_MEM_CAP`.

* **The number is measured, not guessed**: the heaviest legitimate transpiles
  peak at ~140 MB of address space (perl-tests/signatures.t 140, re/pat.t 103,
  perl-tests/pack.t 96, cl/pack-impl.pl 86), so the 4096 MB default — the same
  number `run-perl-suite.pl` already puts on its children — is ~30× headroom.
* **Acceptance, run for real**: a scratch copy of the tree with a
  self-recursive helper in `Pl::Parser2::parse` dies in **3.9 s** with
  `Deep recursion on subroutine "Pl::Parser2::_pcl_runaway_probe" … / Out of
  memory in perl:util:safesysmalloc`, and `free` shows the swap untouched.
  That is the same bug that took the machine down in s435.
* **Cost: unmeasurable** — 20 tiny transpiles, 2.276 s capped vs 2.287 s
  uncapped.  Emission byte-identical (`corpus-diff` over 111 files).
* Exempt: `--bundle`/`--executable`, which spawn SBCL — SBCL RESERVES a
  multi-gigabyte address space and `ulimit -v` counts reservations.
* Guard `Pl/t/mem-cap-01.t` (9 rows), including one that proves the limit is
  ENFORCED and not merely reported (a 16 MB cap must stop the process).

## 6. ASKS

1. **#471's residue: the ~40 `Pl/t` files that `use Pl::Parser` IN PROCESS are
   still uncapped**, because they never spawn `pl2cl` — and an in-flight edit
   hangs the machine from the gate exactly as it did from a bare tool run.  The
   obvious complement is the pattern already in `run-perl-suite.pl`: wrap
   `tools/prove-core` in `systemd-run --user --scope -p MemoryMax=…` (RSS-based,
   so SBCL's address-space reservation is irrelevant, ~30 ms once, covers every
   process the gate starts).  I did NOT do it — the ruling named the `pl2cl`
   seam and I am not widening a ruled shape on my own.  Is the prove-core scope
   in or out?
2. **#473: does `cpan-tests/modules/**/t/*.t` become a census population?**
   43 files / 92 drops, in PROGRAM mode (they are a dist's own test suite).
   Cost ~350 more transpiles per census run.  My recommendation is yes for the
   `.t` half and a header sentence for `japh`.
3. **The s433 sentence "a module with a drop still loads" needs its second
   half** (§3): true of the load, false when the module's own top level calls a
   sub whose body carries the drop.  Should `docs/ir-spec.md` §9.3 carry that,
   or is the census header enough?
4. **Is a promoted owner task's fix allowed to ride in the same session as the
   measurement that promoted it?**  #457 was Q7's first filler; its own bar
   (probes + A/B + board) is met and it takes back 958 rows, so leaving it for
   a later session would have meant shipping a known 958-row hole on purpose.
   I read the standing rule as licensing exactly this, but it is the first time
   the promotion rule has actually fired.
5. **#472 is still unmeasured** (the fresh_perl/runperl CHILD population).  Two
   known members, both rows that had been passing on nothing for years.  It is
   the last uncounted drop population after #473.

## 7. What is NOT done

* **Q3 (#456 half (b) = #469, the PHASE model)** — not started beyond reading
  the assembly loop.  Its own measurement is listed in `plan-post-s433.md` §Q3.
  The shape of the change is clear (two passes over `@sections`: every
  section's compile-phase forms, then every section's run-phase forms), and the
  probe in this session's own scratch confirms the bug is live:
  `print +(bless {}, 'C')->n-1` with `package C; sub n {7}` BELOW it dies
  "Can't locate object method", and works when the package is above.
* `docs/cpan-board14-*.tsv` is not re-blessed — the board A/B was run against a
  worktree, not against the stale s378 file, and re-blessing the whole board
  needs the 14-dist run.
