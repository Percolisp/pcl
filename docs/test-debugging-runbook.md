# Runbook: debugging perl-tests failures & migrating skips

Operational step-by-step for future sessions. The *mechanism* (registry, failure log,
sweep-diff, four-bucket taxonomy) is documented in `docs/test-skip-registry.md`; this file
is the *procedure* to follow.

## 0. One-time orientation each session
```sh
perl sweep-perl-tests.pl --jobs 8            # writes .faillog/*.fails.tsv (Pass/Fail/Skip)
tools/sweep-diff.pl .faillog                 # per-file fail counts (where to work)
tools/sweep-diff.pl diff baselines/fail-baseline.tsv .faillog   # what changed since baseline
```
`Fail` is the only number that matters (skips are documented; crashes are separate). Guard
the **Fully-passing** count — if it drops, find the regression before anything else.

## 1. Debug ONE file (the inner loop)
```sh
PCL_TEST_LOG_DIR=/tmp/fl ./runt <file>       # or just `./runt <file>` then read .faillog
cat .faillog/<file>.fails.tsv                # file⇥num⇥description⇥got⇥expected
```
For **each** failing line, read `got` vs `expected` and route it with the decision tree (§2).
The got/expected column usually tells you the cause without opening anything else. If you
need the generated CL: `./clt <file>` (no SBCL) or `./runpcl` for a snippet.

## 2. Decision tree: FIX the bug, or REGISTER it as not-supported?

Route each failure by what `got`/`expected` shows:

- **`got` is a Perl-plausible wrong value** (wrong number/string/order) → **real bug → FIX it.**
  Reproduce minimally with `./runpcl`, fix in `Pl/*.pm` or `cl/pcl-runtime.lisp`, add a
  `Pl/t/*-01.t` regression test (CLAUDE.md principle 6).
- **`got` leaks an SBCL artifact** (`#S(P-BOX …`, `… is not an array with a fill pointer`,
  `The function … is undefined`) → PCL crashed where Perl would error cleanly. Usually a
  *real bug* (emit a clean Perl-style die) unless the test is pure error-detection (§below).
- **The test only checks an error/warning message** (`expected` is `^Can't modify…`,
  `^Modification of a read-only value`, `… INVALID`, "warns once", etc.) and PCL doesn't
  produce it → **not-supported (principle 9 / error-msg) → REGISTER**, citing
  `docs/not-supported.md` 'Error compatibility for invalid Perl input' or 'Error message text'.
- **Feature is in `docs/not-supported.md`** (read-only scalars/Internals, `@_`/element/ref
  aliasing, lvalue subs/substr, DESTROY-via-GC, `use bytes`, utf8 flag/`utf8::encode`,
  tie internals, `(?{code})`, format/write, given/when) → **REGISTER**, citing the section.
- **Nothing matches** → it's a genuine gap → **FIX**, or leave it failing (don't register
  things that aren't documented not-supported — that's the discipline that keeps `Fail`
  honest).

**Never register a real bug to make the number go down.** When unsure, leave it failing.

## 3. How to REGISTER a not-supported failure
Edit `cl/skip-registry.lisp` — add one line under the file's `register-skips` block
(create the block if absent). Do **NOT** edit the `perl-tests/*.t` file.

```lisp
(register-skips "<file>.t"
  ("<description-regex>" :<category> "<reason — cite docs/not-supported.md §...>"))
```
- **Matcher**: a regex on the test *description* (preferred — number-shift-robust). For an
  **unnamed** test (e.g. `ok($@ =~ /…/)` / `like($@,qr/…/)` with no name → blank description
  in the faillog) use the **integer test number** instead.
- **Categories**: `:principle9 :error-msg :warning-emit :read-only :utf8 :destroy-gc
  :lvalue :alias :tie`.
- **Keep regexes narrow.** Re-run `./runt <file>` and check for `# REGISTRY-STALE` lines —
  a stale flag means the pattern matched a *passing* test (over-broad); narrow it. (Seen
  twice: tr.t `RT #130198` → split into `eval:`/`warn: cho(p|mp)\(@a`; chop.t
  `chomp @a when` → `chomp @a when.*eq 0 and` to exclude the passing `eq 7` sibling.)
- Verify: `./runt <file>` shows the target tests as `# skip`, `fail` drops, `stale: 0`.

## 4. Crashes / PARTIAL — never auto-skipped
The registry hooks per-assertion (`test-ok`); a crash/abort never reaches it. So:
- Abort/under-count localization is automatic (session 217). The harness emits
  `# PCL-INCOMPLETE last=N planned=M desc=…`; the sweep refines it by exit code into
  `<faillog>/_status.tsv` col 7 (`grep -v '\tOK\t' .faillog/_status.tsv | cut -f1,2,7`;
  the columns are name, status, pass, fail, planned, **drops** (s402, task #343), note):
  - **CRASH** → `CRASH after test N (<desc>) -- crash site ~test N+1 | <SBCL error>`. Open the
    source at test N+1, fix the PCL defect so the rest of the file runs; the registry can then
    reach the later assertions.
  - **PARTIAL** → `INCOMPLETE: ran N of M, last test N (<desc>)`. This is a *clean exit that
    under-counted* — tests were dropped/skipped across the file (not a single abort at N+1).
    Diff the emitted TAP numbers against the source to find where PCL skipped a test/block.
- A **whole-file crash from a not-supported feature** (e.g. `(?{code})`, Tie::Array hang):
  use the file-level `@SKIP` list in `sweep-perl-tests.pl` (coarse), or implement the
  deferred per-statement `handler-case` wrapper (`docs/test-skip-registry.md` §3.1).

## 4a. A failure that never ran: the DROPS bucket (task #343, s402)
A statement the compiler could not lower is replaced by `nil` and the file runs
on, so the assertion it carried appears in **no** count — `perl-tests/bless.t`
has exactly one, and it is a test row, in a file the sweep calls passing. Two
places see it now:
- the transpile says so on stderr the moment it happens —
  `PCL: statement dropped at FILE line N: <source text> -- <reason>` (task #339;
  `PCL_DROP_ANNOUNCE=all` also turns it on for the runtime's module transpiles);
- the sweep records a per-file `drops` count and `tools/sweep-diff.pl` compares
  it against `baselines/parse-error-drop-census-s399.tsv` — **more drops than the
  census fails the run**, fewer is a fix and the census row leaves by EDIT.
  `tools/run-perl-suite.pl` prints the same comparison for perl's own t/.

To see the whole population at once (not just what a run touched):
`tools/drop-census.pl . /tmp/drops.tsv` then diff against the blessed census.

Since s435 (the flip) a dropped statement DIES when it is reached — a
perl-shaped, trappable run-time die at the drop site — so the rows AFTER it
in the same top-level form are lost too.  Those show up in the LOST bucket,
and a lost row is accepted only with that cause, edited into
`baselines/pass-baseline.tsv` row by row.

## 4b. A fix that makes a value REAL exposes rows that were passing on NOTHING (s439, ask 12)
Three fixes in a row (the s435 flip twice, #450 once) moved a passing row to a
FAILING one without touching what the row was about: the row compared TWO
results of the same builtin (`is($out1, $out2)`, `ok($a eq $b)`, two
`eval q{ glob(...) }` call sites) and both sides had been EMPTY, so it passed
while testing nothing.  The baselines catch the move, but only say "moved".
So, when a fix turns an empty/undef answer into a real one, BEFORE the run:
`grep -a` the populations (`perl-tests/`, `$PCL_PERL_SUITE_T`) for rows that
compare two results of the changed builtin; those are the candidates, and
their new failure is HONEST — file the gap it exposes (as #489/#490 were for
glob), edit the row into the baseline with that cause, and say so in the
commit.  A row that was passing on nothing is not a regression; a row that
was passing on something is.

## 5. The migration (remaining work): un-mutate the ~14 inline-skipped files
Files with inline `ok(1,'SKIP…')` / commented-out tests (sort.t, state.t, reset.t, lex.t,
quotemeta.t, each.t, join.t, range.t, splice.t, sub.t, loopctl.t, local.t, time.t,
concat.t, …). Per file:
1. Find the local edits: `diff` our `perl-tests/<file>.t` against a pristine upstream copy
   (Perl's `t/op/<file>.t`), or `git log -p` the file for the `SKIP` edits.
2. **Revert** each inline skip back to the original upstream assertion (file becomes
   byte-identical to upstream → diffable).
3. `./runt <file>` → the reverted tests now fail. Triage each via §2 and **register** the
   genuinely not-supported ones in `cl/skip-registry.lisp`; **fix** any that are real bugs.
4. Verify `fail` count and `stale: 0`; confirm the file's Fully-passing status is unchanged.
Do a few files, then a full sweep + `sweep-diff diff` to confirm no net regression.

## 6. After intended changes: re-bless the baseline
```sh
perl sweep-perl-tests.pl --jobs 8
tools/sweep-diff.pl diff baselines/fail-baseline.tsv .faillog   # review NEW (must be empty) + FIXED
tools/sweep-diff.pl save .faillog baselines/fail-baseline.tsv    # commit the new baseline
```
Re-bless **only from a clean sweep** — a file that flaky-crashes under `-j8` (transient
`SIMPLE-FILE-ERROR`; pack.t/join.t/anonsub.t seen) contributes 0 failures that run. As of
session 217 `sweep-diff diff` no longer mislabels these as "FIXED": it reads
`<faillog>/_status.tsv` and lists a crashed/partial file's absent fails under **"DID NOT RUN
… UNVERIFIED"** instead (see `docs/test-skip-registry.md` §3a). Still re-run a suspicious file
in isolation before re-blessing, since a crashed file's true fail set is simply missing that run.

## 7. Always
- After editing a `.lisp` file, run the paren checker (CLAUDE.md §10) and fix non-zero.
- After a runtime/parser change, run `prove -j8 Pl/t/` (the gate) — it does NOT use the
  registry/log, so it's the independent correctness check.
- Update `docs/sweep-bug-catalog.md`, `docs/session-log.md`, and `MEMORY.md` when counts change.

## 8. Leak hunting in the COMPILER (perl side) — the arena census, first not last

Ruled s407 (`docs/fable-answers-s406.md` §4.8 ask 3): dev-only CPAN modules
for a hunt go into a **scratch `local::lib` under the session scratchpad** —
nothing in the tree, nothing in the perlbrew perl — and that is inside the
standing "dist fetches blanket-OK'd" permission (a system install still asks).

What s406's #128 hunt taught (an hour of negatives, then two minutes to the
line): **weak-ref canaries and "does any package variable grow" can only prove
NEGATIVES**; when every object you can name is freed and RSS still grows
linearly, that IS the signature of a REFERENCE CYCLE, and the tool that names
the line is an **arena census** — count every live SV by type/origin across N
transpiles and diff.  Do it FIRST when growth is linear with no plateau.

    # once per session
    cpanm -L "$SCRATCH/ll" Devel::Gladiator Devel::Cycle
    # driver: transpile the same snippet N times, census before/after
    perl -I"$SCRATCH/ll/lib/perl5" -I. -MDevel::Gladiator=walk_arena,arena_ref_counts -e '
      use Pl::Parser2; my $src = q{...};
      Pl::Parser2->parse_code($src) for 1..50;         # warm up
      my $a = arena_ref_counts();
      Pl::Parser2->parse_code($src) for 1..100;
      my $b = arena_ref_counts();
      for (sort keys %$b) { my $d = ($b->{$_}//0) - ($a->{$_}//0);
                            printf "%6d  %s\n", $d, $_ if $d > 5 }'
    # a type that grows by ~k per transpile is the leak; for CODE refs,
    # walk_arena + Devel::Cycle::find_cycle names the closure and its cycle.

The banned shape (DECIDED s406): `my $w; $w = sub { … $w->(…) … }` — use
`__SUB__`.  Guard: `Pl/t/parser-leak-01.t` (compiler files only; a lib/ shim
runs under SBCL, where a cycle is collected and `__SUB__` in an anon sub is a
no-op stub — do not "fix" one there).
