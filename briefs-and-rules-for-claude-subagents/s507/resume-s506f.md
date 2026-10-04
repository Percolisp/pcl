# s507 — RESUME s506f (perl's command-line switches in `pcl`: #2097 + #1702 + parts of #1709).  USER priority.

You are an EXECUTION agent (Opus 5.5) on the PCL project (Perl -> Common Lisp transpiler).  You RESUME a
checkpointed batch in its EXISTING worktree:

    W=/home/bernt/pcl/.claude/worktrees/agent-a6a0e67984b1d959d      (HEAD 516850b3, 12 commits on 618db500, tree clean)

Your launch cwd may be a different, fresh worktree — IGNORE it; all work happens in `$W` (`env -C "$W" CMD`,
`git -C "$W" …`, absolute paths; never run anything in `/home/bernt/pcl` itself).

Read, in this order, IN FULL:
1. `~/pcl-agent-scratch/s507/SHARED-BOX.md` — the protocol of THIS session (its FIRST ACTION: your model
   id into `$W/scratch/s506f/MODEL.txt`).  It REPLACES `s506/SHARED-BOX.md`.
2. `$W/scratch/s506f/STOP.md` — the predecessor's state: members 1–6 all built and guarded, the design
   decisions (do not re-derive them), the measurements, the OWED list and its order.
3. `~/pcl-agent-scratch/s506/s506f-prompt.md` — the original brief: the ruled design D1–D5, the
   acceptance bars, and the FINAL REPORT format (still in force; where it says `s506/…` for SHARED-BOX,
   BENCH-WANTED or MAIN-READY read `s507/…`).
4. `~/pcl-agent-scratch/s473/COMMON.md`.

## What is different from STOP.md's OWED list
- Main is `2e71d4aa` (618db500 + s504c + one docs commit; numbers in SHARED-BOX).  `git -C "$W" rebase main`
  FIRST.  In `pl2cl` KEEP BOTH sides: your `text => $text` and s504c's `module_unit` (parse_source's
  `%known` needs both); verify with `Pl/t/file-private-cells-01.t` (s504c's guard, 19 rows) AND your
  `Pl/t/pcl-switches-01.t` right after the rebase.  Then: does a `--module` / `--extension` / eval-mode
  unit still ignore a `#!` line (your D2 rule) now that `module_unit` exists?  One probe each.
- Generation string: per SHARED-BOX (the lowest of v2-3880 / v2-3980 / v2-4080 above the main you
  finally rebase on; `rt-edit.pl`'s gen pattern must follow).  Artifacts regenerated after the renumber.
- ORDER OF THE BARS (so a rebase across s502e, if it merges first, costs little): after the rebase and
  artifacts → (a) the FULL sweep that confirms the WIP baseline commit `516850b3` (18728 → 18731 was
  measured before; on this main re-derive row by row, never bless) → (b) companion `--all --quick
  --jobs 4` (BEFORE = Fable's run of main, `s507/companion-full.log`, a FULL run: compare file by file
  only the files `--quick` runs; movers re-run serially and spliced with cause) → (c) gate-set-scan →
  (d) `prove tools/t/install-pcl.t` (+ `install-container.t` if podman/docker answers) → (e) the
  compile-time bench (BENCH-WANTED, load < 2) → (f) the one-liner battery re-run on the rebased tree →
  THEN check `s507/MAIN-READY` / main's sha; rebase if it moved → (g) corpus-diff + emission-ab →
  (h) full gate → (i) the sweep again ONLY if you rebased across s502e → (j) everyday BEFORE → AFTER →
  (k) records, task DONE sections → (l) `--record` from the clean committed tree → MERGE-READY.
- s502e (the other batch) changes `cl/pcl-runtime.lisp` (handle stash for a blessed filehandle, 4-arg
  `select`, `sysread`), `Pl/Parser.pm` (list-valued `use constant`), `lib/Scalar/Util.pm`,
  `Pl/t/transpile-test-10.t`, baselines.  If it merges before you: KEEP BOTH everywhere.  Nothing in it
  touches `pcl`, `pl2cl`, `tools/`, `perl-tests/t/test.pl`.
- Docs: keep yours to what must be TRUE at merge (the switch table, `--help`, not-supported, STATUS —
  all already written; re-check them against the final behaviour).  A separate docs agent rewrites the
  `pcl` command documentation AFTER your merge — your STOP.md section "FOR THE DOCS PASS" is its input:
  keep it current.  Do NOT touch `README.md`.

## Things the reviewing model will check (do them before it asks)
- Every row of `Pl/t/pcl-switches-01.t` and `tools/t/harness-switches.t` rows 5–11 FAIL on a base
  extraction of main `2e71d4aa` (inverse verification on the NEW base, list the rows that pass there
  and why they are still worth having).
- `prove --timer Pl/t/pcl-switches-01.t`: its wall time vs the slowest gate file (split into `-02` if
  it becomes the slowest).
- `-c` now RUNS BEGIN/CHECK/`use` and prints on STDERR: grep the tree (`tools/`, `Pl/t/`, `docs/`,
  `tools/lib/PCLCheck.pm`, the installer and its tests) for every caller of `pcl -c` / `--check`
  that relied on the old stream or the old no-run behaviour.
- `-v` = version: grep the tree, docs and tests for `-v` used as `--verbose`.
- The switch-error exit status (25 where perl's is errno-derived): probe three different errors under
  perl and say what perl's statuses are; if they are stable values, mirror them.
- The five behaviour changes "a user of today's pcl would notice" are FLAGGED in the final report in
  one list (they go to the USER verbatim), with the `-w` measurement and decision.

Final report: the format in the original brief's last section, SHORT.  `MERGE-READY: <sha>` as the
first line of `$W/scratch/s506f/STOP.md`, or `NOT MERGE-READY` + the exact reason.

## FABLE'S REVIEW PROBES — already run on your checkpoint `516850b3` (perl → base `2e71d4aa` → tree)
Driver `~/pcl-agent-scratch/s507/review/sw.pl IMPL TAG [REGEX]` — 119 cases, each run in a fresh
directory with its own fixture files and stdin; outputs `review/out/sw.perl` / `sw.base` / `sw.tree1`
(stdout, stderr, exit status and the files an in-place edit leaves).  Result on your checkpoint:
**105 of 119 identical to perl** (the base: 18).  Re-run it on your FINAL tree
(`perl ~/pcl-agent-scratch/s507/review/sw.pl "$W/pcl" final`) and compare case by case with
`out/sw.perl`; the differences must be exactly the ACCEPTED ones below.

YOURS — fix, each with a guard row that fails on the base:
- **F1 `local $^W` is lost** (case `w-assign`; `review/tmp/x/d2.pl`): `$^W = 1; { local $^W = 0; print $^W }
  print $^W` — perl `0` then `1`; your tree `1` then `1`.  `local $^W = 0;` is one of the most common
  lines on CPAN (the pre-`no warnings` way to silence a block), and now that `-w` / `#!perl -w` really
  set `$^W`, a module's `warn … if $^W` inside such a block fires where perl is quiet.  Find how
  `local` binds the OTHER boxed specials (`local $/`, `local $|`, `local $,`) and why `$^W` — which
  you turned from a raw `0` into a box — is not on that path (rule 11: the sibling, not a new arm).
  Probe also `local $^W;` (no value: undef → prints empty… probe it), a nested sub called inside the
  block seeing 0, the value restored after `die` inside `eval { local $^W = 0; die }`, and
  `local ($^W) = 0`-style list form if perl accepts it.  Then RE-COUNT your `-w` measurement: the 27
  perl-tests files and the companion's 121 — a row that now moves because `local $^W = 0` works is
  yours to attribute.
- **F2 an uncaught die under `-e` / `-n` / `-p` / a stdin program is not tidy — task #2492** (cases
  `die-status`, `die-line`, `c-err`, `syntax-err`): `pcl -e 'die "bye\n"'` prints
  `While evaluating the form starting at line 24, column 0` / `of #P"/tmp/pcl_….lisp":bye`, where
  perl — and `pcl file.pl` through the script cache — print `bye`.  PRE-EXISTING (#2492 has the
  suspected cause and its cheap discriminating measurement; #1595 has the mechanism: SBCL's
  load-as-source prints the context before the hook runs, a fasl load prints none).  One-liners are
  where every user of your switches meets it, so: TAKE the #2492 measurement.  If routing the
  temp-program paths (`-e`, `-E`, `-M … script`, `--no-cache`, stdin) through the SAME loader the
  script-cache path uses — its loader, not its cache; #1862's content-keyed cache stays out — is a
  small change in `pcl` (no new mechanism, the first-run cost measured and inside the 50 % compile
  budget), ship it with rows comparing stderr BYTE FOR BYTE with perl for `die "bye\n"`, `die "x"`
  (` at -e line 1.`) and a failing `use`.  If it is not small: append what you measured to #2492 and
  leave it.  Either way say which in the report.
- **F3 (text only, optional)**: `pcl -M` with nothing after it says `Module name required with -M
  option.`; perl says `Missing argument to -M` (status 25 both).  `-M ''` is where perl says yours.
- **F4 check**: pl2cl's STDIN path applies `program_text` when `!defined $eval_pkg` — also under
  `--module` / `--extension` given on a stdin run?  One probe each (`./pl2cl --module < M.pm` with a
  `#!perl -l` first line must not chomp anything); guard it if it was open.

ACCEPTED differences (ruled or pre-existing — list them as such in your report, do not fix):
`Mstrict` (PCL does not enforce `strict vars`: principle 9); `shebang-T`, `T` (taint: D4); `d` (D4);
`nofile` (text); `p-DATA` (DATA readable at BEGIN time — pre-existing); `files-eof` (`close ARGV`
does not reset `$.` — PRE-EXISTING runtime bug, FILED **#2684**); `C0` (@ARGV arrives decoded —
PRE-EXISTING, FILED **#2685**; say in your not-supported entry that `-C0` cannot take the decoding
back); `die-line` (`, <> line 2.` — your #2660) once F2's wrapper is gone.
Also filed from these probes, pre-existing, NOT yours: **#2686** (the cache-building first run of a
script loses the STDOUT of a `require "./file.pl"`'s top level) and **#2687** (`require "./M.pm"`
literal dies "Can't locate"); a `require`d file and an eval string DO ignore their own `#!` line, as
you designed (probed: `review/tmp/x/r2.pl`, `ev.pl`).
