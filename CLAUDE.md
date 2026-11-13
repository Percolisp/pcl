# CLAUDE.md

Guidance for Claude Code in this repository.  **This file has a SIZE LIMIT
(40,000 characters; guard `Pl/t/claude-md-size-01.t`), so each rule is stated
SHORTLY here.**  It is split into exactly TWO companion files, not more:

- **`docs/claude-md/REFERENCE.md` -- the FULL TEXT of the working sections,
  under THE SAME HEADINGS as this file** (Design Principles, Quick Reference,
  WHAT TO RUN WHEN, Pipeline and generated artifacts, Standing rules from
  the review rounds, Semantic Deep-Dives, Dependencies): worked examples,
  reasons, every command with its commentary.  The two files must AGREE; if
  they differ, do the stricter and report it.  **When a rule here is too
  terse to apply, or a brief sends you to a section of `CLAUDE.md` for
  detail, read the section of the same name there.**  A subagent whose task
  touches the compiler, the runtime, the baselines or a test runner reads
  the matching REFERENCE section before it starts.
- `docs/claude-md/HISTORY.md` -- the numbers of every earlier merge, the
  s321-s439 review record, V1-era notes.  No task needs it.

Everything else a task needs is where it always was: `docs/DECIDED.md`, the
docs in the Semantic Deep-Dives index, the task store, your brief.

## Project Overview

**PCL** (Perl to Common Lisp) is a transpiler: it parses Perl with PPI, builds
an AST and generates Common Lisp that runs on SBCL.  The goal is to run real
CPAN modules with Perl's exact semantics.

**This is a Perl project.** Use Perl (not Python/Ruby/etc.) for all scripting,
one-liners and helper scripts — searching logs, processing text, anything a
Perl one-liner via Bash can do.  Don't use subagents for what Perl can do
directly.

## Before You Triage, Probe, or Decide — the lookup order (MANDATORY)

Most questions in this project are already settled; the expensive failure
mode is re-deriving (or contradicting) a settled answer. Before probing a
failure or designing a fix, in this order:

1. **`grep docs/DECIDED.md`** for your keywords — the one-grep index of
   settled questions, each line pointing at the authoritative doc.
2. **`grep docs/not-supported.md`** — deliberate non-support is blessed
   there; a failing test may already be explained.
3. **`docs/test-debugging-runbook.md`** — the FIX-vs-REGISTER decision tree
   for perl-tests/suite failures.
4. **"Semantic Deep-Dives" below** — if a doc covers the area you are about to
   touch, read it first.
5. Only then probe (minimal scratch file, perl vs `./runpcl`).

And symmetrically, when work SETTLES a question: add a one-line pointer to
`docs/DECIDED.md` in the same commit; put load-bearing semantic decisions
in `docs/ir-spec.md` (a code comment may point at the spec, but must never
be the only copy); record failed attempts and what killed them in the task,
so the task says what NOT to retry.  A **"suspect X" task carries the cheap
discriminating measurement** that would confirm or kill X — or names it and
says why it was not taken.

## Design Principles

Numbered; code comments and briefs cite them by number ("rule 12", "9a").
Full text with worked examples: `docs/claude-md/REFERENCE.md` "Design
Principles".

1. **CPAN Compatibility**: match Perl semantics exactly; the goal is to run
   real CPAN modules.

2. **Readable generated code — but speed wins** (priority decision
   2026-07-02): keep the CL recognizable to Perl programmers where it is free
   (`p-sub`, `p-print`, `$varname`); when readability and performance of the
   generated code conflict, performance wins.

3. **Compile-time visibility**: sub definitions and variable declarations are
   wrapped in `eval-when` so BEGIN blocks can call/see what was defined before
   them in source order.

4. **No easy write-offs**: every incompatibility blocks CPAN code.  Fix it, or
   discuss the trade-off with the user BEFORE marking it a limitation.

5. **Never simplify tests**: when a test fails, fix the code, not the test
   (`perl-tests/` = the authoritative Perl suite, `Pl/t/` = PCL's regression
   tests).  Never weaken a `Pl/t/` row, comment a test out, or write
   `ok(1, 'SKIP: ...')`.  A feature genuinely out of scope is discussed with
   the user first and then marked in the **declarative skip-registry**
   (`cl/skip-registry.lisp`, keyed on the test description, citing a
   `docs/not-supported.md` reason; `docs/test-skip-registry.md`) — never by
   editing a `perl-tests/*.t` file.  Crashing/aborting tests are NEVER
   auto-skipped: they stay CRASH/PARTIAL as fix targets.  **On the CPAN
   board** a failing row's cause is a leading marker with a key (`#N`,
   `NS:<id>`, `PERL-SKIP`, `PERL-FAILS`, `UNREDUCED #N`, `HARNESS #N`;
   `docs/failure-cause-classes.md` "The grammar"); `Pl/t/verdict-register-01.t`
   checks every key, and a task is closed only together with the rows keyed on
   it (**CLOSE-WITH-ROWS**).

6. **Add a regression test for every bug fix**, in an existing test file.
   **The constraint is a file's RUN TIME, not its row count**: `prove -j8`
   parallelises across files, so the gate's wall time ≈ the slowest file
   (`test_transpile` = a perl oracle + an SBCL transpile+run; `run_cl` spawns
   SBCL).  Measure with `prove --timer Pl/t/<file>`; if a file is heading past
   the current slowest, start the next `transpile-test-NN.t` (`-10` exists; the next
   new one is **`-11`**).  Leave `transpile-test-01.t` and `-07.t` alone.  **A TIMED
   row's bound is RELATIVE to perl's own time measured in the same run
   (`8 * perl + 0.5 s`), never absolute seconds** — CI is ~3x slower than
   this box.  The timed row bounds gross regressions; a lever's inverse guard
   is a mechanism row that fails on the base.

7. **Document complex semantics in `docs/`**: tricky Perl-vs-CL semantics get
   a `docs/topic-name.md` (problem, solution, edge cases) and ONE LINE in
   "Semantic Deep-Dives" below (the full description goes in the section of
   the same name in `docs/claude-md/REFERENCE.md`).

8. **wantarray/context**: the three-valued implementation is done
   (`docs/wantarray-impl-plan.md`).  A known sweep regression (the VOID_CTX
   sub-body wrap in `_process_expression_statement` is too broad) is fixed
   before any further wantarray work.

9. **Assume valid Perl input**: PCL is a transpiler for functioning Perl, not
   a validator.  Tests that verify REJECTION of invalid Perl are out of scope
   (commented out, not implemented).

   **9a. Fix at the right LAYER — module behaviour never goes in the parser
   or runtime.**  Three layers:
   - **`lib/<Module>.pm` shim** (or a facts overlay `lib/PCL/Facts/…`) — one
     module's behaviour: its subs, prototypes, exports, constants.
   - **`Pl/*.pm` parser/codegen** — generic language mechanisms, keyed on the
     **mechanism, never a name** ("a `(&@)` prototype makes the trailing block
     a block-form"); the shim supplies the data, the parser consumes it.
   - **`cl/pcl-runtime.lisp`** — genuine Perl *core* semantics.

   **Decision rule:** could a user write this in plain Perl?  Then it belongs
   in a `.pm` shim, and the parser/runtime job is only to make the generic
   mechanism work.  **Worked example:** `first { … } @list` (List::Util)
   parse-errored.  WRONG: add `first => {has_block_arg=>1}` to
   `Pl/Environment.pm` (module data in the core).  RIGHT: declare
   `sub first (&@)` in `lib/List/Util.pm` (the data), and stop
   `_extract_module_prototypes` from SKIPPING that module, so the generic
   block-form parser reads the prototype (the mechanism).
   **Smell test (hard stop):** a diff that adds a literal
   CPAN module name *or a non-core function name* under `Pl/` or `cl/` is at
   the wrong layer (core builtins are language, the only exception).
   Architecture: `docs/shipped-modules.md`.

10. **Lisp parenthesis discipline**: after EVERY Write/Edit of a `.lisp` file
    run `sbcl --script tools/check-parens.lisp FILE.lisp` ("balanced: ..." +
    exit 0) and fix any failure before reporting done.  It uses SBCL's own
    reader; do NOT use a textual paren scanner (`|$"|` false-positives).
    No Lisp function body over ~80 lines — extract `case` arms into named
    helpers first.  **Indentation encodes depth**: exactly 2 spaces per paren
    level; never a closing `)` on a line indented deeper than the line that
    opened its form.  For a paren problem, split on `(defun` at column 0
    (`perl .claude/hooks/split-lisp.pl FILE.lisp` → `/tmp/defun-NAME.lisp`)
    and check each chunk; never count parens across the file.

11. **Reuse, don't duplicate — find the existing mechanism first.**  Perl
    features come in families; a fix that copies a special-case branch for one
    member misses the other parse paths and drifts.  Grep for the data table,
    helper or marker that drives the working sibling; normalise your input
    into the existing path (often ONE pre-pass) instead of branching beside
    it; count the parse paths the input can arrive by and push the fix to the
    one place they all pass.  **Smell test (hard stop):** the same logic in
    two places, or a copied branch with one token changed.

12. **A missing case DIES — never falls through to a default.**  A runtime
    `cond`/`case`/dispatch over a closed set of legal values ends in an
    explicit error naming the unhandled value, never `(t 0)`/`(t nil)`.
    Silent-wrong is the worst failure mode here.  **Boundary:** DIE only when
    the missing case should have produced/written a VALUE the program then
    consumes; an EFFECT-ONLY missing case (a jump, a tie, an attribute)
    ANNOUNCES itself on stderr and continues, with a `docs/not-supported.md`
    entry.  The sin is the silence, not the fall-through.

13. **Every PPI bug is LOGGED in the same commit as its fix or workaround**:
    a section in `docs/ppi-upstream-bugs.md` (minimal repro, what perl does,
    actual vs expected token dump, PPI version, the workaround and what it
    unblocks) and a row in `docs/ppi-bug-report.t` (every row FAILS on stock
    PPI; bump `tests => N`).  A bug NOT yet fixed on the branch may get a
    canary row in `Pl/t/misc-fixes-02.t` asserting the broken behaviour: it
    fails when PPI is fixed, the signal to drop the workaround.  Scope: only what PPI itself tokenizes or
    structures wrongly.  **PPI is VENDORED (`vendor/PPI`, the `pcl` branch),
    so the fix goes ON THAT BRANCH** in PPI's two-commit PR shape (failing
    rows first as TODO in `t/ppi_bugs_pcl.t`, then the fix; PPI's own suite
    green), the registry row in `Pl/PPIBugs.pm` gets `fixed_in => 'pcl'`, and
    PCL's repair pass for it is DELETED.  A repair in `Pl/` is written only
    when the fix cannot go on the branch yet.  A deletion is probed beyond the
    corpus (the `print` argument slot, after `->`, statement start).  Read
    `docs/ppi-fix-at-source-s527.md` before touching any `_repair_*` /
    `_fix_ppi_*` / `_merge_*` pass or `vendor/PPI`.

## Quick Reference

Every command with its full commentary (the cached core, `PCL_OPT`, the
installer, the container test, XS, the companion suite, the runners):
`docs/claude-md/REFERENCE.md` "Quick Reference".

```bash
prove -j8 Pl/t/                    # THE GATE (always -j8); must end `Result: PASS`
tools/prove-core [prove args]      # the same on a FRESH temp core (belt and braces)
prove -v Pl/t/codegen-01.t         # one file
echo 'my $x = 1 + 2;' | ./pl2cl    # quick transpile
./runpcl x.pl                      # transpile + run (output may hold NULs: grep -a)

perl tools/sweep-perl-tests.pl --jobs 8          # full sweep, ALONE on the box; RUNS ITS OWN GATE
perl tools/sweep-perl-tests.pl --jobs 4          # full sweep on a SHARED box (every agent, always)
perl tools/sweep-perl-tests.pl --jobs 1 perl-tests/foo.t   # one file (informational)
tools/sweep-diff.pl diff baselines/fail-baseline.tsv .faillog   # rebuild a verdict
tools/corpus-diff.pl               # emission A/B over the corpus; READ its SILENT-DROP line
tools/emission-ab.pl --ref <base> --list lib/**/*.pm
tools/everyday-smoke.pl            # the steering number: `EVERYDAY: N of M identical to perl`
tools/run-perl-suite.pl --all --quick --jobs 4   # perl's own t/ (the companion), default form
tools/run-perl-suite.pl --jobs 1 FILE            # one companion file
tools/run-dist-t.pl --rows DIST t/FILE           # one CPAN-board file
tools/ir-host-leak.pl ; tools/ir-inventory.pl    # the IR stays in its three vocabularies
tools/ir-conform --jobs 2                        # after any cl/ or emission change: 0 fail / 0 stale
tools/gate-set-scan.pl ; tools/drop-census.pl    # population-wide instruments
tools/rebuild-pack                 # regenerates cl/pcl-pack.lisp ONLY (mro + warnings: "Generated artifacts" below)
tools/tag-license FILE             # every new code file carries the license tag
tools/install-pcl --prefix ~/.local   # installer; test: prove tools/t/install-pcl.t
prove tools/t/install-container.t  # installer in a container (minutes; before a tag)
tools/vendor-ppi <clone> pcl       # refresh vendor/PPI from the pcl branch
PCL_OPT=none ./pl2cl < prog.pl     # all named optimizations off (Pl/Passes.pm registry)
PCL_SHOW_SBCL=1 <any runner>       # print the exact SBCL command it spawns
```

- **THE RUNTIME IS KEPT COMPILED AND CACHED**: every runner that spawns SBCL
  goes through `tools/lib/PCLSbcl.pm` and starts from a saved core under
  `~/.pcl-cache/core/`, named by a content hash (it cannot be stale; a
  worktree gets its own).  `PCL_NO_CORE=1` = source mode; `pcl --clear-cache`
  removes the cores; extensions are compiled once into `~/.pcl-cache/ext/`
  (`PCL_NO_FASL_CACHE=1` off).  **Stale module cache**: `rm -rf ~/.pcl-cache/*`.
- **A runner chooses WHAT to load, never HOW SBCL starts.**  Paths outside
  the checkout are derived (`tools/lib/PCLPaths.pm`), never written down
  (guard `Pl/t/no-hardcoded-paths-01.t`).  Every script finds its tree through
  `PCLPaths::root`.
- **Companion suite**: `--all --jobs 8` can OOM — use `--jobs 4`; an agent's
  one-file run is `--jobs 1`.  A file
  that needs more than the 90 s default is registered in
  `baselines/perl-suite-timeouts.tsv`, never left to TIMEOUT.
- **`.tsv` files under `.faillog/` and `baselines/` contain NUL bytes**: use
  `grep -a` or perl, or grep prints nothing.
- **License**: same as Perl; never tag files from the Perl distribution or
  CPAN (`perl-tests/`, `cpan-tests/`).  `Pl/t/license-tag-01.t` checks.
- **XS** (`tools/pcl-xs-install`, `tools/pcl-conform`): the pclxs bridge is a
  separate repo (`~/pclxs`).  `Pl/t/xs-01/02/03.t` are PARKED (`skip_all`,
  USER s485) until pclxs works again; `PCL_XS_TESTS=1` runs them.

### WHAT TO RUN WHEN

The cadence is keyed on WHAT CHANGED, because every measurement is blind to a
known set of change kinds.  Rows are ADDITIVE.  **Always:**
- **the gate** — a lone change: `tools/prove-core`.  **In a BATCH (s477
  ruling): per MEMBER the TARGETED gate** (`prove Pl/t/<guard file> <the
  Pl/t files that name the area touched>`), **and the FULL gate ONCE at the
  END of the batch**, before MERGE-READY;
- if anything under `Pl/` changed: `tools/corpus-diff.pl` (READ its
  SILENT-DROP line) + `tools/emission-ab.pl --ref <base> --list lib/**/*.pm`;
- after any `cl/` or emission change: `tools/ir-conform --jobs 2`;
- the targeted files the change names.

**Who runs what:** a BATCH (an agent) runs everything in the table except
what is marked ROUND; ROUND = the reviewing session, once per round on merged
main (the CPAN board's tree half, the companion `--all --quick`, the full
companion).  On a shared box sweeps are `--jobs 4`, companion runs `--jobs 1`.  Full text, run
times and reasons: `docs/claude-md/REFERENCE.md` "WHAT TO RUN WHEN".

| what changed | full perl-tests sweep | companion suite | also |
|---|---|---|---|
| `Pl/**`, corpus-diff IDENTICAL, lib byte-identical, NOT name resolution | **NO — it cannot move** | no | — |
| `Pl/**`, corpus-diff shows diffs | YES, after every diff is explained per file + probed vs perl | ROUND; the batch runs ONE-FILE acceptance of the files its members name | generation bump + ALL THREE artifacts regenerated ("Generated artifacts" below) + `tools/ir-host-leak.pl` |
| `Pl/**` name resolution / scoping / rename / capture / promotion | **YES — the sweep IS the gate** | ROUND | gate-SET scan (`tools/gate-set-scan.pl`: base vs tree) over BOTH populations — `perl-tests/*.t` and perl's own `t/` — when a checker / refusal / decline WIDENS |
| `cl/**` runtime | YES (invisible to corpus-diff) | ROUND; per batch only a bounded leg on files the brief NAMES (a coercion / stringification change: the brief names `op/` files) | rule-12 read of the touched dispatch; `tools/ir-inventory.pl` if an export moved; `tools/ir-host-leak.pl` if a macro expansion changed |
| `lib/**` shim | YES | ROUND; one-file runs of the dist files a member names | — |
| harness: `perl-tests/t/test.pl`, `cl/pcl-test.lisp`, `cl/skip-registry.lisp` | YES | **`--all --quick` PER BATCH** | baselines edited ROW BY ROW |
| runners: `tools/sweep-perl-tests.pl`, `tools/run-perl-suite.pl`, `tools/lib/PCLSbcl.pm`, `tools/pclperl-for-tests`, `Pl/t/PCLCore.pm` | the runner that changed, once; verdicts compared file by file | same | `PCL_SHOW_SBCL=1` before/after diff |
| `docs/**`, `tools/t/**`, memory | nothing beyond the gate | no | — |
| any BATCH that changes `Pl/**`, `cl/**` or `lib/**` (once per batch) | — | — | `tools/everyday-smoke.pl`: quote `EVERYDAY: N of M` BEFORE → AFTER |

- **Baselines are edited ROW BY ROW with the cause, never re-blessed from a
  run** (`baselines/fail-baseline.tsv`, the drop census
  `parse-error-drop-census-s399.tsv`, `cpan-board14-fails.tsv`,
  `perl-suite-fails.tsv`, `everyday-baseline.tsv`).  A NEW row is explained or
  fixed; a FIXED row leaves by edit citing the batch.  The one exception:
  `pass-baseline.tsv` (the sweep's passing rows) is normally edited by hand
  too, and may be re-blessed with `sweep-diff.pl save-status` ONLY from a
  gate-green run after a per-file audit (runbook §6; it stamps `# taken-at:`).
- **Read the sweep's `TOTAL passing` line, not only the diff**: the diff
  buckets are NEW / FIXED / UNSTABLE / **LOST** (baseline passing rows the run
  did not produce) / **DROPS** (statements replaced by `nil`).  A change that
  makes a file abort earlier reports `0 new` while coverage evaporates.  A
  TIMEOUT contributes no rows; the sweep retries it once at 3x.
- **Compare a gate count against a measurement of the SAME tree.**
- **ROUND-LEVEL cadence (USER 2026-10-17, kept s523)**: the two slow
  instruments — the CPAN board tree half and the companion `--all --quick` —
  run ONCE PER ROUND by the reviewing session on merged main, after every 3–4
  merges or at session end if a merge happened since the last round; a mover
  is attributed by `git bisect` with one-file runs.  The full companion
  (`PCL_SESSION=sNNN tools/run-perl-suite.pl --all --jobs 4 --bless-stamps`)
  runs at least once per round on a quiet box.  A standing duty with no skip.
- **THE NUMBER THE PROJECT STEERS BY** is the `EVERYDAY:` line — ordinary
  programs byte-identical to perl in STDOUT + exit status (goal > 90 %).  It
  is a steering number, never a compatibility percentage
  (`docs/everyday-battery.md`).  The baseline's cause column picks the next
  correctness batch.  `--record` (whole corpus, clean tree) appends
  `baselines/everyday-history.tsv` at each merge.
- **"CI green" means BOTH workflows**: the newest `CI` run AND the newest
  `install-matrix` run.  The install matrix runs on a push only for its own
  paths, so `prove tools/t/install-container.t` is run locally before a tag
  and after any change to `tools/install-pcl` or `tools/install-matrix/*`,
  and a merge that touches the SBCL floor adds a comment line to
  `.github/workflows/install-matrix.yml` so the push re-runs it.  Not a
  per-batch duty otherwise.

### Pipeline and generated artifacts

Full text: `docs/claude-md/REFERENCE.md` "Pipeline and generated artifacts".

- **ONE pipeline, ONE expression compiler**: `Pl/Parser2.pm` +
  `Pl/VarAnnotator.pm` + `Pl/CLForm.pm`; expressions through `Pl/PExpr.pm` →
  `Pl/ExprToCL.pm` once; statements classified by `Pl/Stmt.pm`; scopes by
  `Pl/Binder.pm`.  There is NO fallback: anything not lowered is a hard,
  perl-shaped error (`PCL: unsupported …`) with a `docs/not-supported.md`
  entry.  `Pl/Parser.pm` (v1) is still loaded for its expression seam.
  Optimizations are NAMED entries in `Pl/Passes.pm`, switched by `PCL_OPT`
  (a typo dies naming the list); `PCL_OPT=none` must RUN identically.
- **Bump `*pcl-cache-generation*`** (`cl/pcl-runtime.lisp`) on every
  emission-changing commit — module caches are keyed by it.  Parallel batches
  get reserved generation numbers; keep yours across a rebase.
- **Generated artifacts checked into the tree** — `cl/pcl-pack.lisp` (from
  `cl/pack-impl.pl`), `cl/pcl-mro.lisp` (`lib/mro.pm`), `cl/pcl-warnings.lisp`
  (`lib/warnings.pm`).  After an emission-changing commit regenerate ALL
  THREE, on the final rebased tree: `tools/rebuild-pack` (pack ONLY);
  `./pl2cl --extension lib/mro.pm > cl/pcl-mro.lisp && tools/tag-license
  cl/pcl-mro.lisp`; the same for `lib/warnings.pm` → `cl/pcl-warnings.lisp`.
  `Pl/t/artifact-staleness-01.t` compares each line-1 `gen=` stamp against
  the generation and fails the gate otherwise.  The fourth,
  `cl/pcl-uniprops.lisp`, is DATA (`tools/rebuild-uniprops`, guard
  `Pl/t/uniprops-01.t`) and has no `gen=`.
- **Runtime Symbols Must Be Exported** from the `pcl` package
  (`cl/pcl-runtime.lisp` `:export`): generated code runs in user packages that
  `(:use :pcl)`; an unexported special (`*wantarray*`) silently becomes a
  different variable there.
- **`docs/ir-spec.md` is the NORMATIVE semantics manual** — update it on any
  semantic emission/runtime change.
- **Adding things**: an operator → `Pl/PExpr/Config.pm` `%precedences` (+
  `Pl/ExprToCL.pm`); a builtin → `known_no_of_params` there (2 = exactly 2,
  `[1,2]`, -1 list, -2 defaults to `$_`, -3 to `@_`, -12 one param then list);
  a compound statement → an arm in `Pl/Parser2.pm` `_lower_compound` + an
  exported runtime macro.  A new context-sensitive builtin goes in
  `%WANTARRAY_SENSITIVE`.
- PPI's `find` returns `0` (not undef) when nothing is found: `|| []`, never
  `// []`.

## Standing rules from the review rounds

The operational residue of the s321–s540 rulings.  Each rule with its origin
and what it guards against: `docs/claude-md/REFERENCE.md` "Standing rules
from the review rounds".

- **Errors fail in the SAME PLACES as perl and are TIDY** (one line, never a
  Lisp backtrace); the exact text is not a goal.  **`eval STRING` must work**
  — never gate it as unsupported.
- **A statement the compiler cannot lower is a perl-shaped, trappable
  run-time die at the drop site** (one shape, every mode), announced as
  `PCL: statement dropped at F line N` and counted in the drop census
  (`baselines/parse-error-drop-census-s399.tsv`: more drops than the census
  fails the sweep).
- **Review probes run perl → BASE → tree.**  A probe the base passes has not
  reached the bug.  A claimed bar must be on disk (a log ending in the
  `heavy.sh` END line).
- **Expectation-rewrite rule** (editing what a `Pl/t` row expects): the new
  text is perl-probed; the diff is exactly the divergence; the edit
  STRENGTHENS the row; a guard row lands in the same commit.
- **A commit that removes a refusal greps `Pl/t` for the refusal's message
  text** — a stale twin guard otherwise fails later.
- **A detector, its rewriter and its promoter share ONE resolver**: fixing
  only the detector turns a refusal into a SILENT WRONG.  A sigil- or
  scope-blind capture refusal is a bug — delete or rename, never narrow.
- **A fix that makes values real exposes rows that were passing on nothing**:
  read each newly failing row for its CAUSE and edit the baseline by hand.
  A COUNT is not a finding.
- **"Pre-existing" is WHEN, not WHY**: a companion mover is spliced into a
  baseline only with its cause.  A `--quick` NOT-RUN row is never a mover.
- **Bucket counts need the row TOTAL in both directions**; a row number is
  meaningful only within its own run.
- **A capture-dependent emission keys the eval cache.**  A diagnostic that can
  fire during a RUN answers "and on a warm cache?".
- **A census increase is legal when it converts a worse failure (a crash)
  into a counted drop.**
- **No pipe after `git rebase` / `git merge` in a command chain** (it hides
  the exit status); verify a fast-forward by its own exit status.
- **Never `git stash` for a compare** — use a worktree at the base sha.

## Test Status -- CURRENT NUMBERS

**s540 end, 2026-11-13: main = MAIN-READY-92 `212d4782` + records (code
`24988e35`, generation v2-14400).**  Gate **371 files / 12,131 rows
`Result: PASS`**; full sweep **TOTAL passing 18,803**, GATE clean; corpus-diff
population 111 files; ir-conform 328 / 0; ir-host-leak 31; **EVERYDAY 115 of
122 (94.3 %)**; CPAN board (round of s539, on MAIN-READY-85): 641 PASS / 161
PARTIAL / 27 FAIL / 67 SKIP of 896 files, 109,797 ok / 1,740 not ok; PPI
`pcl` = `c2f21706`; CI green.  Known default-build limits flagged to the
USER: `caller` file/line and `croak` location only under
`PCL_OPT=line-track`; `DESTROY` runs at program exit only.

**REPLACED IN PLACE at every merge (one paragraph); the previous paragraph
moves to the top of `docs/claude-md/HISTORY.md` part 1.**  This is the only
place the current numbers are written down inside the repo rules.  The user-facing
copy is `docs/STATUS.md` — update both together.

## Key Files to Read

When resuming work (items keep their old numbers: briefs cite "Key Files 0a"):
- **0.** `docs/DECIDED.md` — the one-grep index of settled questions.
- **0a.** `dot-claude-in-home-dir/README.md` — **the TASK STORE and Claude's MEMORY
  NOTES live IN THE REPO**: `~/.claude/tasks/pcl` and the memory directory are
  SYMLINKS into `dot-claude-in-home-dir/{tasks/pcl,memory}`; every `#NNNN` in
  the docs is a file there.  Both are PUBLISHED: technical content and work
  decisions only, nothing personal.  In an agent WORKTREE that directory is a
  stale checkout — tasks are always read and written through
  `~/.claude/tasks/pcl` (write with `JSON::PP->new->utf8` to a `:raw` handle;
  an agent files new tasks in the ID RANGE its brief gives) and committed on
  main by the reviewing session.
- **0b.** `briefs-and-rules-for-claude-subagents/README.md` — the agent RULEBOOK
  (`s473/COMMON.md`), each session's protocol, briefs and running notes
  (`sNNN/SHARED-BOX.md`, `<label>-prompt.md`, `PAUSE-sNNN.md`,
  `resume-after-stop.md`).  WRITE THE REPO PATH: the old
  `~/pcl-agent-scratch/...` paths are file-level symlinks into it.
- **1.** `docs/session-log.md` — session history, newest first.
- `docs/STATUS.md` — the user-facing compatibility page.
- The live queue: `docs/plan-post-s433.md`; the compatibility plan (USER
  s538) in memory `project_compat_plan_s538`; the parser program in
  `docs/parser-review-s525.md` §5 (#3694).
- **2.** **A review session writes its rulings into `docs/DECIDED.md` + the session
  log + the live plan doc; do not start a review-doc family.**  The compressed
  record of the s321–s439 review rounds is `docs/claude-md/HISTORY.md` part 2;
  the removed per-session review files are readable from history
  (`git show 959bf43:docs/fable-answers-s439.md`).

### Semantic Deep-Dives (read before touching these areas)

One line each: READ THE DOC BEFORE TOUCHING THE AREA.  The full description
of every entry (what it found, what it ruled) is in
`docs/claude-md/REFERENCE.md` "Semantic Deep-Dives".  All paths are under `docs/` unless shown.

**Parser, scopes, PPI**
- `pexpr-term-parsing-review.md` — PExpr's operand/term machinery (the
  `$end_pars` region): do NOT add guards there; fix = Option B (#153).
- `parser-review-s525.md` — the pass pipeline, repairs and scope machinery;
  STANDING RULE: no new scope walk, no new suffix family, no new repair pass.
- `ppi-fix-at-source-s527.md`, `ppi-upstream-bugs.md` — PPI bugs are fixed on
  the vendored branch (rule 13).
- `static-parse-limits-review-s512.md` — what a static parse cannot know.
- `detector-measurement-s513d.md` — the #2610 run-time mis-parse detector
  (`PCL_DETECT=announce|die|off|log`; default `announce`).
- `option-b-phase2-plan.md`, `b2-stale-operand-ceiling-s417.md`,
  `b1-operand-grammar-s416.md` — the list-operator / operand grammar.
- `interp-scan.md` — interpolation scanning happens in `Pl/InterpScan.pm` or
  not at all.
- `var-handling-review-s379.md`, `closure-lexical-scoping.md`,
  `declaration-ordering.md` — variable identity, renames, phases.

**Code generation, IR, compiler structure**
- `ir-spec.md` — NORMATIVE; `generated-cl-ir-review.md` — the IR reviewed.
- `codegen-review-s526.md` — the statement lowering (`_lower_block_1`,
  `Pl/Stmt.pm`), the v1 text seam (`Pl::CLForm::raw`), VarAnnotator's
  verdicts, `p-setf`.  STATUS: its C1 (the classifier) and C2's first kind
  (`local`) are DONE; the next C2 kinds and C3–C7 are open.
- `v2-target-architecture.md` (read FIRST when joining compiler work),
  `v2-code-review.md`, `compiler-duplication-review-s386.md`.
- `wantarray-context.md`, `ctx-bind-fallback-analysis-s510.md` — context.
- `first-run-from-text-design-s510.md` — `%p-run-script-cached-1`.

**Runtime**
- `runtime-review-s525.md` — `box-set`, `unbox`, `%p-bitwise-kind-of`,
  `p-setf`, `p-list-=`.
- `tie-aggregates.md` — any tie path or an I/O builtin's entry.
- `caller-implementation.md`, `point4-decisions-s539.md` — `caller`,
  `utf8::is_utf8`, the DESTROY design (three USER rulings).
- `stable-booleans-design-s516.md` — `p-bool`, `p-!`, `p-defined`.
- `method-lookup-eval-s534.md` — `p-method-call`, `p-find-overload`.
- `boxed-aggregates-design-s455.md` — elements RAW by default.
- `extensions.md`, `shipped-modules.md` — extension loading, shims, facts
  overlays.

**Speed** (read before proposing a perf round or quoting a speed number)
- `speed-low-hanging-review-s540.md`, `cpan-speed-comparison-s540.md`,
  `speed-review-s509.md`, `where-the-time-goes.md`,
  `faster-codegen-suggestions.md`.

**Tests, triage, baselines**
- `test-debugging-runbook.md` — the faillog loop, FIX vs REGISTER.
- `test-skip-registry.md`, `failure-cause-classes.md` — marking rows.
- `tap-assertion-audit.md` — before touching `cl/pcl-test.lisp`.
- `everyday-battery.md` — before quoting the EVERYDAY number.
- `cpan-board-failure-review-s521.md` — before picking a board batch or
  quoting a board number.
- `test-infrastructure.md`, `sweep-bug-catalog.md`.
- `baselines/parse-error-drop-census-s399.tsv` — the #138 drop family,
  counted; rows leave by edit.

**Product, scope, plans**
- `not-supported.md` — deliberate non-support (stable `ns-` ids).
- `production-readiness-review-s513.md` — before quoting a production date.
- `todo-features.md`, `v2-endgame-plan.md`, `MOO_MOOSE_DESIGN.md`,
  `js-target-plan.md`, `c-target-notes.md`.

**XS** (parked with pclxs)
- `xs-artifact-cache.md`, `xs-blessed-ref-referent-bug.md`,
  `xs-abi5-and-destroy.md`, `xs-shim-design.md`.

## Dependencies

Full text: `docs/claude-md/REFERENCE.md` "Dependencies".

- Perl 5.20+; SBCL **2.5.2+** (the floor is TESTED: `tools/t/sbcl-floor.t`
  under `PCL_SBCL_FLOOR`, and the container leg
  `PCL_CONTAINER_BASE=ubuntu:22.04 SBCL_VERSION=2.5.2 prove
  tools/t/install-container.t`).
- **PPI is VENDORED** (`vendor/PPI/lib`, loaded ONLY through
  `use Pl::VendorPPI;`, never `use PPI;`).  Never edit `vendor/PPI/lib` by
  hand: `tools/vendor-ppi <clone> pcl` (`Pl/t/vendor-ppi-01.t` checks the tree
  id; run the canary `PCL_SYSTEM_PPI=1 prove Pl/t/ppi-bugs-registry-01.t` at
  every refresh).  The branch's public home is `https://github.com/Percolisp/PPI` branch
  `pcl`, mirrored by the bare repo `~/pcl-agent-scratch/ppi/PPI.git`.  A PPI
  batch (an agent) fast-forwards the BARE MIRROR at MERGE-READY and nothing
  else; the reviewing session pushes the fork after the merge and refreshes
  upstream pull request #459.  Agents never push to GitHub.  `PCL_SYSTEM_PPI=1` uses the installed PPI on
  purpose.
- Non-core modules, THE ONE LIST: PPI's deps `PCLVendor::ppi_deps()` (Clone,
  Params::Util, Task::Weaken, Safe::Isa) and Moo.  Data::Dump and Try::Tiny
  are installed on CI only as transpile SUBJECTS of a few fixture rows — a
  test never uses them as a tool, and a row whose subject is a non-core
  module skips when it is absent (`eval { require X; 1 }`; guard
  `Pl/t/core-deps-01.t`).  **Nothing else that is not core perl**:
  CI is a stock machine; before adding a `use`, check
  `Module::CoreList::is_core`.  Diagnostics use core `Data::Dumper`.

## Keeping this file small

- This file states RULES and POINTERS.  A session narrative goes in
  `docs/session-log.md`, a ruling in `docs/DECIDED.md`, a measurement in the
  doc that owns it.
- **TWO companion files, never more** (`REFERENCE.md`, `HISTORY.md`): do not
  split a section out into a new file -- a reader must never have to guess
  which piece holds a rule.  A section keeps the SAME NAME here and in
  `REFERENCE.md`, and section names are not changed: briefs and code
  comments cite them ("Test Status", "Key Files 0a", "rule 12").
- "Test Status -- CURRENT NUMBERS" is ONE paragraph, replaced in place.
- A new doc gets ONE line in "Semantic Deep-Dives"; its description goes in
  the same section of `REFERENCE.md`.
- A rule that changes is changed here AND in `REFERENCE.md` in one commit.
- Agents never edit this file; the reviewing session adds what a batch needs
  at its merge.  `Pl/t/claude-md-size-01.t` fails the gate when this file or
  the memory index `MEMORY.md` passes its limit.
