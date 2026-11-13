# CLAUDE.md REFERENCE -- the full text behind every section of `CLAUDE.md`

**What this is.**  `CLAUDE.md` (loaded into every Claude Code session) has a size
limit of 40,000 characters, so since s541 (2026-11-13) it states each rule
SHORTLY.  This ONE file holds the FULL text of the working sections -- the
worked examples, the reasons, every command with its commentary -- under THE
SAME HEADINGS `CLAUDE.md` uses.  If a rule in `CLAUDE.md` is too terse to
apply, or a brief sends you to a `CLAUDE.md` section for detail, read the
section of the same name here.  There is no third place: everything that is
not working reference (the numbers history, the old review record, V1-era
notes) is in `docs/claude-md/HISTORY.md`, and no task needs it.

**How to find things.**  `grep -n "^##" docs/claude-md/REFERENCE.md` lists the
sections: Design Principles (1-13, 9a) -- Quick Reference -- WHAT TO RUN WHEN
-- Pipeline and generated artifacts (with Runtime Symbols Must Be Exported)
-- The full sweep runs its own gate (the buckets) -- Standing rules from the
review rounds -- Semantic Deep-Dives -- Dependencies.  Principle numbers are the same as in `CLAUDE.md`
and in the code comments that cite them.

**The two files must AGREE.**  The text here was moved from the pre-split
`CLAUDE.md` (`git show 8afc7dda:CLAUDE.md`) and CORRECTED where it had gone
stale (s541: rule 2's prefix, rule 6's next file number, rule 7, rule 13's
push rule, the artifact commands, the companion cadence line, the gate
cadence).  If you find `CLAUDE.md` and this file saying different things,
that is a BUG in the docs: do the stricter of the two and report it in your
STOP.md / final report.  THE CURRENT NUMBERS (gate, sweep, everyday, board,
generation) are ONLY in `CLAUDE.md` "Test Status -- CURRENT NUMBERS": a
number in this file is the number of the day its sentence was written.

**Maintenance (the reviewing session).**  A rule that changes is changed in
BOTH files in the same commit.  A new design / review doc gets one line in
`CLAUDE.md` "Semantic Deep-Dives" and its description appended to the section
of the same name here.  Agents never edit `CLAUDE.md`; an agent appends its
doc description here and writes the index line into its STOP.md.

---

## Design Principles

1. **CPAN Compatibility**: Match Perl semantics exactly. The goal is to run real CPAN modules.

2. **Readable Generated Code — but speed wins**: Keep generated CL readable for Perl programmers where it is free. Use Perl-like naming (`p-sub`, `p-print`, `$varname`; the prefix was `pl-` until the v2 runtime) so the output is recognizable. **Priority decision (2026-07-02): when readability and performance of the generated code conflict, performance wins** — never sacrifice a speed transform for prettier output. See `docs/where-the-time-goes.md` and `docs/codegen-rewrite-review.md`.

3. **Compile-Time Visibility**: Wrap sub definitions and variable declarations in `eval-when` so BEGIN blocks can call subs and access variables defined before them in source order.

4. **No Easy Write-Offs**: Don't dismiss problems as "documented limitations" without discussion. Every incompatibility is serious because it blocks CPAN code. If something doesn't work, either fix it or discuss the tradeoffs with the user before marking it as a limitation.

5. **Never Simplify Tests**: When a test fails, fix the code, not the test. This applies to both `perl-tests/` (authoritative Perl test suite) and `Pl/t/` (PCL regression tests). Do NOT weaken a `Pl/t/` test to a simpler form just because the original form fails due to an unrelated bug — keep the test semantically equivalent to what it's supposed to verify. Commenting out failing tests or replacing them with `ok(1, 'SKIP: ...')` hides bugs. If a feature is genuinely out of scope, discuss with the user first — then mark it via the **declarative skip-registry** (`cl/skip-registry.lisp`), NOT by editing the `perl-tests/*.t` file. The registry keys on the test description, cites a `docs/not-supported.md` reason, still runs the assertion, and flags itself stale if the test ever starts passing. See `docs/test-skip-registry.md`. Crashing/aborting tests are NEVER auto-skipped — they stay CRASH/PARTIAL as fix targets.  **On the CPAN board** a failing row's cause is a leading marker with a key (`#N`, `NS:<id>`, `PERL-SKIP`, `PERL-FAILS`, `UNREDUCED #N`, `HARNESS #N`; `docs/failure-cause-classes.md` "The grammar"); `Pl/t/verdict-register-01.t` checks every key, and a task is closed only together with the rows keyed on it (**CLOSE-WITH-ROWS**, s531b).

6. **Add Regression Tests for Bug Fixes**: When fixing a bug, add a test case to an existing test file that covers the fixed behavior. This prevents regressions.

   **The constraint is a file's RUN TIME, not its test count** (user, 2026-07-28, restated and refined 2026-08-01). `prove -j8` parallelises across files, so the gate's wall time ≈ the *slowest single file* — a file with many fast rows is fine, a file with a few slow ones is not. What makes a row slow: `test_transpile` runs a perl oracle **and** an SBCL transpile+run; `run_cl` spawns SBCL. So before adding, ask *how long does this file take*, and measure with `prove --timer Pl/t/<file>`; if the answer is heading past the current slowest file, start the next `transpile-test-NN.t` instead (copy an existing file's header/helpers).  **A TIMED row's bound is RELATIVE to perl's own time on the same program, measured in the run (`8 * perl + 0.5 s`), never an absolute number of seconds: CI's runner is ~3x slower than this box (s513: an absolute 1.5 s turned main red).**  The timed row is the gross-regression bound; the lever's inverse guard is a mechanism row that fails on the base. `transpile-test-01.t` (118 rows) and `transpile-test-07.t` (45, closed by the user in s321) are the two to leave alone; as of s321 the files are 01, 01b, 02, 03, 04, 04b, 05, 06, 07, 08, so the next new one is **`-11`** (`-09` landed in s321-era, `-10` exists since then; s374 added the non-transpile-test `bareword-call-01.t`).

7. **Document Complex Semantics in `docs/`**: When solving a problem involving tricky Perl-vs-CL semantics, write a `docs/topic-name.md` file explaining the problem, the solution, and edge cases. Give it ONE LINE in CLAUDE.md's "Semantic Deep-Dives" index and its full description in the section of the same name in this file (an agent appends the description here and hands the index line to the reviewing session). This prevents re-investigating the same issue in future sessions. Examples: declaration ordering, wantarray context, string escapes.

8. **wantarray/context** — three-valued implementation is done (session 163). See `docs/wantarray-impl-plan.md`. A sweep regression exists (VOID_CTX sub-body wrap in `_process_expression_statement` is too broad); fix that regression before any further wantarray work.

9. **Assume Valid Perl Input**: PCL is a transpiler for functioning Perl code, not a validator. It does not need to detect or reject invalid Perl (syntax errors, non-associative operator chains, etc.). Tests that verify rejection of invalid Perl (e.g. `eval("sub { $a <=> $b <=> $c }")` returning `undef`) are out of scope and should be commented out, not implemented.

   **9a. Fix at the right LAYER — module behavior never goes in the parser or runtime.**
   When a CPAN module misbehaves, the parser (`Pl/*.pm`) or runtime (`cl/pcl-runtime.lisp`) is the *closest* place to patch, and therefore the wrong one by default. Three layers, each owning a different kind of fact:
   - **`lib/<Module>.pm` shim** — one module's behavior: its subs, prototypes, exports, constants.  A module that must keep running unchanged gets a FACTS OVERLAY instead (`lib/PCL/Facts/<Module>.pm`: forward declarations only; `docs/shipped-modules.md` "Facts overlays"). PCL transpiles it like user code. This is where module-specific facts live.
   - **`Pl/*.pm` parser/codegen** — *generic language mechanisms*, keyed on the **mechanism, never a name**: e.g. "a `(&@)` prototype makes the trailing block parse as a block-form." The shim supplies the data (the prototype); the parser consumes it generically.
   - **`cl/pcl-runtime.lisp`** — genuine Perl *core* semantics: builtins, the box model.

   **Decision rule:** *Could a user write this thing in plain Perl?* If yes → it belongs in a `.pm` shim, and your only parser/runtime job is to make the generic mechanism work so that plain Perl transpiles correctly.

   **Smell test (hard stop):** if your diff adds a literal CPAN module name *or a non-core function name* to a file under `Pl/` or `cl/`, you are at the wrong layer — back up. (Core builtins — `grep`/`map`/`sort`/`print` — are the only exception: they are language, not modules.)

   **Worked example (session 244):** `first { … } @list` (List::Util) parse-errored.
   - WRONG: add `first => {has_block_arg=>1}` to `Pl/Environment.pm` (module data in the core).
   - RIGHT: declare `sub first (&@)` in `lib/List/Util.pm` (the *data*), and stop `_extract_module_prototypes` from skipping List::Util so the *generic* block-form parser reads that prototype (the *mechanism*).

   See `docs/shipped-modules.md` for the module-override architecture.

10. **Lisp Parenthesis Discipline**: After every Write or Edit to a `.lisp` file, immediately run the paren checker and fix any failure before reporting done:
    ```bash
    sbcl --script tools/check-parens.lisp FILENAME.lisp   # "balanced: ..." + exit 0, or an UNBALANCED line
    ```
    It uses SBCL's own reader (`*read-suppress*`, nothing evaluated), so it is
    exact: pipe-quoted symbols (`|$"|`), `#| |#` block comments, `#\(` char
    literals and `#.(...)` are all handled.  Do NOT use a textual/perl paren
    scanner — the old one-liner false-positived on `cl/pcl-runtime.lisp`
    (its `|$"|` symbol reads as an unterminated string to a scanner, s308).
    Never write a Lisp function body longer than ~80 lines. If a function needs deeply nested dispatch (e.g. a `case` with many arms inside several `let`s), extract the arms into named helper functions first, then write the short dispatcher. A function that fits on one screen has countable parens; a 300-line function does not.

    **Indentation must encode depth**: Use exactly 2 spaces per paren level. A line's indentation column divided by 2 equals the paren depth it runs at. This makes depth visually checkable without counting parens. When writing or reviewing CL code, if the indentation looks wrong, the parens are wrong. Never write a closing `)` on a line that is indented deeper than the line that opened its form.

    **Debugging paren problems: split on `defun`**: When a `.lisp` file has a paren or formatting problem, do NOT count parens across the whole file. Instead, split it into one file per `defun` in `/tmp/` by splitting on lines that start with `(defun ` at column 0 (no indentation). Format and check each chunk independently. Use the helper script:
    ```bash
    perl .claude/hooks/split-lisp.pl FILENAME.lisp   # writes /tmp/defun-FUNCNAME.lisp for each defun
    ```

11. **Reuse, Don't Duplicate — find the existing mechanism before adding code.**
    Before writing a fix, ask: *does this behaviour already exist for a sibling
    case, and can I route mine through the same path?* Most Perl features come in
    families (the named-unary `$_`-default family `uc`/`lc`/`length`/…; the
    list-operator filehandle family `print`/`say`/`printf`; the block-arg
    prototype family `grep`/`map`/`first`). A fix that copies a special-case
    branch for one member of a family is almost always wrong: it will miss the
    other parse paths the same input flows through, and it duplicates logic that
    will drift.

    **Procedure when fixing a bug:**
    - **Grep first.** Search for the data table, helper, or marker that already
      drives the sibling behaviour (e.g. the `[1,-2]` "defaults to `$_`" spec,
      `_is_print_term_start`, `add_implicit_default_param`). Read how a working
      sibling is handled end-to-end.
    - **Normalise into the existing path** rather than branching beside it. Often
      the smallest correct fix is a *single* pre-pass that rewrites your odd input
      into the shape the generic machinery already consumes (worked example: a
      bare filetest `-e` is tokenised as an Operator, not a Word, so it never hit
      the `$_`-default machinery — the fix inserts a `$_` token after it in one
      `_default_filetest_operand` pass, so *both* the single-element and
      operator-precedence parse paths handle it with zero new special cases,
      instead of duplicating the default in each path).
    - **Count the parse paths.** If the same input can arrive via more than one
      route (single-element dispatch, operator loop, funcall args, block body),
      a per-path fix is a smell — push the fix earlier, to the one place they all
      pass through.
    - **Smell test (hard stop):** if your diff adds the *same* logic in two
      places, or copies an existing branch with one token changed, stop and find
      the shared upstream point instead.

12. **A missing case DIES — never falls through to a default.** A runtime
    `cond`/`case`/dispatch over a closed set of legal values (bit widths,
    format letters, type tags, …) ends in an explicit error naming the
    unhandled value — never a `(t 0)`/`(t nil)` arm that swallows it. The
    archetype: `p-vec`'s width dispatch listed 64 as legal in its docstring
    but had no 64 branch, so `vec($x,1,64) = $q` silently wrote *nothing*
    (all-zero output that looked plausible). Silent-wrong is the worst
    failure mode in this codebase — same family as the #138 silently
    deleted statement. This is the runtime's version of the "Parser2 TODO:"
    discipline: anything not fully handled must say so loudly. (Adopted
    s317; retroactive audit is task #152.)

    **Boundary (RULED s329, `docs/fable-answers-s328.md` §1): DIE only when
    the missing case should have produced/written a VALUE the program then
    consumes.** An EFFECT-ONLY missing case (a jump, a tie, an attribute) in
    code that otherwise runs correctly ANNOUNCES itself on stderr and
    continues, with a `docs/not-supported.md` entry — measured live: making
    computed-label `goto` die cost state.t 88 verified rows while sweep-diff
    reported "0 new".  The sin is the silence, not the fall-through.

13. **Every PPI bug gets LOGGED, in the same commit as its workaround.**
    PPI is upstream software we depend on, and PCL keeps finding real bugs in
    it. Each one is worth reporting back — but only if it is written down while
    the repro is in hand, and that is exactly the moment it is easiest to skip
    (the workaround already works, so the bug feels finished). It is not: an
    unlogged PPI bug is a fix we owe upstream and never send, and it is also
    the thing a future session will re-derive from scratch.

    **When you work around a PPI mis-tokenization or mis-lex, add a section to
    `docs/ppi-upstream-bugs.md` in the same commit**, containing:
    - a MINIMAL repro (valid Perl, smallest that shows it) and what perl does;
    - the actual `PPI::Document` token/structure dump vs. the expected one;
    - the PPI version tested (`perl -MPPI -e 'print $PPI::VERSION'`);
    - PCL's workaround and which files/tests it unblocks.

    Then add the case to `docs/ppi-bug-report.t` — the self-contained
    Test::More file that IS the upstream report, where every row FAILS on
    STOCK PPI (1.291, the installed one; `PCL_SYSTEM_PPI=1`) (a failing row is the bug). Bump its `tests => N`. A **canary
    row** asserting the currently-broken behaviour is welcome in
    `Pl/t/misc-fixes-02.t`: when a PPI upgrade fixes the bug, that row fails,
    which is the signal to drop the workaround.

    **Scope (the file's own note):** only things PPI itself tokenizes or
    structures wrongly. A correct token stream that PCL's PExpr then interprets
    wrongly is PCL's bug, not PPI's, and belongs in a task instead.

    **Since s527a (DECIDED ## s527) the WORKAROUND IS THE FIX ITSELF**: PPI is
    vendored (`vendor/PPI`, the `pcl` branch), so a confirmed PPI bug is fixed on
    that branch in PPI's two-commit PR shape (its rows go into the branch's
    `t/ppi_bugs_pcl.t`, the twin of `docs/ppi-bug-report.t`), the registry row in
    `Pl/PPIBugs.pm` gets `fixed_in => 'pcl'`, and the catalogue section records
    the group and commit.  A repair pass in `Pl/` is written only when the fix
    cannot go on the branch yet, and it is deleted (one commit, corpus-diff
    identical) when the fix lands.  Lesson from the merge probes (s527a, s528): a
    PCL repair and the PPI fix can each cover shapes the other does not
    (`print $v.2` printed NOTHING on main), so a deletion is probed beyond the
    corpus -- the `print` argument slot, after `->`, statement start.

## Quick Reference

```bash
# Run all tests (from project root) — always use -j8 for parallel execution
prove -j8 Pl/t/

# THE RUNTIME IS KEPT COMPILED AND CACHED (USER s439): every runner that
# spawns SBCL through tools/lib/PCLSbcl.pm (the gate, the sweep, the companion
# suite, runpcl, pclperl-for-tests, pcl) starts from a saved core of
# cl/pcl-runtime.lisp under ~/.pcl-cache/core/, built on first use and NAMED
# by a hash of the runtime source + `sbcl --version` + ~/.sbclrc + the
# runtime's absolute path (a worktree gets its own) -- so it cannot be stale:
# an edit makes a new core and prunes the old.  Extensions (pack/mro/warnings/
# xs) load lazily from the tree and are NOT in the core.
# (#1202, s493: an extension is compiled ONCE into ~/.pcl-cache/ext/, keyed by the
# artifact's own BYTES + the runtime identity, so a regenerated artifact cannot reach
# the old entry; PCL_NO_FASL_CACHE=1 turns it off, --no-cache does not.  pack: 4.3 s -> 0.004 s.)
# Plain `prove -j8 Pl/t/` therefore runs at prove-core speed (~3:45 wall, measured s439).
# PCL_NO_CORE=1 = source mode; PCL_SHOW_SBCL=1 prints which core; `pcl
# --clear-cache` removes the cores; a failed build leaves <core>.failed (one
# hour) and falls back to source, LOUDLY.  tools/t/sbcl-prefix.t is its test.
tools/prove-core                 # == prove -j8 Pl/t/ on a FRESH temp core (belt and braces)
tools/prove-core Pl/t/foo-01.t   # any prove args
perl tools/sweep-perl-tests.pl --jobs 8          # the full sweep, ALONE on the box (it runs its own gate)
perl tools/sweep-perl-tests.pl --jobs 4          # the full sweep on a SHARED box: every agent, always (COMMON.md)
tools/ir-conform --jobs 2                        # after any cl/ or emission change: 0 fail / 0 stale

# Test single file
prove -v Pl/t/codegen-01.t

# Quick transpile test
echo 'my $x = 1 + 2;' | ./pl2cl

# THE OPTIMIZATION REGISTRY (Pl/Passes.pm, s411 Phase R, task #383): every
# speed transform is a NAMED, facts-licensed emission (Kind A) or a CLForm
# pass (Kind B, none registered yet); PCL_OPT switches them.  Kind-A names:
# raw-slot raw-numeric str-buffer foreach-range (+ insensitive-call elem-setf
# with Phase A).  A typo DIES naming the known list.  PCL_OPT=none is the
# general-form compiler: its emission must RUN identically (Pl/t/passes-01.t;
# the gate under it differs only in transpile-SHAPE rows).
PCL_OPT=none ./pl2cl < prog.pl               # everything off
PCL_OPT=-raw-numeric,-str-buffer ./runpcl x.pl   # named ones off (PCL_NO_RAW_VERDICT=1 = -raw-numeric)

# INSTALL PCL onto a machine (task #277, release phase 1).  The runtime and its
# saved core are COMPILED AT INSTALL — the XS model, never at first use.
# Copies the runtime tree (pcl, pl2cl, runpcl, Pl/, lib/, cl/, tools/lib/) in
# its repo-RELATIVE shape (the lookups depend on it), writes bin/ wrappers,
# builds <root>/pcl.core, writes <root>/VERSION (what `pcl --version` reads on
# an installed tree), and refuses to finish unless the INSTALLED tools
# transpile and run a program.  PCLSbcl picks the installed core up
# automatically; a checkout has none, so development runners are unaffected.
# EVERY script finds its tree through the ONE resolver PCLPaths::root (#1302):
# $PCL_ROOT when set, else the caller's own real directory or its parent,
# VERIFIED by cl/pcl-runtime.lisp being there — guard Pl/t/pcl-root-01.t.
tools/install-pcl --prefix ~/.local      # default prefix is $HOME/.local
tools/install-pcl --no-core --dry-run    # what it would do; no core build
tools/install-pcl --uninstall --prefix ~/.local   # removes the tree + its wrappers
prove tools/t/install-pcl.t              # its end-to-end test (not in the gate)

# THE INSTALLER IN A CONTAINER (task #1304): the same recipe the CI install
# matrix runs, locally, plus the two shapes the matrix cannot reach — a
# NON-ROOT user installing into $HOME/.local, and a SHARED install under
# /opt/pcl whose users each get their own module cache.  Run it before a tag
# and after any change to tools/install-pcl or tools/install-matrix/*.  It
# probes `podman` then `docker` and plan-skips cleanly when neither answers,
# so CI runners without a socket are fine.  The base image (ubuntu:24.04 +
# tools/install-matrix/deps.sh) is tagged by a hash of deps.sh + SBCL_VERSION
# and REUSED, so the first run pays the download and later ones do not.  The
# repo enters as a `git archive HEAD` extraction bind-mounted read-only —
# uncommitted work is NOT what it tests, and it says so.
prove tools/t/install-container.t        # minutes; never in the gate
PCL_INSTALL_CONTAINER_SKIP=1 prove tools/t/install-container.t   # skip it

# XS: build a distribution for PCL and put it where XSLoader::load looks.
# Compile happens HERE, at install time, like perl — not at first use.
# Cache key is the pclxs ABI from xs-pin, encoded in the PATH, so an ABI
# bump makes old artifacts unreachable rather than merely stale.
# See docs/xs-artifact-cache.md for the alternatives and revisit triggers.
# See docs/xs-abi5-and-destroy.md for pclxs ABI 5 / DESTROY (not done here yet).
# The ABI-6 `magic` group is IMPLEMENTED (s315, task #115): XS OO works —
# Digest::MD5->new->add->hexdigest, its own md5-aaa.t 256/256.  History and
# rules in docs/xs-blessed-ref-referent-bug.md (DONE section at the end);
# guard Pl/t/xs-03.t.  The `io` capability group (addfile/filehandles) is
# the remaining optional group not implemented here.
tools/pcl-xs-install ~/.cpan/build/Digest-MD5-2.59-0
tools/pcl-xs-install --list      # what is cached, and for which ABIs
tools/pcl-xs-install --clean     # drop artifacts built for other ABIs

# XS: run pclxs's CONFORMANCE CORPUS against PCL as a host.  This is the
# pclxs project's definition of "a host is done", and it answers a
# different question from Pl/t/xs-01.t: that file asks "does the bridge
# work at all", this asks "does PCL answer every case the way real perl
# does" -- 216 cases, with real perl as the oracle.  Minutes, not seconds
# (one SBCL launch per case set), so it is not part of the Pl/t gate.
tools/pcl-conform                # whole corpus
tools/pcl-conform 96-flags       # one case-set file

# perl's OWN t/ suite (the companion sweep).  A file that legitimately needs
# longer than the 90s default is registered in baselines/perl-suite-timeouts.tsv
# (rel<TAB>seconds<TAB>cause; effective timeout = max(row, --timeout), and the
# allowances in effect are printed per run) — never left to TIMEOUT into "no
# rows at all", which is how a file's passing rows vanish invisibly (#176).
tools/run-perl-suite.pl --all --quick --jobs 4   # THE DEFAULT FORM (#345)
tools/run-perl-suite.pl --all --jobs 4           # full: AT LEAST ONCE PER ROUND, by the reviewing session (see WHAT TO RUN WHEN)
# NB: --all --jobs 8 can OOM the 10 GB scope (each worker reserves a 512 MB
# control stack for a PCL side AND runs a perl side) — use --jobs 4 (s399).
# --quick does not run the files that spend a whole timeout to produce nothing
# (the #326 hang set) or whose registered allowance exceeds 120 s.  Each gets a
# NOT-RUN row naming which rule fired and why, and still counts as UNEXPLAINED,
# so the coverage hole is countable — never inferred from an absence.

# THE FIVE RUNNERS THAT SPAWN SBCL share one command-line builder,
# tools/lib/PCLSbcl.pm ($STACK_MB, --core placement, banner flags): the gate
# (Pl/t/PCLCore.pm), the sweep, this suite, ./runpcl, tools/pclperl-for-tests.
# A runner chooses WHAT to load, never HOW SBCL starts — one drifting runner
# is how #324 measured PCL on a 2 MB stack for months.
PCL_SHOW_SBCL=1 <any runner>      # print the exact command it spawns
# Paths OUTSIDE the checkout are derived, never written down:
# tools/lib/PCLPaths.pm (perl_suite_t -> $PCL_PERL_SUITE_T / $PERLBREW_ROOT /
# %Config).  Guards: Pl/t/no-hardcoded-paths-01.t (in the gate),
# tools/t/sbcl-prefix.t (run directly, like tools/t/tap-align.t).

# LICENSE = same as Perl, and EVERY PCL code file carries the tag (USER s401).
# A new .pm/.pl/.t/.lisp/.sh or shebang script:
tools/tag-license FILE           # idempotent; rules in tools/lib/PCLLicense.pm
tools/tag-license --check        # what Pl/t/license-tag-01.t (in the gate) checks
# Never tag files from the Perl distribution or CPAN (perl-tests/, cpan-tests/,
# the two lib/ carry-overs are excluded by name in PCLLicense.pm).

# PPI IS VENDORED (s527a, DECIDED ## s527): vendor/PPI/lib = PCL's `pcl` branch of
# upstream PPI master + its fix commits, loaded only through Pl::VendorPPI.
tools/vendor-ppi <clone> pcl                            # refresh vendor/PPI + UPSTREAM from a clone's branch
PCL_SYSTEM_PPI=1 prove Pl/t/ppi-bugs-registry-01.t     # the canary on the INSTALLED PPI ("did upstream catch up")
perl -I vendor/PPI/lib docs/ppi-bug-report.t            # the 91-row report on the vendored lib (33 ok at s527a; 14 on PPI 1.291)
```

### WHAT TO RUN WHEN (RULED s401, `docs/fable-answers-s400.md` §8 — replaces "every 3rd–5th change")

The cadence is keyed on WHAT CHANGED, because every measurement is blind to
a known set of change kinds.  Rows are ADDITIVE — every row that applies
fires.  **Always, per change:** the gate -- for a lone change `tools/prove-core`
(~4.5 min); **inside a BATCH (s477 ruling, USER): per MEMBER the TARGETED
gate** (`prove Pl/t/<your guard file> <the Pl/t files that name the area you
touched>`, about a minute) **and the FULL gate ONCE, at the END of the batch
before MERGE-READY** (a regression the final gate finds is bisected over the
per-member commits); if anything
under `Pl/` changed, `tools/corpus-diff.pl` (~2 min — READ its SILENT-DROP
line) + `tools/emission-ab.pl --ref <base> --list lib/**/*.pm` (seconds);
plus the targeted files the change names.

| what changed | full perl-tests sweep (~2–3 min since s439b's cached core; was ~10) | companion suite — **ROUND-LEVEL since s519 (USER 2026-10-17)**: Fable runs `--all --quick` + the CPAN board once per 3–4 merges on the merged main; a batch runs it only where this column says PER BATCH, plus ONE-FILE runs of the files its members name | also |
|---|---|---|---|
| `Pl/**`, corpus-diff IDENTICAL, lib byte-identical, NOT a name-resolution change | **NO — it cannot move; do not run it "to be safe"** | no | — |
| `Pl/**`, corpus-diff shows diffs | YES, after every diff is explained per file + probed vs perl | ROUND (Fable); the batch runs ONE-FILE acceptance of the files its members name | gen bump + ALL THREE artifacts regenerated (`tools/rebuild-pack` does `cl/pcl-pack.lisp` ONLY; the two other commands are under "Pipeline and generated artifacts") (staleness gate enforces it) + `tools/ir-conform --jobs 2` + `tools/ir-host-leak.pl` (the IR must stay in its three vocabularies — a new bare CL symbol in the emission is a LEAK, s470bm / ir-spec §11b) |
| `Pl/**` name-resolution / scoping / rename / capture / promotion (Parser2 `_rename_*`, `_promote_*`, VarAnnotator, GlobalPartition, eval capture, span passes) | **YES — the sweep IS the gate** (#296) | ROUND (Fable) — the per-batch gate is the SWEEP | gate-SET scan (`tools/gate-set-scan.pl`, on a base worktree and on the tree, then diff) over BOTH populations -- `perl-tests/*.t` and perl's own `t/` -- when a checker / refusal / decline WIDENS (s372) |
| `cl/**` runtime | YES (invisible to corpus-diff) | ROUND (Fable); PER BATCH only a bounded leg on files the brief NAMES (a coercion / stringification change: the brief names `op/` files -- the s421 "`cl/` coercion change runs the op/ leg" ruling, as narrowed by the s519 round-level cadence) | rule-12 read of the touched dispatch + `tools/ir-conform --jobs 2` + `tools/ir-inventory.pl` regenerated if an export moved (`Pl/t/ir-inventory-01.t` gates it) + `tools/ir-host-leak.pl` if a macro's expansion changed |
| `lib/**` shim | YES | ROUND (Fable); one-file runs of the dist files a member names | — |
| harness: `perl-tests/t/test.pl`, `cl/pcl-test.lisp`, `cl/skip-registry.lisp` | YES | **`--all --quick` PER BATCH** (the exception: both populations reach it) | baselines edited ROW BY ROW |
| runners: `tools/sweep-perl-tests.pl`, `tools/run-perl-suite.pl`, `tools/lib/PCLSbcl.pm`, `tools/pclperl-for-tests`, `Pl/t/PCLCore.pm` | the runner that changed, once; verdicts compared file-by-file | same | `PCL_SHOW_SBCL=1` before/after diff |
| `docs/**`, `tools/t/**`, memory | nothing beyond the gate | no | — |
| **any BATCH that changes `Pl/**`, `cl/**` or `lib/**`** (once per batch, not per change) | — | — | **`tools/everyday-smoke.pl`** (~2 min cold, ~20 s warm; needs no perl): quote its last line `EVERYDAY: N of M identical to perl` BEFORE → AFTER in the report.  A **NEW** row is yours to fix or explain; a **FIXED** row leaves `baselines/everyday-baseline.tsv` BY EDIT citing your batch; a **MOVED** row is re-attributed.  There is no bless option. |

**ROUND-LEVEL CADENCE (USER 2026-10-17, DECIDED ## s519; EVALUATED s523 = KEPT, DECIDED ## s523: two one-round exposures in unnamed files, both fixed forward; legs-only agents per batch 2 -> 0):** the two SLOW instruments -- the CPAN board tree half (~20 min) and the companion `--all --quick` (15-25 min, exclusive) -- are no longer per-batch legs.  Fable runs both ONCE PER ROUND on the merged main (a review worktree at main's sha) after every 3-4 merges, or at session end if any merge happened since the last round run; a mover is attributed by `git bisect` over the merge range with a one-file run per step and its row edited with the cause (or handed to the next brief).  A batch still runs the gate, the sweep (per the table), corpus-diff + emission-ab, everyday, and ONE-FILE acceptance runs of the exact board / companion files its members claim (`tools/run-dist-t.pl --rows DIST t/FILE`; `tools/run-perl-suite.pl --jobs 1 FILE`) -- a FIXED blessed row leaves `baselines/cpan-board14-fails.tsv` BY EDIT on the strength of that run.  Reason: legs + attribution cost about one agent per batch (s518d: three agents for four members).  A regression in those two can sit on main for one round -- accepted, they are baselines, not the gate; the round run is a standing duty with no skip.

**THE NUMBER THE PROJECT STEERS BY (USER, end of s494; instrument #2099,
s495):** the `EVERYDAY:` line — ordinary programs whose STDOUT + exit status
are byte-identical to perl's — not the suite pass rate.  Goal **> 90 %**
(**114 of 122 = 93.4 % at s507, 2026-10-05**; 85 of 122 = 69.7 % at the first
run, s495 — the trend is `baselines/everyday-history.tsv`).  The baseline's cause column is the steering
input: the task that holds the most programs is the next correctness batch.
It is a steering number, never a compatibility percentage
(`docs/everyday-battery.md`).  `--record` (whole corpus, clean tree) appends
`baselines/everyday-history.tsv` — Fable runs it at each merge.

Companion: **`--quick` is the default form** (#345: skips the #326 hang
set, caps registered allowances at 120 s, LISTS every skipped/capped file as
NOT-RUN); **the full `--all --jobs 4` AT LEAST ONCE PER ROUND** (I4, #993,
s465az — was "at most once per session"), on a quiet box, run as
`PCL_SESSION=sNNN tools/run-perl-suite.pl --all --jobs 4 --bless-stamps` so
`baselines/perl-suite-notrun-stamps.tsv` records which session last measured
each hole (the #326 hang set is reached ONLY by the full form); every run
prints the NOT-RUN STAMPS block, and a stamp older than the current round is
the reason to run it.  The full run also prints the ROW DIFF
(`baselines/perl-suite-fails.tsv`, the companion's row-level fail baseline)
and SHORTFALL (`baselines/row-shortfall.tsv`) blocks — read them like the
sweep's buckets (`docs/test-debugging-runbook.md` §4c–§4e); a NEW ROW is
explained or fixed, never re-blessed over.  Fable review
sessions: cold gate + full sweep + probes, plus a `--quick` companion when
the batch touched name resolution or the harness.  The three s399 tools:
`corpus-diff`'s drop counter is per-change (free); `tools/gate-set-scan.pl`
is on-demand, MANDATORY only for the checker-widening row;
`tools/drop-census.pl` is a runner column (#343) — not a separate step.

### Pipeline and generated artifacts

**Pipeline (singular, since E4.1 step 2 / #242, s356) — and ONE expression
compiler (since s411, Phase A of `docs/plan-one-compiler-s411.md`):** the v2
structured-emission pipeline (`Pl/Parser2.pm` + `Pl/VarAnnotator.pm` +
`Pl/CLForm.pm`, expressions through PExpr → `Pl/ExprToCL.pm` ONCE — the
second generator `Pl/ExprToCL2.pm` and its discarded parse are DELETED; its
two emission rules are the Kind-A gates `insensitive-call` / `elem-setf` in
`Pl/Passes.pm`) is the **only** one. **`PCL_V1`,
`PCL_V1_FILES` and the whole-file v1 fallback are GONE** — anything v2
cannot lower is now a hard error, because a fallback meant a compiler gap
silently became a re-transpile through a second compiler with different
semantics. `PCL_V2=1` is still a no-op kept for old scripts. *(`Pl/Parser.pm`
is still LOADED — v1's expression seam is called from inside v2; what went
away is the file-level v1 entry. Its now-unreachable file-level chunks are
task #243.)* Ruled refusals are perl-shaped and trappable
(`PCL: unsupported in string eval: …` → `$@`), each with a
`docs/not-supported.md` entry. `--lenient-ppi` is accepted and inert; a PPI
failure dies naming the file. Module-cache paths are keyed by
`cl/pcl-runtime.lisp`'s `*pcl-cache-generation*` — bump the generation
string on any emission-changing commit, or stale cached module transpiles
will be reused. **Plan/status: `docs/v2-endgame-plan.md` (E1–E5 + §6 the two product
targets: beat-perl speed / clear macro-IR — perf worklist in
`docs/faster-codegen-suggestions.md`)
+ `docs/v2-opus5-execution-plan.md` (**the CURRENT ordered worklist +
per-session checklist + guardrails, s316t**; reader's overview:
`docs/v2-upcoming-work.md`).** `docs/v2-opus48-execution-plan.md` (E1/E2
era) and `docs/v2-completion-plan.md` (W-phase) are background. Verify emission changes with `tools/corpus-diff.pl`; track
gates with `perl tools/v2-census.pl`.

**Generated artifacts checked into the tree — regenerate them after an
emission-changing commit, or they keep running on the codegen that built
them.** They also embed the BUILD machine's absolute paths in their preamble
(`*pcl-pl2cl-path*`, the @INC pushes, perlbrew dirs), so any future
INSTALLATION process must regenerate them on the target machine — or make
the preamble relocatable first (task #217, user note s335).
`cl/pcl-pack.lisp` is `cl/pack-impl.pl` (pack/unpack, written in
Perl) transpiled by PCL plus a hand-written appendix: run
**`tools/rebuild-pack`**, then `perl tools/sweep-perl-tests.pl --jobs 1
--timeout 380 perl-tests/pack.t` and `tools/sweep-diff.pl` (expect 0 new;
s316b: 5635 pass / 90 fail).  It was found at gen v2-30 against a v2-71
compiler — 40 generations of drift, which made every pack.t run a test of
the *old* emitter (and again at s396: v2-136 vs v2-147, eleven generations).
**`tools/rebuild-pack` regenerates `cl/pcl-pack.lisp` ONLY.**  The two
others are transpiled with `--extension`:
`./pl2cl --extension lib/mro.pm > cl/pcl-mro.lisp && tools/tag-license cl/pcl-mro.lisp`
and
`./pl2cl --extension lib/warnings.pm > cl/pcl-warnings.lisp && tools/tag-license cl/pcl-warnings.lisp`
(an emission-changing batch runs all three on its final rebased tree).
`cl/pcl-mro.lisp` comes from `lib/mro.pm`, and
**`cl/pcl-warnings.lisp` from `lib/warnings.pm`** — there are THREE compiler-built
ones, plus a FOURTH that is DATA (s496a, task #2060): **`cl/pcl-uniprops.lisp`**,
perl's own Unicode property tables (`Unicode::UCD::prop_invlist`), written by
**`tools/rebuild-uniprops`** under the oracle perl (`tools/rebuild-uniprops &&
tools/tag-license cl/pcl-uniprops.lisp`).  Its line-1 stamp is `;;; pcl-uniprops
unicode=15.0.0 perl=5.40.3 tool=tools/rebuild-uniprops` — NO `gen=`, because it
depends on perl's Unicode version, not on the compiler, so a generation bump never
makes it stale; `Pl/t/artifact-staleness-01.t` discovers artifacts by the `;;; pcl:
pipeline=… gen=` stamp and does not adopt it — `Pl/t/uniprops-01.t` guards it instead
(regenerates and compares bytes; checks the stamp's Unicode version against perl's).
**The drift is now a GATE**: `Pl/t/artifact-staleness-01.t` compares each
artifact's line-1 `gen=` stamp against `*pcl-cache-generation*`, so bumping
the generation without regenerating fails a row the same session (s399,
task #331).  The artifacts are discovered by that stamp, not listed.

### Runtime Symbols Must Be Exported

When adding new special variables or functions to `pcl-runtime.lisp`, they must be exported from the `pcl` package. Generated code runs in user packages (e.g., `|File::Basename|`) that `(:use :pcl)`, so unexported symbols won't be visible.

Example: `*wantarray*` must be in the `:export` list, otherwise `(let ((*wantarray* t)) ...)` in a user package creates a different variable.

```lisp
;; In defpackage :pcl
(:export
 ...
 #:*wantarray*   ; Context variable - MUST be exported
 ...)
```


### The full sweep runs its own gate: the buckets

(Moved here from the old "Test Status" section: the mechanism is working
reference, the row counts quoted in it are the counts of the day.)

- **The full sweep RUNS ITS OWN GATE (s330, #204)**: `perl tools/sweep-perl-tests.pl
  --jobs 8` with no file arguments ends by running `tools/sweep-diff.pl diff
  baselines/fail-baseline.tsv .faillog` and **exits with that verdict** (`--no-gate`
  opts out; a sweep of named files stays informational).  The diff now has a
  **fourth bucket, LOST** — baseline PASSING rows the run did not produce,
  compared against `baselines/pass-baseline.tsv` (`sweep-diff.pl save-status .faillog
  baselines/pass-baseline.tsv` re-blesses it).  The first three buckets read failing
  rows only, so a change that makes a file abort EARLIER used to report
  `0 new / 0 fixed` while coverage evaporated (s328: state.t 157 → 69).  Every
  run prints `TOTAL passing: baseline N, current M`, and when no pass baseline
  is found it prints `LOST: NOT CHECKED` rather than nothing.
  **FIFTH bucket, DROPS (s402, #343)** — statements the compiler could not
  lower and replaced with `nil` (#138 family).  They are invisible to every
  other bucket: the row simply is not there (bless.t's drop IS a test row, in
  a file the sweep calls passing).  The sweep records a per-file `drops` count
  in `.faillog/_status.tsv` (columns: name, status, pass, fail, planned,
  **drops**, note) and `sweep-diff.pl` compares it against
  `baselines/parse-error-drop-census-s399.tsv` — **the census IS the baseline**, a
  drop leaves it by EDIT; more drops than the census fails the run like a NEW
  failure.  `tools/run-perl-suite.pl` prints the same comparison for perl's
  own t/, and every transpile now announces a drop as it happens:
  `PCL: statement dropped at F line N: <text> -- <reason>` (task #339; OFF in
  `pl2cl --module`, the runtime's module load — `PCL_DROP_ANNOUNCE=all`
  forces it back on).
- Full `perl-tests/` sweep, CURRENT (s406, re-run after #348 switched
  `which_perl`'s children to PCL): **TOTAL passing 18517** across 108
  files, GATE clean, drops 12 = census — identical to s405 in every bucket
  (closure.t stays OK 272/4, pack.t OK 5636/89).  The one row above the s399
  number is `ref.t` 190 → 191,
  edited into `baselines/pass-baseline.tsv` by hand with its cause (s404l's
  blank-line fix in `tools/pclperl-for-tests`, attributed by bisection in a
  worktree).  The s399 measurement it sits on: **704 blessed fails**,
  **64 files fully passing**, TOTAL passing 18516 across 108 files.  The
  s399 move is the only one that is not a regression to chase: task #323 made
  `warning_is`/`warning_like`/`warnings_like` in `perl-tests/t/test.pl` stop
  manufacturing a pass, so 24 rows (assignwarn.t 20, hashassign.t 4) became
  honest failures with ONE cause — PCL emits no warnings-gated diagnostic
  (`not-supported.md` "Warnings-gated diagnostics are absent", owner #221).
  Both baselines were edited ROW BY ROW, never re-blessed, and
  `baselines/pass-baseline.tsv` carries a header note saying so.
  Historical baseline text follows.
- Full `perl-tests/` sweep: **679 blessed fails** in `baselines/fail-baseline.tsv`,
  **65 files fully passing**, 18499 passing / 910 failing across 108 files
  (re-measured s356 at the E4.1 flip; GATE clean, +0.  The flip moved three
  files, all pre-authorized and edited into the baselines with their causes:
  **eval.t 110 → 114** — the ruled eval refusal makes the four `my $$x`-family
  rows fail as perl does, so they pass (and leave `fail-baseline` by EDIT);
  **tr.t 241 → 239** — `tr.t:494` builds a 73,769-char `eval` string, the one
  F6 oversized-run-form event, a ruled refusal per `fable-answers-s346.md`
  §2.3; **lex.t 46 → 45** — the #228 `[perl #129069]` registration, whose pass
  was an accident of `--lenient-ppi` truncating NUL source to nothing.
  (re-measured s341 on a COLD cache; `sweep-diff.pl diff
  baselines/fail-baseline.tsv .faillog` = **0 new / 0 fixed / 0 LOST**, plus 2 rows
  the tool
  itself flags UNSTABLE — new fails ABOVE the abort point of postfixderef.t and
  ref.t, both already PARTIAL, so they are crash-file noise, not regressions.
  **Both baselines were re-hygiened in s341 (#223)**: the two now-passing
  scalar.t rows edited out of the fail baseline, and `baselines/pass-baseline.tsv`
  re-blessed from that run after a per-file audit — it had been blessed at
  s337b from a run OLDER than its own commit, which is why every later session
  read a phantom `+8`.  `save-status` now stamps `# taken-at: <sha> <date>` so
  a stale bless is visible instead of inferred.
  689 → 683 in s330: the `scalar()`-never-dereferences fix made 7 rows pass
  (removed by EDITING them out, never by re-blessing from a run, which would
  silently absorb anything else that moved) and exposed 1 row that had been
  passing only because `scalar()` flattened both sides to undef — added by
  hand with its cause.  s323 had removed the `do.t` "$! is EISDIR on do dir"
  row the same way.)  Baseline from s315d —
  class-model target-first reads, $TODO honored again under :invert,
  no-match s/// write gate; fresh_perl/runperl children run under PCL via
  `tools/pclperl-for-tests`; `PCL_FRESH_PERL=real` restores the old compare
  mode.
  **pack.t and the TIMEOUT retry (#176, s322).**  A file that TIMEOUTs
  contributes NO rows, so `sweep-diff.pl` can only report its baseline
  failures as *unverified* — a regression inside a timing-out file is
  invisible, and the headline "0 new" says nothing about it.  pack.t was
  merely SLOWER than the timeout (~166 s at s322), not hung, so the sweep now
  **retries a TIMEOUT once at 3× the timeout, at the end of the queue**
  (`--no-retry` disables); no operator has to remember `--timeout 400`.
  Measured s322: pack.t completes at 5636 pass / 89 fail and its failures are
  **identical to the blessed baseline** — 0 new, 0 fixed.
  **s334 (#184): pack.t is now ~69 s — under the 90 s default.**  Its minute
  was COMPILE time, not run time (78.5 s transpile → 7.0 s; the run is 54.7 s
  either way), and the cause was a per-token tree-walking predicate in
  `_rewrite_var_uses`, not the regenerated artifact the task suspected.  The
  retry stays as the backstop for contention.
  **CORRECTION (s322): the earlier claim here that "pack.t has NO rows in the
  blessed baseline" was FALSE** — `baselines/fail-baseline.tsv` has always carried
  its 58 pack.t rows.  The claim came from `grep`ping that file, which
  contains NUL bytes: grep then treats it as binary and prints nothing.  Use
  `grep -a`, or perl, on any `.tsv` under `.faillog/` or `baselines/` (the
  blessed baselines moved from `docs/` to `baselines/` in s440 — USER: docs/
  must stay browsable).
  See `docs/sweep-bug-catalog.md`

## Standing rules from the review rounds

The same rules as the section of this name in `CLAUDE.md`, each with where it
came from and what it guards against.  (The narrative of each round is in
`docs/session-log.md`; `docs/claude-md/HISTORY.md` part 2 is the compressed
record, kept only as history.)

- **Errors fail in the SAME PLACES as perl and are TIDY; the exact text is
  not a goal** (USER, refined s494).  One line, never a Lisp backtrace.  A
  board / suite row that differs from perl only in message TEXT is keyed to
  its `ns-` id, not fixed.
- **`eval STRING` must be supported** (USER, 2026-07-07): never gate a string
  eval as unsupported; a refusal inside one is perl-shaped and trappable
  (`PCL: unsupported in string eval: …` in `$@`).
- **A statement the compiler cannot lower is a perl-shaped, trappable
  run-time die at the drop site** (RULED s433, shipped as "the flip"): ONE
  shape for every drop in every mode, no classifier.  The `;; PARSE ERROR`
  comment stays in the emission for the census; the transpile announces
  `PCL: statement dropped at F line N: <text> -- <reason>` on stderr; a drop
  inside a string eval dies there (s407/s409: "announce and continue" keeps
  the wrong value).  The census (`baselines/parse-error-drop-census-s399.tsv`)
  IS the baseline: a drop leaves it by edit; more drops than the census fails
  the sweep like a NEW failure.
- **Review probes run perl → BASE → tree** (s516 on).  A probe the base
  passes has not reached the bug.  A claimed bar must be on disk: a log whose
  last line is `=== heavy.sh <label> ... END rc=...`.
- **Expectation-rewrite rule** (s377, the "four conjuncts"), for any edit of
  what a `Pl/t` row EXPECTS: (1) the new text is perl-probed; (2) the diff is
  exactly the divergence; (3) the edit STRENGTHENS the row; (4) a guard row
  lands in the same commit.  A `Pl/t` expectation can encode the old bug
  (s438: `glob-01.t` asserted the one answer perl never gives).
- **A commit that removes a refusal greps `Pl/t` for the refusal's message
  text** (s416): a stale twin guard of the removed refusal otherwise fails the
  gate a session later (it happened twice: parser2-02.t t60, t62).
- **A detector, its rewriter and its promoter share ONE resolver** (s372,
  s438 #454): fixing only the detector turns a refusal into a SILENT WRONG.
  A sigil- or scope-blind capture refusal is a bug: delete or rename it,
  never narrow it.  When a fix widens what a CHECKER sees, the gate-SET scan
  over both populations is mandatory (the WHAT TO RUN WHEN table).
- **A fix that makes values real exposes rows that were passing on nothing**
  (s418, s438): read each newly failing row for its CAUSE, and edit the
  baseline by hand with that cause.  A COUNT is not a finding.
- **"Pre-existing" is WHEN, not WHY** (s421): a companion or board mover is
  spliced into a baseline only with its cause, attributed (bisect, one-file
  runs).  A `--quick` NOT-RUN row is never a mover (s409).
- **Bucket counts need the row TOTAL in both directions; a row number is
  meaningful only within its own run** (s386: a by-number TAP join across two
  runs put a failure in the wrong region).  TAP descriptions are the join
  keys.
- **A capture-dependent emission keys the eval cache** (s387).  **A
  diagnostic that can fire during a RUN answers "and on a warm cache?"**
  (s403: a module's cached transpile does not re-run the compiler, so a
  transpile-time message is silent on the second run).
- **A census increase is legal when it converts a worse failure (a crash)
  into a counted drop** (s409).
- **Compare a gate count against a measurement of the SAME tree** (s409).
- **No pipe after `git rebase` / `git merge` in a command chain** (s540: a
  `| tail` hid a failed rebase and the chain ran on); verify a fast-forward by
  its own exit status.
- **Never `git stash` for a compare**: a worktree (or a `git archive`
  extraction) at the base sha.  Never `cp` a snapshot over a live file.
- **The `cl/` coercion / stringification rule** (s421: such a change runs the
  `op/` companion leg) lives in the WHAT TO RUN WHEN table's `cl/**` row since
  the s519 round-level cadence: the BRIEF names the `op/` files as the
  batch's bounded leg.

## Key Files to Read: the Semantic Deep-Dives

### Semantic Deep-Dives (read before touching these areas)
- `docs/pexpr-term-parsing-review.md` - **READ BEFORE TOUCHING PExpr.pm's operand/term machinery** (the `$end_pars` region, named-unary operands, postfix `->` chains, ~lines 2600-3700). The region is a maze of hand-derived boundary conditions; three s316v attempts to add a rule there all failed in ways probes did not catch (task #142 records them). The fix for that region is Option B (`_reduce_term`, task #153) — do not add guards there.
- `docs/b2-stale-operand-ceiling-s417.md` - **Track B2 (#343), MECHANISM LOCATED (s417), fix NOT designed**: `$last_low_prio_op` is an index saved during handle_subcalls's right-to-left scan and invalidated by the scan's own splices, so a paren-less list operator's argument list swallows the `or`/`and` that should end it. Has the trace table, the node dumps, the 658-file population scan (10 events / 3 sources) and the perl-probed verdict per site — including that the family's shift≥2 half is a SILENT WRONG the drop census cannot see, and that one of the three sites is stale-but-benign. Read before designing the fix.
- `docs/option-b-phase2-plan.md` - **Option B phase 2, SIZED from the census TEXT (s407)**: the #138 drop census is ~300 feature absences / ~40 term grammar / ~15 lexer bugs, so phase 2 is three tracks (refusals #371 → the named-unary operand grammar #372 → #343 → fillers → the announce→DIE flip), NOT a rewrite of `parse()`'s main loop.  Recipe: `tools/drop-census.pl` (counts) + `tools/drop-harvest.pl` (the text).
- `docs/v2-target-architecture.md` - **THE TARGET SHAPE of the v2 compiler** (s316t): the pipeline as data transformations, the Stmt-classifier/Facts/CLForm data structures, the pass plug-in contract for optimizations, module map, and the calibrated cost to v2-final. Read this FIRST when joining the compiler work; the E5 steps in `docs/v2-endgame-plan.md` converge on it.
- `docs/v2-code-review.md` - **The s316t design review** that produced the target: the token-splitter bug family (three precedence tables + probe-confirmed silent-drop), the seam-state inventory, the E4.1 reachability re-scope. Gap analysis behind the architecture doc.
- `docs/compiler-duplication-review-s386.md` - **USER-asked duplication/inefficiency review (s386, measured by full-corpus call trace)**: v1 is still the PRIMARY expression compiler (88% seam fallback, ExprToCL2 native ~12% — #153 FOLD's metric + chunk 0); ~3.5k lines confirmed dead (pre-E2 text emitters, BlockAnalyzer, W12 remnants — task #303, after #291); compile-time hot spots (DEBUG 4.3M calls, accessor churn, `_flat` ×1.24M → #213).
- `docs/ir-spec.md` - **THE TRANSLATOR'S MANUAL for the generated CL** (s277, normative): data model (box/undef-vs-nil/aggregates/refs), coercion + truthiness tables, the *wantarray* context protocol, p-sub calling convention, control flow + non-local exits, OO dispatch (C3, string method names), magic globals, load model, op-family rules. Read this to translate PCL output to another environment; runtime is the reference where the doc is silent.
- `docs/generated-cl-ir-review.md` - **Review of the generated CL as an IR** (s277): the output's structure/vocabularies/shape tables, what to keep, ranked friction list (raw seams, control chars in strings, unstructured regex literals, host-idiom constructors, context-bind noise) with zero-runtime-cost fixes, and the consumer contract for translating to other targets. Supersedes stale `CODEGEN_DESIGN.md` §naming until that file is rewritten.
- `docs/caller-implementation.md` - How `caller()` reports the calling frame's **package** (dynamic `*pcl-current-package*` + per-sub caller stack; orig-case carried out-of-band since single-segment pkg names are upcased into CL packages). Cost analysis included.
- `docs/declaration-ordering.md` - Perl vs CL compile/load phases, defvar/defun ordering, local/dynamic scoping
- `docs/wantarray-context.md` - Wantarray/context system (work authorized 2026-05-29; previously deferred)
- `docs/ppi-glob-disambiguation.md` - **RESOLVED (fixed in PPI 1.291)**: PPI used to misread `< expr >` as glob and drop statements; now tokenizes correctly. Regression guard in `Pl/t/misc-fixes-02.t`. See `docs/ppi-upstream-bugs.md`.
- `docs/closure-lexical-scoping.md` - **NEXT TODO**: Why `defvar` breaks closures, plan for `$x__lex__N` renaming
- `docs/todo-features.md` - **Features left to implement** (tiered, with test counts and fix areas)
- `docs/not-supported.md` - **Deliberate non-support** (design decisions: `@_` aliasing, Unicode limits, etc.)
- `docs/v1-implementation-plan.md` - **V1 feature plan** (prioritized, with full implementation details for each item including `local $hash{key}`, bare-if return, string eval, etc.)
- `docs/test-infrastructure.md` - **Test infra notes**: why SBCL startup is slow, `fresh_perl_is` limitations, saved-core optimisation
- `docs/test-skip-registry.md` - **Marking not-supported tests**: declarative skip-registry (`cl/skip-registry.lisp`) instead of editing `perl-tests/*.t`; keyed on description (or test-number for unnamed); stale-detector; failure log + `tools/sweep-diff.pl`; crash/PARTIAL stay as fix targets, never auto-skipped
- `baselines/parse-error-drop-census-s399.tsv` - **The #138 family, counted**: every file whose emitted CL contains a `;; PARSE ERROR:` progn (a statement the compiler could not lower, replaced by nil and execution continuing) — 33 files / 106 drops as of s420 (72/379 at the s399 measurement), with the compiler's own message per file.  Rows leave BY EDIT with their cause, never by re-blessing. A drop is NOT cosmetic: bless.t's is a test row that never runs, in a file the sweep reports as passing. Task #343 has the minimised trigger and says the fix belongs in Option B phase 2, not in the `$end_pars` region in place.
- `docs/tap-assertion-audit.md` - **What the TAP layer can and cannot claim** (#202, s330): the per-function inventory of reachable failure paths, the ten findings (unlike could not fail; eq_hash had never run; cmp_ok manufactured verdicts for `<=>`/`cmp`/`=~`/`!~`), the rule that **a claim that cannot be evaluated reports `not ok` naming the reason and only `plan()` dies**, why TAP descriptions must be Test::More's (they are join keys), and the two deliberate non-changes. Read before touching `cl/pcl-test.lisp`.
- `docs/test-debugging-runbook.md` - **HOW-TO procedure**: the faillog-driven inner loop, the FIX-vs-REGISTER decision tree, the skip-migration steps, baseline re-blessing. Read this before triaging perl-tests failures.
- `docs/xs-artifact-cache.md` - **XS artifact cache + XSLoader::load**: where a shim-built .so lives (`~/.pcl-cache/xs/abi-N/auto/...`), why the key is the pclxs ABI encoded in the PATH, why the compile is at install time, and what would change each decision. Written as decisions-with-alternatives because this is new ground.
- `docs/xs-blessed-ref-referent-bug.md` - **XS OO: DONE end to end (s315, task #115).** The full history of the blocker (s314 diagnosis → pclxs ABI-6 magic group → PCL's `xs-magic-set`/`xs-magic-get` + the `xs-ref-target` referent-identity fix + the 64-arg argv cap removal) with the rules that made it correct. Read the DONE section before touching magic/ref_target; guard `Pl/t/xs-03.t`.
- `docs/xs-abi5-and-destroy.md` - **what pclxs ABI 5 changes here, and what it costs**: nothing is broken (filehandles are the first OPTIONAL vtable capability group, so the pin can stay at abi 4), but DESTROY is now callable and needs no ABI bump — an unimplemented destructor leaks the C side of every T_PTROBJ object, which is bounded in a script and unbounded in a long-lived image. Has the performance section: cache `pclxs_has_destroy` per CLASS or pay a bridge crossing per finalized object.
- `docs/extensions.md` - **Extension loading**: `p-load-extension`, self-loading stubs, standalone binaries, adding new extensions
- `docs/point4-decisions-s539.md` - **The three USER decisions of the compatibility plan, sized (s539)**: the UTF-8 flag (#1389: the content rule emulated under real perl -- 273 failing rows become 20), `caller` fidelity (#233 + #1361: 458 board rows; tier 1 reads the registers `die` already has), scope-exit `DESTROY` (#2370: the parked Stage 2 reaches none of the board's whole-file losses).  Options and a recommendation each; nothing decided.  Read it before touching `utf8::is_utf8`, `caller` or the destroy design.
- `docs/cpan-speed-comparison-s540.md` - **The CPAN board timed under perl and under PCL, file by file (s540a, #4787)**: it is COMPILE, not run -- perl 64.9 s CPU, PCL 2,154 s (33.2x) over the 638 files PCL runs as perl does: 82 % loading (the dist's own modules compiled from text every run 1,412 s, the Test::More layer from source 225 s), 10 % transpile, 8 % run; the ranked list of what should be faster with each lever's measurement (#4961-#4967, #4986), the known levers the board confirms or does not show, and the method (`tools/board-timing.pl`: the board's way of running a file, the phase split by an encapsulating hooks fasl; CPU is NOT load-proof here -- normalised by the TAP-layer compile).  Per-file table `baselines/board-timing-s540.tsv`.  Read it before quoting a CPAN speed number or picking a perf batch.
- `docs/speed-low-hanging-review-s540.md` - **The rest of PCL read for low-hanging speed fruit (s540b, #4788)**: the emitted code's shapes, the runtime's hot helpers, the shims against perl's XS, the compile pipeline, each suspicion measured on a scratch copy; THE LIST of nine filed levers (#4981-#4985, #4988-#4991: `tr///` re-expanding per call is a Data::Dumper program's whole 12x, a literal `use constant` is a call at every use, `$|` makes every print probe a weak table, Digest::MD5 / dclone / base64 / canonpath / strftime in the shims), the refuted ones (the void-position bind, a string-compare plain arm, the detector table's bytes), the "not low-hanging" table with what each waits for (local modules compiled from text every run #4986, the policy, the frame, `caller`, boxes for references), and the older worklists item by item.  Read it before proposing a perf round.
- `docs/speed-review-s509.md` - **Where the remaining speed is, and what is cheap** (s509, the USER's question): start-up re-measured (a warm one-liner is 53 ms, half of it the Perl launcher -- the parked #2422 / #1862 / #2421), thirteen slow bench rows profiled and classified, four run-time levers filed with measurements (#2770 typed string copy: two rows halved by five lines; #2771 `print`; #2772 scalar `grep`; #2773 `unshift`), and what is NOT cheap. Read it before proposing a perf round.
- `docs/ctx-bind-fallback-analysis-s510.md` - **The list-context bind around built-in calls (#2775), ANALYSED and RULED (s510)**: the catch-all `(p-list-ctx …)` on every built-in call in a list slot goes, once the eight context-reading built-ins it was hiding are in `%WANTARRAY_SENSITIVE` (#2801); `join`'s bind stays until #2803. Has the counts (3,012 binds over 318 files; density 11.1 → 7.1 per 100 lines), the trial's gate / sweep / everyday, the four gate rows that change and the exact change for the batch (`s510/s510c-prompt.md`). Read it before touching the funcall context rule in `Pl/ExprToCL.pm`.
- `docs/static-parse-limits-review-s512.md` - **What a STATIC parse of Perl cannot know (s512, the USER's question)**: the seven families of parse facts perl learns by RUNNING code (BEGIN / import / pragmas / filters / keyword plugins / @INC / eval) and how PCL supplies, guesses or misses each; 18 probes; NEW #2871 (a prototype applies ABOVE its definition -- the table is whole-file, perl's is positional), #2872, #2873 (`use autodie` silent), #2874 (`use bigint`); the known discussion (Kegler 2008, PPI, Guacamole, Perlito5) and why the run-time detector (#2610) stays PCL's route.  Read it before promising a parse fact that only running code can produce.
- `docs/detector-measurement-s513d.md` - **The #2610 detector's first measurement (s513d)**: how PCL recognises, at run time, a call site whose parse a BEGIN-time sub install contradicts; 374 distinct sites / 69 names over the four populations, the five shapes with their verdicts (harmless constants, two real families, a registry gap behind the noise), the USER's ruling (a FLAG: announce vs die), the gate's 53 sites in full.  Read it before touching the detector or deciding its default.  **§6 (s514d): PRODUCTION MODE -- the table is emitted in every unit by default, `PCL_DETECT=announce|die|off|log`, the per-population line counts (gate 71 / sweep 17 / everyday 32 / board 63) and what they are (mostly #3143, the import-prototype gap); the DEFAULT was a USER decision: `off` from s514f (2026-10-11) until the import-tag gap closed; **§9 (s516a, 2026-10-13): the default is `announce` AGAIN** -- after #3184 + #3206 + #2879 the recount in log mode reads gate 0 / sweep 0 / everyday 0 / board 23 (18 real Sub::Uplevel `CORE::GLOBAL::caller` sites + 5 harmless Getopt-Long `GetOptions` `(@)` sites, #3261); `PCL_DETECT=off` stays available by name.**
- `docs/tie-aggregates.md` - **`tie` on scalars, arrays, hashes AND (§7, s514d) filehandles**: the empty-shell + side-table design for aggregates, why a handle has no free miss branch and how the one-counter entry test was measured free (a typed sentinel cost fhprint 11 %), the tied-handle method table and what `local *FH` undoes.  Read before touching any tie path or an I/O builtin's entry.
- `docs/production-readiness-review-s513.md` - **How long to the end of the bug queue, how long to production (s513, the USER's question, 2026-10-10)**: the queue is a FRONTIER (770 open; 13 closures / 18 filings a day; the number that can reach zero is the silent-wrongs inside a SUPPORTED SUBSET, 105 today, ~2 months); production in three TIERS -- your own scripts NOW with a diff-test gate, pure-Perl CPAN apps 3-5 months (two parked USER rulings hold it: scope-exit DESTROY #2370, caller fidelity #233), XS not on a measurable path; the measured module-load table (Encode / Storable / File::Glob / Digest::MD5 do not load); the recommendation order (Encode shim first).  Read it before quoting a production date.
- `docs/first-run-from-text-design-s510.md` - **The first run of a program (#2702), DESIGN measured with a prototype, nothing built**: today the run that builds a program's compiled file loses a module's load-time output, runs a printing `use` before an earlier BEGIN and imports twice (4 of 13 probe programs match perl; the second run 11 of 13). The design: run that first run from the transpiled TEXT and build the compiled file in the last exit hook, in the same image — every module body exactly once. Has the guards the build needs (same pid, compile phase completed, ordinary exits only), the precondition (#2764's plain-stream loader for the script text), what was decided against (a forked or a fresh builder process) and the bar. Read it before touching `%p-run-script-cached-1`.
- `docs/stable-booleans-design-s516.md` - **perl 5.36+ STABLE BOOLEANS (#3242), DESIGNED, nothing built (s516)**: perl's boolean is the shared `"1"` / `""` string buffer's identity, so PCL's is two interned strings `+p-yes+` / `+p-no+` returned by the ONE `p-bool` helper, `is_bool` = `eq`, plus two `eq` fast arms (truthiness, to-number) that keep the counting idiom in the bench band; the struct and box-flag alternatives are rejected there with reasons; §6 is the bar for the brief that builds it.  Read it before touching `p-bool`, `p-!`, `p-defined` or `builtin::is_bool`.
- `docs/cpan-board-failure-review-s521.md` - **The CPAN board's known failures by CAUSE, with the fix plan (s521, the USER's question, 2026-10-18)**: every blessed row's cause with its task status and a layer / size / blocked-by verdict, the ~98,000 rows the board never RUNS behind its `*PLAN*` / `*FILE*` lines, the stale rows, the six ordered batches (batch 1 = Capture-Tiny's two real mechanisms: `PerlIO::get_layers` missing + the `\Q`-newline regex bug, #3510) and the "NOT IN THE PLAN" cost table of the standing rulings (#233 + #1361 hold 237 rows).  Read it before picking the next board batch or quoting a board number.
- `docs/ppi-fix-at-source-s527.md` - **PPI FIXED AT THE SOURCE, not repaired after the fact (s527, the USER's question, 2026-10-24 evening; RULED)**: 12 token bugs fixed in PPI's own tokenizer in 54 lines (PPI's suite green, its own TODO "(1)-2" fixed, our report 14 -> 33) against 1,313 lines of PCL repair subs and a 0.95 s vs 0.25 s parse entry; upstream measured (101 unmerged bot PRs, our June issues unanswered); the decision = a `pcl` branch on upstream master, vendored as `vendor/PPI`, every fix a two-commit upstream PR, PCL's repairs deleted row by row (brief `s527/s527a-prompt.md`); the remaining token rows with their PPI sites in §6.  Read it before touching any `_repair_*` / `_fix_ppi_*` / `_merge_*` pass or `vendor/PPI`.
- `docs/parser-review-s525.md` - **THE PARSER REVIEWED AGAINST PERL'S OWN FRONT END (s525, the USER's question, 2026-10-24)**: sane as an artifact, not as a structure -- 69 passes in `Parser2::parse` (18 may re-serialize and re-parse the document), 18 scope-walk implementations, 34 term/operator predicates, ~35 PPI repair passes, 46 % of compile time in `PPI::Node::find`; perl's corner cases come from THREE single states (toke.c's `PL_expect`, the keyword classes with one perly.y rule each, pad.c's binding table) that PCL re-derives as unrelated special cases.  Three routes sized: copy the three states into the present shape (Option 1, 12-18 sessions, byte-diff gated), an own toke.c-shaped lexer (Option 2, not first), translate perl's OPTREE through `B` (Option 3: exact by construction, ~43k of 54k lines replaced, 40-60 sessions).  **USER RULED (same day): Option 3 OFF -- an exact perl version at transpile time is unrealistic, and the compiled-code cache would freeze the compile-time environment (perl FOLDS on it).  LIVE RECOMMENDATION (§5 box): Option 1 NOW in one slot per round, CHECKERS FIRST (an inert expect-state checker + the binder in dual-run), then 1a -> 1c -> 1b, 12-18 sessions (task #3694); standing rule: no new scope walk, no new suffix family, no new repair pass once the checker exists.  Read it before touching the pass pipeline, the repairs or the scope machinery.
- `docs/runtime-review-s525.md` - **THE RUNTIME REVIEWED AGAINST PERL'S OWN IMPLEMENTATION (s525r, the USER's question, 2026-10-24)**: sane as an artifact and, with three named exceptions, as a structure -- one coercion path, one value reader (`unbox`) with tie and magic inside it, duplication 0.85 %, growth = perl's surface; 25 op probes vs perl, 20 identical, the 5 divergent all the REPRESENTATION (the ruled IV/UV/NV faces, the missing "was numified" flag: #1040, #3760).  The exceptions: box-or-raw dispatch written per site (~1,000 type tests, 5 `typecase`s), the box has perl's caches but not its flags (the string-vs-number guess has an exact answer in the representation), 12 shape-dispatching macros (`p-setf` 16 arms) = a second lowering layer.  Recommendation R1-R6 (§10): three named repairs + one measured refactor, no restructuring program.  Read it before touching `box-set`, `unbox`, `%p-bitwise-kind-of`, `p-setf` or `p-list-=`.
- `docs/method-lookup-eval-s534.md` - **METHOD LOOKUP EVALUATED (s534a, the USER's ask, 2026-11-08; recommendation awaiting the USER)**: own-class calls are 1.2x of perl, INHERITED ones had never been measured -- the four new bench rows read `methinh` 3.98x, `methinh3` 7.85x, `methpoly` 3.74x, `ovlbool` 6.53x, ~85 % of it the by-name `@ISA` re-walk (incl. an unguarded weak-table probe in `%p-isa-symbol`, 11-19 %).  Six prototypes A/B-measured + a 61-case probe matrix: #582's invalidate-on-every-`@ISA`-write design is REJECTED (an `@ISA` is a bare vector, so the mark taxes every array write -- PPI +19 % -- and a missed writer is silent: 12 of 61 cases went stale); VALIDATING a cached resolution (hit and miss, per (class, method), plus the ancestry) by re-reading each passed class's `@ISA` names and `pl-NAME` symbol gives 1.3-1.7x of perl and PPI 4.4x -> 2.75x with no audit (#4523 #4524 #4525).  Read it before touching `p-method-call`, `p-find-overload`, `%pcl-isa-ancestry` or the own-class cache.
- `docs/codegen-review-s526.md` - **[STATUS s541: C1 is DONE -- the statement classifier `Pl/Stmt.pm`, s533a / MAIN-READY-68, `_lower_block_1` is 49 lines now, not the 1,188 described below; C2's first kind `local` is DONE, s533c / MAIN-READY-72; the next C2 kinds and C3-C7 are open.]** **THE CODE GENERATION REVIEWED AGAINST PERL'S OP TREE AND THE IR SPEC (s526c, a Fable fork, the USER's third review, 2026-10-24)**: sane as an artifact and, in its EXPRESSION half and its registry, as a structure (one expression compiler, 95 IR heads; one printer; 24 Kind-A names + one Kind-B pass, `PCL_OPT=none` honest; duplication 3.2 %; the code generation is 6.0 % of compile time -- PPI is 60.5 %); the debt is the STATEMENT lowering, which has the parser's shape: no statement classifier (`_lower_block_1` 1,188 lines of `isa`/`content` shape tests at the point of use), the v1 TEXT seam as the DEFAULT for 1,058 of 20,825 corpus statements (`local` 337, `use` 244, `BEGIN` 147, `no` 133, signatured subs 89, loop-modifier `return` 37, `require` 34 -- 33 v1 `_process_*` stay live, 53 text heads, 160,000 `is_raw` calls per module), the mutable `_let_bound_vars` accumulator (51 uses).  The compiler NAMES 92 % of stores; `p-setf` is one store in thirteen (483 of ~6,200) -- the runtime review's "second lowering layer" is the tail; the representation verdicts are sound and conservative (90 % of lexicals stay boxed; a raw slot drops exactly one perl bit, #3780); context is DYNAMIC where perl's `OPf_WANT` is static (13.3 binds per 100 emitted lines; `p-void-ctx` the most frequent IR head, 8,636).  **RECOMMENDATION ADOPTED (DECIDED ## s526): no separate program -- C1 (a statement classifier, IDENTICAL emission, 2-3 sessions) + C2 (close the text seam one statement kind per session, `local` first) = step 1d of #3694 in the parser slot; C3 (facts on binding ids) rides on the 1c ports; C4 (the void-position bind as a Kind-A gate, measure first), C5 (#75's constructor macros, with the next generation bump), C6/C7 fillers.**  Read it before touching `_lower_block_1`, the seam (`Pl::CLForm::raw`, `fallback_parser`), VarAnnotator's verdicts or `p-setf`'s arms.
- `docs/everyday-battery.md` - **THE NUMBER THE PROJECT STEERS BY** (#2099, s495): `tools/everyday-smoke.pl` + the checked-in `everyday/` corpus — "N of M ordinary programs identical to perl", with a blessed per-program baseline (`baselines/everyday-baseline.tsv`, rows leave BY EDIT) and a batch-by-batch history (`baselines/everyday-history.tsv`). Read it before quoting the number: it is a steering number, never a compatibility percentage. `--corpus DIR --baseline FILE` measures another population (Rosetta, #2104) with the same tool.

- `docs/interp-scan.md` - **`Pl::InterpScan`, the ONE scanner of variable references inside strings and patterns (#237)**: its event contract and its consumers (the regex-pattern interpolator, StringInterpolation). STANDING RULE: interpolation scanning happens in `Pl/InterpScan.pm` or not at all -- no new scanner, no per-site fix. Read it before touching anything that looks for `$x` / `@x` / `${...}` inside a quoted string or a regex.
- `docs/var-handling-review-s379.md` - **The variable-handling design review (s379, the USER's ask)**: hoisting, declarations, renaming; 51 % of Parser2.pm is variable identity; directions A (bind-once symbol table -- became `Pl/Binder.pm`), B (one scanner = InterpScan), C (one promotion engine), D (defglobal). STANDING RULES: no new scanner fix, no new suffix family, no new scope walk. Read it before touching a rename / promotion / capture pass.
- `docs/b1-operand-grammar-s416.md` - **Stacked filetests (`-f -d $x`), designed from measurement (s416)**: one predicate (`_is_print_term_start`: a `-X` operator starts a term), the print-argument leg, the `_`-chain lowering (naive nesting is a SILENT WRONG). Read with `docs/option-b-phase2-plan.md` before touching filetest operands.
- `docs/shipped-modules.md` - **How `use Module` is resolved**: the search order, the module-override architecture (`lib/` shims: which module is replaced and why, per module), and the FACTS OVERLAYS (`lib/PCL/Facts/…`: forward declarations for facts a static parse cannot see). Read it before adding or changing a shim, and for rule 9a.
- `docs/failure-cause-classes.md` - **How a failing row's CAUSE is classified** (ruled s486a; the rule lives in `tools/lib/PCLCauses.pm`): the classes, and "The grammar" of the leading cause marker on a CPAN-board row (`#N`, `NS:<id>`, `PERL-SKIP`, `PERL-FAILS`, `UNREDUCED #N`, `HARNESS #N`). Read it before editing a cause column in any baseline.
- `docs/where-the-time-goes.md` - **A plain-language tour of PCL performance (2026-07-02)**: where a run's time goes and why; the source of the "speed wins over readable output" priority decision.
- `docs/faster-codegen-suggestions.md` - **Generating faster CL, measured variants per construct**: for each hot construct the CL emitted today, an alternative shape computing the identical result, and the measured difference. The perf worklist for Target A (absolute speed).
- `docs/boxed-aggregates-design-s455.md` - **Boxed aggregates → raw element storage (s455d; ALL PHASES SHIPPED)**: array / hash elements are stored RAW by default; `PCL_RAW_ELEMS=0` is the all-boxed A/B world. Read it before touching element access, aliasing of elements, or `p-aref` / `p-gethash` and their setters.
- `docs/sweep-bug-catalog.md` - **The perl-tests sweep's failures by cause -- a session 201-231 snapshot**: its row numbers are stale; its first section says where the causes live now (the baselines' cause columns). History; read only for the cause analysis of an old row.
- `docs/xs-shim-design.md` - **The XS bridge design** (libperlshim + host vtable; built as the separate public repo `~/pclxs`): how CPAN XS / C extensions run on PCL. Parked with pclxs (the three `Pl/t/xs-0N.t` files are `skip_all`).
- `docs/MOO_MOOSE_DESIGN.md` - **Moo / Moose support plan** (older): how the frameworks' metaprogramming maps onto PCL. Current state of Moo is `docs/moo-status.md`.
- `docs/js-target-plan.md` - **A JavaScript target for the PCL IR, the plan (s453)**: parked -- do not start unasked (speed and correctness first; task #622).
- `docs/c-target-notes.md` - **A C backend for the IR -- notes, not a plan**: reviewed NO (DECIDED §s481, its §8); perl stays the ORACLE and is never a runtime dependency.

## Dependencies

- Perl 5.20+
- SBCL 2.5.2+ (the full suite has passed on 2.5.2 — the prior dev env until
  2026-05-31 — and on 2.6.0, the current one; the runtime uses some
  SBCL-internal symbols, so the supported floor is whatever the suite is
  actually validated against, not a guessed range).  **The floor is TESTED, not
  assumed (s523c, #3528):** `tools/t/sbcl-floor.t` runs the pack rows under a
  second SBCL named by `PCL_SBCL_FLOOR` (skips without; its header says how to
  install 2.5.2 beside 2.6.0), and the container leg
  `PCL_CONTAINER_BASE=ubuntu:22.04 SBCL_VERSION=2.5.2 prove tools/t/install-container.t`
  is the install-matrix shape.  2.5.2 has no `move-double-float-arg` VOP, so an
  unbounded inline `truncate` of a double (the old `p-int` double arm) does not
  compile there -- `p-int` open-codes the fixnum range and sends a wider double
  to the notinline `%p-int-wide`.
- PPI: VENDORED since s527a (MAIN-READY-48, 2026-10-25; USER decision 2026-10-24,
  DECIDED ## s527, `docs/ppi-fix-at-source-s527.md`): `vendor/PPI/lib` is PCL's
  `pcl` branch of upstream PPI master plus its fix commits (`vendor/PPI/UPSTREAM`
  names the upstream sha, the branch sha and every carried fix), loaded ONLY
  through the one loader `Pl::VendorPPI` (`use Pl::VendorPPI;`, never `use PPI;`
  -- a PPI loaded from elsewhere first is a die, not a silent mix).
  `PCL_SYSTEM_PPI=1` uses the installed PPI on purpose (the registry canary's
  "did upstream catch up" run, A/Bs).  PPI's non-core deps must be installed:
  Clone, Params::Util, Task::Weaken, Safe::Isa -- THE ONE LIST is
  `PCLVendor::ppi_deps()` (#3924), checked by `Pl/t/core-deps-01.t`.  YAML::PP is
  not a dependency since s527b: PCL's PPI loads it only to read
  `PPI_CUSTOM_FEATURE_INCLUDES` (group YAML, `46ba7e81`); upstream master's hard
  `use YAML::PP` was found by the first red CI run of MAIN-READY-48.  Never edit `vendor/PPI/lib` by
  hand: `tools/vendor-ppi <clone> pcl` refreshes it and the stamp
  (`Pl/t/vendor-ppi-01.t` checks the tree id; run the canary at every refresh);
  the branch's PUBLIC home is the fork `https://github.com/Percolisp/PPI` (branch `pcl`, created and first pushed s532, 2026-11-02, `a376a6d8`; `gh` is logged in as Percolisp on this box), mirrored by the bare repo `~/pcl-agent-scratch/ppi/PPI.git` (remote `fork`, HTTPS) -- **who pushes:** a PPI batch (an agent) fast-forwards the BARE MIRROR at MERGE-READY and nothing else (COMMON.md: an agent never pushes to GitHub); the REVIEWING SESSION pushes the fork after the batch's merge and refreshes upstream pull request #459 (`gh api -X PATCH`)
  (branches `master` = upstream, `pcl`) -- the fork exists since s532 (2026-11-02): `https://github.com/Percolisp/PPI`, branch `pcl`.
  A PPI bug is FIXED on that branch in PPI's two-commit PR shape (the failing
  rows first as TODO, then the fix; PPI's own suite green after every commit;
  perl 5.8 code), the registry row gets `fixed_in => 'pcl'`, and PCL's repair
  for it is DELETED -- a repair in `Pl/` is only for a bug whose fix is not on
  the branch yet.
- Moo (OO framework)
- Test::More (core)
- **Nothing else that is not core perl** (s440): `Data::Dump` was a non-core
  import in three `Pl/*.pm` files and 20 gate tests and broke the first CI run
  ("Can't locate Data/Dump.pm" in the installed `pl2cl`).  Diagnostics use
  core `Data::Dumper` (`Pl::PExpr::_dd`, `Pl/t/PCLDump.pm`); the two gate rows
  whose transpile SUBJECT is the CPAN module skip when it is not installed.
  Before adding a `use`, check `Module::CoreList::is_core` -- CI is a stock
  machine with exactly PPI's four deps (Clone Params::Util Task::Weaken Safe::Isa --
  `PCLVendor::ppi_deps()`), Moo, Data::Dump + Try::Tiny (fixtures); PPI itself is vendored and
  CI checks that the vendored lib loads.

