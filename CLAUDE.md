# CLAUDE.md

This file provides guidance to Claude Code when working with this repository.

## Project Overview

**PCL** (Perl to Common Lisp) is a transpiler that converts Perl code to Common Lisp. It parses Perl using PPI, builds an AST, and generates CL code.

**Status:** V2 complete - expressions, statements, constants, OO support

**Important:** This is a Perl project. Use Perl (not Python/Ruby/etc.) for any scripting, one-liners, or helper scripts. This includes searching log files, processing text, and any task where a Perl one-liner via Bash would work — don't use subagents for what Perl can do directly.

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
4. Only then probe (minimal scratch file, perl vs `./runpcl`).

And symmetrically, when work SETTLES a question: add a one-line pointer to
`docs/DECIDED.md` in the same commit; put load-bearing semantic decisions
in `docs/ir-spec.md` (a code comment may point at the spec, but must never
be the only copy); record failed attempts and what killed them in the task,
so the task says what NOT to retry.  A **"suspect X" task carries the cheap
discriminating measurement** that would confirm or kill X — or names it and
says why it was not taken (ruled s335: #184's suspicion was carried 13
sessions when "time the two phases separately" was 3 minutes and would have
retargeted the task at filing).

## Design Principles

1. **CPAN Compatibility**: Match Perl semantics exactly. The goal is to run real CPAN modules.

2. **Readable Generated Code — but speed wins**: Keep generated CL readable for Perl programmers where it is free. Use Perl-like naming (pl-sub, pl-print, $varname) so the output is recognizable. **Priority decision (2026-07-02): when readability and performance of the generated code conflict, performance wins** — never sacrifice a speed transform for prettier output. See `docs/where-the-time-goes.md` and `docs/codegen-rewrite-review.md`.

3. **Compile-Time Visibility**: Wrap sub definitions and variable declarations in `eval-when` so BEGIN blocks can call subs and access variables defined before them in source order.

4. **No Easy Write-Offs**: Don't dismiss problems as "documented limitations" without discussion. Every incompatibility is serious because it blocks CPAN code. If something doesn't work, either fix it or discuss the tradeoffs with the user before marking it as a limitation.

5. **Never Simplify Tests**: When a test fails, fix the code, not the test. This applies to both `perl-tests/` (authoritative Perl test suite) and `Pl/t/` (PCL regression tests). Do NOT weaken a `Pl/t/` test to a simpler form just because the original form fails due to an unrelated bug — keep the test semantically equivalent to what it's supposed to verify. Commenting out failing tests or replacing them with `ok(1, 'SKIP: ...')` hides bugs. If a feature is genuinely out of scope, discuss with the user first — then mark it via the **declarative skip-registry** (`cl/skip-registry.lisp`), NOT by editing the `perl-tests/*.t` file. The registry keys on the test description, cites a `docs/not-supported.md` reason, still runs the assertion, and flags itself stale if the test ever starts passing. See `docs/test-skip-registry.md`. Crashing/aborting tests are NEVER auto-skipped — they stay CRASH/PARTIAL as fix targets.

6. **Add Regression Tests for Bug Fixes**: When fixing a bug, add a test case to an existing test file that covers the fixed behavior. This prevents regressions.

   **The constraint is a file's RUN TIME, not its test count** (user, 2026-07-28, restated and refined 2026-08-01). `prove -j8` parallelises across files, so the gate's wall time ≈ the *slowest single file* — a file with many fast rows is fine, a file with a few slow ones is not. What makes a row slow: `test_transpile` runs a perl oracle **and** an SBCL transpile+run; `run_cl` spawns SBCL. So before adding, ask *how long does this file take*, and measure with `prove --timer Pl/t/<file>`; if the answer is heading past the current slowest file, start the next `transpile-test-NN.t` instead (copy an existing file's header/helpers). `transpile-test-01.t` (118 rows) and `transpile-test-07.t` (45, closed by the user in s321) are the two to leave alone; as of s321 the files are 01, 01b, 02, 03, 04, 04b, 05, 06, 07, 08, so the next new one is **`-10`** (`-09` landed in s321-era; s374 added the non-transpile-test `bareword-call-01.t`).

7. **Document Complex Semantics in `docs/`**: When solving a problem involving tricky Perl-vs-CL semantics, write a `docs/topic-name.md` file explaining the problem, the solution, and edge cases. Reference it from CLAUDE.md's "Key Files to Read" section. This prevents re-investigating the same issue in future sessions. Examples: declaration ordering, wantarray context, string escapes.

8. **wantarray/context** — three-valued implementation is done (session 163). See `docs/wantarray-impl-plan.md`. A sweep regression exists (VOID_CTX sub-body wrap in `_process_expression_statement` is too broad); fix that regression before any further wantarray work.

9. **Assume Valid Perl Input**: PCL is a transpiler for functioning Perl code, not a validator. It does not need to detect or reject invalid Perl (syntax errors, non-associative operator chains, etc.). Tests that verify rejection of invalid Perl (e.g. `eval("sub { $a <=> $b <=> $c }")` returning `undef`) are out of scope and should be commented out, not implemented.

   **9a. Fix at the right LAYER — module behavior never goes in the parser or runtime.**
   When a CPAN module misbehaves, the parser (`Pl/*.pm`) or runtime (`cl/pcl-runtime.lisp`) is the *closest* place to patch, and therefore the wrong one by default. Three layers, each owning a different kind of fact:
   - **`lib/<Module>.pm` shim** — one module's behavior: its subs, prototypes, exports, constants. PCL transpiles it like user code. This is where module-specific facts live.
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
    Test::More file that IS the upstream report, where every row FAILS on the
    current PPI (a failing row is the bug). Bump its `tests => N`. A **canary
    row** asserting the currently-broken behaviour is welcome in
    `Pl/t/misc-fixes-02.t`: when a PPI upgrade fixes the bug, that row fails,
    which is the signal to drop the workaround.

    **Scope (the file's own note):** only things PPI itself tokenizes or
    structures wrongly. A correct token stream that PCL's PExpr then interprets
    wrongly is PCL's bug, not PPI's, and belongs in a task instead.

## Quick Reference

```bash
# Run all tests (from project root) — always use -j8 for parallel execution
prove -j8 Pl/t/

# ~3.4x faster (8:18 -> 2:30): run against a saved SBCL core (runtime
# pre-compiled in). tools/prove-core rebuilds a FRESH core every run (never
# stale) then runs prove. Identical results; core is purely a speed cache.
tools/prove-core                 # == prove -j8 Pl/t/
tools/prove-core Pl/t/foo-01.t   # any prove args

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
# Copies the runtime tree (pl2cl, runpcl, Pl/, lib/, cl/, tools/lib/) in its
# repo-RELATIVE shape (the lookups depend on it), writes bin/ wrappers, builds
# <root>/pcl.core, and refuses to finish unless the INSTALLED tools transpile
# and run a program.  PCLSbcl picks the installed core up automatically; a
# checkout has none, so development runners are unaffected.
tools/install-pcl --prefix ~/.local      # default prefix is $HOME/.local
tools/install-pcl --no-core --dry-run    # what it would do; no core build
prove tools/t/install-pcl.t              # its end-to-end test (not in the gate)

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
# longer than the 90s default is registered in docs/perl-suite-timeouts.tsv
# (rel<TAB>seconds<TAB>cause; effective timeout = max(row, --timeout), and the
# allowances in effect are printed per run) — never left to TIMEOUT into "no
# rows at all", which is how a file's passing rows vanish invisibly (#176).
tools/run-perl-suite.pl --all --quick --jobs 4   # THE DEFAULT FORM (#345)
tools/run-perl-suite.pl --all --jobs 4           # full: once per session, at most
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
```

### WHAT TO RUN WHEN (RULED s401, `docs/fable-answers-s400.md` §8 — replaces "every 3rd–5th change")

The cadence is keyed on WHAT CHANGED, because every measurement is blind to
a known set of change kinds.  Rows are ADDITIVE — every row that applies
fires.  **Always, per change:** `tools/prove-core` (~4.5 min); if anything
under `Pl/` changed, `tools/corpus-diff.pl` (~2 min — READ its SILENT-DROP
line) + `tools/emission-ab.pl --ref <base> --list lib/**/*.pm` (seconds);
plus the targeted files the change names.

| what changed | full perl-tests sweep (~10 min) | companion suite (`--quick` ~15–25 min; full `--all` 30–60) | also |
|---|---|---|---|
| `Pl/**`, corpus-diff IDENTICAL, lib byte-identical, NOT a name-resolution change | **NO — it cannot move; do not run it "to be safe"** | no | — |
| `Pl/**`, corpus-diff shows diffs | YES, after every diff is explained per file + probed vs perl | the dirs whose files carry the shape (`grep -a`); `--quick` once if broad | gen bump + `tools/rebuild-pack` (staleness gate enforces it) |
| `Pl/**` name-resolution / scoping / rename / capture / promotion (Parser2 `_rename_*`, `_promote_*`, VarAnnotator, GlobalPartition, eval capture, span passes) | **YES — the sweep IS the gate** (#296) | `--quick` once | gate-SET scan over both populations when a checker / refusal / decline WIDENS (s372) |
| `cl/**` runtime | YES (invisible to corpus-diff) | the dirs the change touches (op/, io/, re/ …) | rule-12 read of the touched dispatch |
| `lib/**` shim | YES | the files that `use` the module (`grep -a`) | — |
| harness: `perl-tests/t/test.pl`, `cl/pcl-test.lisp`, `cl/skip-registry.lisp` | YES | **`--all --quick`** (both populations reach it) | baselines edited ROW BY ROW |
| runners: `sweep-perl-tests.pl`, `tools/run-perl-suite.pl`, `tools/lib/PCLSbcl.pm`, `tools/pclperl-for-tests`, `Pl/t/PCLCore.pm` | the runner that changed, once; verdicts compared file-by-file | same | `PCL_SHOW_SBCL=1` before/after diff |
| `docs/**`, `tools/t/**`, memory | nothing beyond the gate | no | — |

Companion: **`--quick` is the default form** (#345: skips the #326 hang
set, caps registered allowances at 120 s, LISTS every skipped/capped file as
NOT-RUN); the full `--all` at most once per session and only when a row
says so, before a snapshot re-bless, or for a Fable review.  Fable review
sessions: cold gate + full sweep + probes, plus a `--quick` companion when
the batch touched name resolution or the harness.  The three s399 tools:
`corpus-diff`'s drop counter is per-change (free); `tools/gate-set-scan.pl`
is on-demand, MANDATORY only for the checker-widening row;
`tools/drop-census.pl` is a runner column (#343) — not a separate step.

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
**`tools/rebuild-pack`**, then `perl sweep-perl-tests.pl --jobs 1
--timeout 380 perl-tests/pack.t` and `tools/sweep-diff.pl` (expect 0 new;
s316b: 5635 pass / 90 fail).  It was found at gen v2-30 against a v2-71
compiler — 40 generations of drift, which made every pack.t run a test of
the *old* emitter (and again at s396: v2-136 vs v2-147, eleven generations).
`cl/pcl-mro.lisp` comes from `lib/mro.pm` the same way, and so does
**`cl/pcl-warnings.lisp` from `lib/warnings.pm`** — there are THREE.
**The drift is now a GATE**: `Pl/t/artifact-staleness-01.t` compares each
artifact's line-1 `gen=` stamp against `*pcl-cache-generation*`, so bumping
the generation without regenerating fails a row the same session (s399,
task #331).  The artifacts are discovered by that stamp, not listed.

## Architecture

### Core Pipeline

```
Perl Source → PPI → PExpr (AST) → ExprToCL → Common Lisp
                      ↓
                 Environment (constants, packages, prototypes)
```

### Key Modules

| Module | Purpose |
|--------|---------|
| `Pl/Parser.pm` | Statement-level parser. Entry: `parse()` |
| `Pl/PExpr.pm` | Expression parser. Entry: `parse_expr_to_tree()` |
| `Pl/ExprToCL.pm` | Code generator. Entry: `generate()` |
| `Pl/Environment.pm` | Tracks constants, prototypes, package stack |
| `Pl/OpcodeTree.pm` | AST node storage (ID-based) |

### Parser Components (Pl/PExpr/)

| File | Purpose |
|------|---------|
| `Config.pm` | Operator precedence (1-92), function param specs |
| `StringInterpolation.pm` | `"$var"` string parsing |
| `TokenUtils.pm` | Token classification helpers |

## What's Implemented

### Expressions
- All operators with correct precedence
- Ternary `? :`
- Method calls `$obj->method()`
- Array/hash access `$a[0]`, `$h{key}`
- References `\$x`, `$$ref`
- Regex `s///`, `tr///`

### Statements
- `if`/`elsif`/`else`/`unless`
- `while`/`until`
- `for`/`foreach` (both C-style and list)
- `sub` with signatures and defaults
- `package` with block scoping
- `use constant`
- `try`/`catch`/`finally` (perl 5.34 `use feature 'try'`; task #340, s405) —
  `p-try`, semantics in `docs/ir-spec.md` §6.3.  **`use experimental 'try'`
  does NOT work** (two independent reasons, task #360).

### OO Support
- `bless`, `ref`
- Method calls: `$obj->method()`, `Class->new()`
- Inheritance via `@ISA` with C3 MRO (CLOS-based)
- `SUPER::method()` calls
- Package stack (3+ levels)
- Multiple inheritance / diamond inheritance

## Code Generation Patterns

```perl
# Assignment
my $x = 1;           → (pl-setf $x 1)

# Operators
$a + $b              → (pl-+ $a $b)
$x++                 → (pl-++-post $x)

# Function calls
length($s)           → (pl-length $s)
print "hi"           → (pl-print "hi")
print STDERR "err"   → (pl-print :fh STDERR "err")

# Control flow
if ($x) { }          → (pl-if $x (progn ...))
while ($x) { }       → (pl-while $x ...)
for (...) { }        → (pl-for (init) (cond) (incr) ...)
foreach $x (@a) { }  → (pl-foreach ($x @a) ...)

# Subroutines (wrapped in eval-when via pl-sub for BEGIN visibility)
sub foo($x) { }      → (pl-sub pl-foo ($x) ...)

# Constants
use constant PI => 3.14  → (defconstant +PI+ 3.14)
PI                       → +PI+

# OO
bless {}, "Class"    → (pl-bless (pl-hash) "Class")
$obj->method()       → (pl-method-call $obj 'method)
```

## Config.pm Parameter Specs

```perl
# In known_no_of_params hash:
func => 2           # Exactly 2 params
func => [1, 2]      # 1 or 2 params
func => -1          # List (variable)
func => -2          # Defaults to $_ if no args
func => -3          # Defaults to @_ if no args
func => -12         # 1 param before list
```

## Test Status

- **160 test files, 5700 rows** (s430) with a built pclxs sibling (s409, measured
  COLD; the 13 pclxs xs rows currently FAIL there — pclxs is under separate
  work, user s394/s395: ignore XS rows); **5600 without** (arithmetic: minus
  the 14 xs rows).  **RULED s409: compare a gate count against a measurement
  of the SAME tree** — the xs files PRODUCE 0–14 rows depending on where
  pclxs\x27s current state aborts them, so a written-down total drifts on its
  own; when only a number is at hand, subtract the xs rows produced in each
  run (s408 wrote 5439 with 11 xs rows = the same 5428).  The gate count is deterministic *per environment*, but it
  is conditional: `Pl/t/xs-01/02/03.t` (6+4+4 = **exactly 14** rows) resolve
  pclxs as `$FindBin::Bin/../../../pclxs` — **a sibling of the CHECKOUT** — and
  `plan skip_all` (contributing 0) when it is missing or `libpclxs.so` is not
  built.
  **GOTCHA that cost s319 an investigation: comparing against a `git worktree`
  silently subtracts those 14**, because the worktree lives elsewhere and its
  sibling path does not exist.  A worktree is still the right way to compare
  against HEAD (never a stash-copy) — just set `PCLXS_DIR=~/pclxs`, or expect
  and subtract the 14.  What must hold either way is `Result: PASS` and the
  file count.
- **XS conformance: 398 pass, 0 fail — fully green** against pclxs's corpus
  with real perl as oracle (`tools/pcl-conform`, re-measured s339 after the
  rule-12 pass over `cl/pcl-xs.lisp`; XS OO/magic
  works — Digest::MD5's own md5-aaa.t is 256/256 under PCL).
  Not in the Pl/t gate — minutes, not seconds.
- **All passing**
- **Runtime: ~2:30 with `tools/prove-core`** (~5+ min with plain `prove -j8`;
  each test file spawns a new SBCL process)
- **The full sweep RUNS ITS OWN GATE (s330, #204)**: `perl sweep-perl-tests.pl
  --jobs 8` with no file arguments ends by running `tools/sweep-diff.pl diff
  docs/fail-baseline.tsv .faillog` and **exits with that verdict** (`--no-gate`
  opts out; a sweep of named files stays informational).  The diff now has a
  **fourth bucket, LOST** — baseline PASSING rows the run did not produce,
  compared against `docs/pass-baseline.tsv` (`sweep-diff.pl save-status .faillog
  docs/pass-baseline.tsv` re-blesses it).  The first three buckets read failing
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
  `docs/parse-error-drop-census-s399.tsv` — **the census IS the baseline**, a
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
  edited into `docs/pass-baseline.tsv` by hand with its cause (s404l's
  blank-line fix in `tools/pclperl-for-tests`, attributed by bisection in a
  worktree).  The s399 measurement it sits on: **704 blessed fails**,
  **64 files fully passing**, TOTAL passing 18516 across 108 files.  The
  s399 move is the only one that is not a regression to chase: task #323 made
  `warning_is`/`warning_like`/`warnings_like` in `perl-tests/t/test.pl` stop
  manufacturing a pass, so 24 rows (assignwarn.t 20, hashassign.t 4) became
  honest failures with ONE cause — PCL emits no warnings-gated diagnostic
  (`not-supported.md` "Warnings-gated diagnostics are absent", owner #221).
  Both baselines were edited ROW BY ROW, never re-blessed, and
  `docs/pass-baseline.tsv` carries a header note saying so.
  Historical baseline text follows.
- Full `perl-tests/` sweep: **679 blessed fails** in `docs/fail-baseline.tsv`,
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
  docs/fail-baseline.tsv .faillog` = **0 new / 0 fixed / 0 LOST**, plus 2 rows
  the tool
  itself flags UNSTABLE — new fails ABOVE the abort point of postfixderef.t and
  ref.t, both already PARTIAL, so they are crash-file noise, not regressions.
  **Both baselines were re-hygiened in s341 (#223)**: the two now-passing
  scalar.t rows edited out of the fail baseline, and `docs/pass-baseline.tsv`
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
  blessed baseline" was FALSE** — `docs/fail-baseline.tsv` has always carried
  its 58 pack.t rows.  The claim came from `grep`ping that file, which
  contains NUL bytes: grep then treats it as binary and prints nothing.  Use
  `grep -a`, or perl, on any `.tsv` under `.faillog/` or `docs/`.
  See `docs/sweep-bug-catalog.md`
- v2 pipeline census: 111 files v2-native / 0 gated to v1 — E1 complete
  (`perl tools/v2-census.pl` for the live numbers)

## Common Pitfalls

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

## Key Files to Read

When resuming work:
0. `docs/DECIDED.md` - **One-grep index of settled questions** (grep it before probing or designing anything — see the lookup order at the top of this file)
1. `docs/session-log.md` - Session history (compact, newest first)
2. `docs/fable-answers-s316v.md` - Current design/policy rulings (answers to `opus5-review-requests-s316v.md`)
2b000. **s417 RULED + Track B2 (#343) SHIPPED (s418, Fable, 2026-08-20;
`docs/fable-answers-s417.md` + `docs/b2-ceiling-fix-s418.md`).**  s417 APPROVED
(gate COLD 153/5590; sweep re-run clean; 14 probes).  **B2 = the stale ceiling,
FIXED by recomputing at use**: `$last_low_prio_op` DELETED; the paren-less
list-operator argument ceiling is derived from the CURRENT `@$e` by one
rightward scan to the first same-level `and`/`or`/`xor` (the probe's own
`$actual`, validated over 658 files; the region prohibition does not apply —
no new rule, the boundary's meaning is unchanged; the `PCL_B2_TRACE` probe is
deleted, not a gate).  Emission A/B over the four populations: diffs ONLY
bless.t + split.t (both populations) + **Text::CSV_PP.pm:1566 — an ELEVENTH
uncounted silent-wrong in the cpan population** (`… && grep m/\D/ => keys %{…}
and $hdrs ||= "auto"`, grep swallowed the `and`; probed both branches vs
perl); reg_fold.t byte-identical as predicted.  Gate-SET scan 638×2: exactly
the 4 drop→OK moves.  Sweep TOTAL 18369 → **18363** — bless.t's un-dropped
assertion runs and FAILS HONESTLY (`bless` misses get-magic on a tied operand,
**#408**), split.t's un-dropped `or skip` takes the file's own skip branch
(PCL's `\s` under /u misses NBSP, **#407**): 3 baseline fails fixed, 6
ACCIDENTAL passes (rows running on the dropped statement's undef `$sp`) now
skip; both baselines edited ROW BY ROW.  Census 46/139 → **42/135**; gate
**154/5594**; generation **v2-161**.  Guard `Pl/t/listop-ceiling-01.t`.
**runpcl now forwards `^PCL:` lines on a successful transpile** — it used to
discard the #339 drop announcement.  #259/#335 probed post-fix: NO change, as
designed.
2b00000000000000. **s438b + s438c (Opus, 2026-08-23): Q4 — the two named-unary OPERAND SITES become ONE (#453), and an imported `()`-prototype sub is a TERM (#365).**  **#453**: perl decides a named unary from the PROTOTYPE alone, and `_proto_parse_spec` was already the one reading of that shape, so `is_named_unary` now answers for a DECLARED sub whose spec is 1 / [0,1] (`(1000 4 24 27 30 46 100 115 1000` `(;1000 4 24 27 30 46 100 115 1000` `(*)` `(_)` `(\@)`) — which routes it to the site that runs `_extend_high_prec` and, because the same predicate is the strictly-single site's guard, out of the site that stops at the first term: `f "a" . "b"` was f(a)b, `f $x + 1` was 1, `(*)`'s `g + 1, "\n"` was g(1,"\n").  `known_no_of_params` is NOT a second source (its 1 covers shift/close/fileno, which must keep the strictly-single site and its bareword-filehandle branch).  **#365 — NOT where the task pointed, and that is the finding**: the classifier IS asked (23 times for `pi`) and answers `no` because **the prototype never crossed the `use`** — `_merge_module_prototypes` imported only block args, parameter SLOTS and names the export scan listed, and that scan reads literal `qw()` while Math::Complex builds `` from a variable (`my  = qw(pi …); our  = (qw(…), )`).  An empty prototype is a PARSE fact and now crosses a `use` on its own shape; `is_proto` does NOT identify it (`sub pi ()` arrives as is_proto 0 — a first attempt keyed on it changed nothing).  **ONE predicate `Pl::Environment::proto_is_zero_arg`** — the record test was in `PExpr::_is_zero_arg_func` and nowhere in the merge, which is how they disagreed (rule 11).  **Both changes are emission-IDENTICAL over the four populations** (corpus-diff 111 + emission-ab 951 files SAME / 0 DIFF / 0 RCDIFF, twice — RCDIFF 0 is also the die-scan), so the s371 rule applies and the GUARDS are the bar: `Pl/t/user-unary-01.t` (12 rows, 5 negatives) + `Pl/t/imported-term-01.t` (7 rows; its fixture builds `@EXPORT` from a VARIABLE so it tests the mechanism, not one module's spelling), both inverse-guarded on a `fe46c7b` worktree.  Gate **165/5760**; sweep TOTAL **18312 (+0)** GATE clean; companion **528 files, ZERO real movers** (io/pvbm.t alone 23/5 — the SIXTH time).  **#484 filed**: the #351 `WORD /` repair CAN reach a classifier (`_word_is_term`) but that reads only THIS document's terms and the prototype pre-merge runs AFTER the repairs, so `pi / 2 + pi / 4` is still repaired to a match and dropped.  **NEXT = Q5 (P4: #454, #435, #455).**

2b0000000000000. **s438 (Opus, 2026-08-23): the two CENSUS INSTRUMENTS — the drop census gains a SIXTH population (#473) and a SEVENTH is measured for the first time (#472); the companion scan stops filtering silently (s434 ask 1).  NO product change; six findings filed.**  **#473**: `cpan-tests/modules/**/t/**/*.t` (289 files, PROGRAM mode) is a census population — **42 files / 83 drops**, blessed with causes (`docs/parse-error-drop-census-s399.tsv` 39/102 -> **81 files / 185 drops**; the 36 non-board pre-existing rows byte-identical).  The s436 A/B's "43/92" reconciles exactly: these 42 + `examples/tools.t` (9 drops), an EXAMPLE, named in the header with its count beside `t/japh/`; an `f702da3` worktree gives the same 42/83, so nothing moved.  Measured, not assumed: the dist `.t` files transpile WITHOUT the dist's own `lib/`+`t/lib` (identical rows), and the tool now strips the repo root out of message TEXT (one message quotes its own file, so a row depended on how ROOT was spelled).  **79 of the 83 are ONE mechanism — #478: `_extract_module_prototypes` skips every `Test2::`/`Test::` module BY NAME, so a `(&)` block-form call is not parsed** (probe: two modules identical but for the package name); the no-semicolon spelling is SILENTLY mis-parsed instead (`blk { 42 }` -> `(pl-blk 42)`, the block's value where perl passes a code ref).  Residue #480 (`$_.2` — PPI lexes `.2` as a float), #481 (a fat comma autoquotes a METHOD NAME), #482 (`$obj->state` dies in the compiler).  **#472**: `PCL_DROP_LOG` is one arm in the ONE announcer (`FILE/LINE/TEXT/REASON`, ungated by `PCL_DROP_ANNOUNCE`, never on stderr — the child's stderr IS the row's observed output); the sweep sets it around the RUN only, carries a `child-drops` column in `_status.tsv` and prints a per-file count AND the distinct SITES; REPORTED, NOT GATED.  **First measurement: 241 drops in 98 files, TEN sites** — two of them OURS, `perl-tests/t/test.pl:179-180` reached by 98 of 98 files (**#479**, and that line is wrong in REAL PERL too: `my $f` in a ternary's condition is not in scope in its branches), six one-off child programs, and **one real gap in a real module: core `Devel/Peek.pm:59`, where PPI lexes `<<index` as a HEREDOC (#483)**.  **s434 ask 1**: the companion `--all` scan is **528 files, not 523** — a `BEGIN`-`@INC` file was dropped silently, which is how five rows came to be unrefreshable; all five measured, all five produce a verdict, and `%NEED_HARNESS_NOT_RUN` (empty by measurement) feeds the existing NOT-RUN path.  ONE snapshot row edited: `op/require_errors.t` C_notok 70 -> 68, bisected to **the flip** (3/70 at 4356e77 and 9138404, 3/68 at f702da3 and HEAD), two rows that were failing and are now missing with their form.  Bar: gate **163/5741** (the +2 over s437's 5739 are s437's OWN review-fix rows), corpus-diff IDENTICAL over 111 (no generation bump), sweep TOTAL **18312 (+0)** GATE clean, companion 528 files with both SNAPSHOT holes at zero.  Asks: `docs/opus5-review-requests-s438.md`.

2b000000000000. **s437 (Fable, 2026-08-23): s434 + s435 + s436 REVIEWED + APPROVED — Q1 (instruments) + Q2 (THE FLIP) + Q3 (THE PHASE MODEL) are DONE; NEXT = the two census instruments (#473 + #472 + s434 ask 1), then Q4 (#453 + #365)** (rulings `docs/fable-answers-s437.md`).  Independently re-measured COLD: gate 163/5739 (only the 13 pclxs xs rows), sweep TOTAL 18312 (+0) GATE clean drops 5 = census, companion `--all --quick` 523 files with ZERO real movers (io/pvbm.t read 20/8 in the parallel AND the #366 serial pass and 23/5 alone — the FOURTH time; op/utf8cache.t DIFF→TIMEOUT at C_ok 2 is the recovery load reaching its "quadratic pos" loop, which PCL runs QUADRATICALLY — **#477**, a Target-A runtime bug: 100k chars 4 s, 200k 15.7 s, perl 1M in 0.09 s), lib A/B 21 permutation + 1 in-package, 42 probes vs perl.  **ONE REVIEW FIX (ask 6 was a real bug): `package NAME VERSION` now sets `$VERSION` at the HEAD of its section's compile phase** — a BEGIN in the same section read undef (perl 1.5) on HEAD and on the base alike; one line moves, corpus identical over 111, two `decl-ordering-02.t` rows, gen **v2-181**.  Filed from probes, both PRE-EXISTING: **#475** (a FILE-level `our` alias is not requalified across a TOP-level `package` statement — `our $t="m"; package A; $t.="A"` prints m; the in-block spelling works), **#476** (`-NAME` with a DECLARED sub is a negated CALL in perl, the string in PCL).  Rulings: prove-core MemoryMax scope IN (tools filler, measure the gate's peak first); #473 YES (cpan `.t` population, program mode; japh = header sentence); #472 = `PCL_DROP_LOG` side channel in the ONE announcer + the sweep's `child-drops` line, measure first; a promoted fix may ride in the promoting session under the s366 filler rule; #474 FOLD; the stub block stays; s434's six-file `--bless-rows` RATIFIED after READING the rows (standing: a bless after an instrument change says per file that the new rows fall under the registered reason); never-refreshed rows → NOT-RUN rows / joined scan by measurement.  Q7 re-ordered: the PROMOTED #463 item 2 (`++${"23::foo"}`, 18 rows behind one drop) first.
2b00000000000. **s433 (Fable, 2026-08-22): s431 + s432 REVIEWED + APPROVED — THE LIVE QUEUE IS `docs/plan-post-s433.md`** (rulings `docs/fable-answers-s433.md`).  Re-verified cold gate 160/5705 + sweep TOTAL 18366 GATE clean.  **The announce→DIE flip has a SHAPE: a perl-shaped, trappable RUN-TIME die at the drop site — ONE shape for every drop in every mode, no classifier** (the `;; PARSE ERROR` comment stays for the census; the stderr announcement, #363, `--module` silence and Track A stay); its unit is the statement, so the s431 file prices do not apply and the module-mode increment is DISSOLVED; it waits only for the two instruments (#467 recovery in both runners + re-bless; #462 module census).  **#456 half (b) PROMOTED and RULED as the PHASE model across sections** (#469: a later section's BEGIN runs after an earlier section's run-time code — probed; "hoist the def alone" is UNSAFE, symbol-macro cells).  Filed #468 (never-declared plain call: no AUTOLOAD, raw CL error), #469, #470 (identity-promoted file lexical ≡ `$main::y`).  Queue: Q1 #467+#462 → Q2 the flip → Q3 #456(b) → Q4 #453+#365 → Q5 → Q6 → Q7 module fillers (#457 first) + #468/#470; release gate independent.

2b0000000000. **s430 (Fable, 2026-08-22): B3 COMPLETE (#153 CLOSED; B3.3 = #374(b), the keyword-named lexical sub renamed POSITION-AWARE) — THE LIVE QUEUE IS `docs/plan-post-s430.md`** (P1 flip re-census → P2 #456 → P3 #453+#365 → P4 #454/#435/#455 → P5 #451/#452/#449/#450 → push week → CI → v0.1 tag; recipes in its §3; Fable: the flip design from P1's table).  Record: DECIDED §s430, `docs/b3-operand-collapse-s428.md` §B3.3.
2b000000000. **s425 (Fable, 2026-08-22): the three USER decisions CLOSED and round 2
of parallel Opus agents — E + B MERGED, F IN FLIGHT at session end.**  Decisions
(`docs/plan-post-s420.md` §4, DECIDED s425): **indirect object = MAYBE LATER**
(USER; `not-supported.md` entry after measuring that `new Foo`/`new Foo(LIST)`
already WORK and the scalar-invocant spelling is 2 loud census drops — not
refused, 288 rows); **the v0.1 tag DECOUPLES from the flip** (tag after the
first green CI run + O1); **next Fable = B3**.  **E = s426 (#388 consumer 3 +
#420 + #422.1) MERGED `c1983e1`**: StringInterpolation is an InterpScan
`scan_one` consumer (1216 → 664 lines) and a slice through a reference
(`@$r[0,1]`, silent-wrong in CODE) is fixed by one helper; ir-spec §3.2b.
**B = s423 (#418 widened) MERGED `f02fe2a`**: one helper `Pl::CLForm::cl_sym`/
`cl_pkg` (identity on ASCII) + the runtime `%pcl-invert-case` guard; uni/+mro/
legs 15 gains; **Fable fixes on top `dbef93c`** (`$#NAME` ArrayIndex spelling —
`$#Foo::Bar::x` was an ASCII READ ERROR; interpolated hash-key autoquote
class).  Gate 156/5655, sweep TOTAL 18365 +0 (B's tree; the two fixes verified
by guard rows + probes only).  Generation **v2-171** (B's; to be renumbered
FRESH after F).  Filed: #434 (mixed Word leaf), #435 (fragment re-parses skip
the #410 repair — `"$Ｘ[$ｉ]"` silently reads element 0), #443/#444 (E),
#431–#433 (B).  Rulings: `docs/fable-answers-s423-s426-s427.md`; consumer 2
scheduled as O4.  **F = s427 (O3 fillers) was STILL RUNNING — its worktree
`agent-a1ef8cda8527aca2b` is the first thing the next session reads** (memory
`project_s421_opus_agents_inflight`).  Launcher rule: per-agent generation
strings get a GAP above main (a rebase renumber landed on a sibling's key).
2b00000000. **s421 (Fable, 2026-08-22): s420 REVIEWED + APPROVED; THE LIVE
QUEUE IS `docs/plan-post-s420.md`** (review record `docs/fable-answers-s420.md`).
Independently re-measured: gate COLD 155/5614 (the 13 xs rows), sweep TOTAL
18364 (+0) GATE clean, census 33/106 with an EMPTY row diff, eight probe
files vs perl — every shape s420 names identical; #390 CLOSED (= #414).  Five
PRE-EXISTING findings filed from the probes: **#420** (`"$$r[1]"` /
`"$$h{k}"` / `"${$r}[i]"` / `"@$r[…]"` / `"@{$r}[i]"` inside a dq string leave
the subscript LITERAL — silent wrong; routed through #388 consumer 3),
**#421** (prototype table keyed by BARE name collides across packages),
**#418 WIDENED** (NFKC + `:invert` makes `%Ｘ` ≡ `%X` — pipe-quote ANY
non-ASCII symbol), **#422** (`"@{^CAPTURE}"` interpolation drops; `$Ｘ {a}`;
PPI's LEXER fails a file on `for my $Ｉ (…)` — §23 addendum), and **#423**: the
op/gv.t 50/47 → 49/48 mover s420 spliced as "pre-existing" is ATTRIBUTED to
**s419d** by a three-way probe (the box path prints every typeglob-holding box
as `GLOB(0x…)` — glob VALUE ≡ glob REF in the box model; s419d moved s///+tr
onto that path).  Rulings: **"pre-existing" is WHEN, not WHY** — splice a
companion mover only with its cause; **a `cl/` coercion/stringification change
runs the op/ companion leg**.  Queue: O1 = #419 + #418 (two emission rules,
the biggest prizes) → O2 = #423 (measure the glob representation first) +
#388-consumer-3 with #420/#422.1 as acceptance → O3 fillers (#415, #421,
#422.2, #399 count) → push week (USER, 2026-08-24) → first CI run → tag.
**Next Fable session = B3 (`_reduce_term`, #153)** — the flip's long pole.  **Then three Opus agents ran in parallel (s422/#419 + s424/#423 MERGED, gate 156/5639; s423/#418 IN FLIGHT at session end — `docs/fable-answers-s422-s424.md` §B).**
USER decisions open: tag DECOUPLES from the flip (recommended yes), B3 next
(yes), indirect object #399/#381.
2b0000000. **s420 (Opus, 2026-08-22): four of the flip's unblock-list tasks
SHIPPED — the drop census is 33 files / 106 drops** (was 42/135; edited row by
row with causes, `docs/parse-error-drop-census-s399.tsv`).  **#414**: a CLONED
PPI element does not keep its tokens alive — PPI's DESTROY empties every
descendant, so a clone that is a NODE (the inner `[0]` of `"$x[$i[0]]"`) went
HOLLOW between parse and emit and the leaf emitter died on `content` = undef
(that is what the `…:?` meant).  One `_anchor` helper at all six
StringInterpolation fragment sites; every nested interpolated subscript works
now, and perl-tests/postfixderef.t's `.` overload handler stopped being
dropped (**sweep TOTAL 18363 → 18364**, test 107 left `fail-baseline` by
EDIT).  **#413**: the prototype table and `declared_subs` are keyed by the
BARE name — the convention `_bareword_callable_here` already relied on — so a
package-QUALIFIED declaration (`sub main::end(&)` from inside `package End`)
was invisible; normalized at the `Pl::Environment` seam in BOTH directions
(**perl applies a prototype on a QUALIFIED CALL too** — probed, the opposite
of what the task guessed).  **#412**: `@{^CAPTURE}` implemented beside
`@-`/`@+`, `%{^CAPTURE}`/`%{^CAPTURE_ALL}` mapped onto `%+`/`%-` (perl
SYNONYMS), `$#{^CAPTURE}` collapsed by `_block_caret_name` (which also fixed
the spaced `$ {^XY}`); a caret variable of ANY sigil must be pipe-quoted —
bare all-caps reads DOWN-cased under `:invert`.  **#410**: PPI splits `$Ｘ`
into Cast + Word (`ppi-upstream-bugs.md` §23, `Unknown.pm`'s `$` branch tests
`/[a-z_]/i` where its siblings test `/[\w:]/`), and merging the tokens is not
enough — the LEXER had already built the following `{…}` as a BLOCK, so
`_merge_unicode_symbols` re-classes the postfix chain (uni/gv.t 37/38 →
41/40).  Also **#417** (`$#+` is the pattern's group count, `$#-` stops at the
last participant — PCL truncated both) and **#416** (`s///` no-match returns
`""`, not `0`).  Gate **155/5614**; generation **v2-163**; gate-SET scan over
both populations = exactly 8 files, all drop→OK; every companion mover
verified PRE-EXISTING on a `47e0750` worktree and spliced into
`docs/perl-suite-run.tsv`.  Filed: **#418** (a non-ASCII PACKAGE name
mismatches at the CL reader — SBCL NFKC-normalizes and `:invert` down-cases;
pipe-quoting defeats both) and **#419** (one `>0x10FFFF` literal makes the
WHOLE emitted file unreadable — `t/re/pat.t` is 1263 perl rows behind it).
2b00000. **s419 (Fable, 2026-08-21): the flip re-census DONE — the announce→DIE
flip is BLOCKED by its own bar; release phases 3+5 executed.**
`docs/drop-census-s419-flip-gate.md`: census re-verified ROW FOR ROW (42/135),
every drop classified into 14 families with owners; 82 are compiler gaps in
productive files (ref.t 191 rows, closure.t 272, …), so the flip waits on the
unblock list (#410–#415 new, #399, #374(b)/#365, #259, #153-B3) — re-census
after they land, flip with the s373 three-leg bar.  The one unblocked
increment (module-mode DIE, the last fully-silent #138 member) waits for a
cpan-board drop count (flip-gate §5).  **#279 DONE** (root junk deleted,
24 planning .mds → `docs/history/`, CODEGEN/MOO_MOOSE designs → `docs/`,
tracked `.suitelog-*` gone, hook path relative); **#280 DONE** (README
refreshed with measured numbers, `docs/STATUS.md` = the user-facing
compatibility page, `CHANGELOG.md`; NO v0.1 tag — precondition is #282 green
+ phase 4); **#283 authored** (`.github/workflows/ci.yml`, installer-based —
activates at the deferred push, week of 2026-08-24); **#282**: no container
runtime on this machine — the sanitized-fresh-HOME rehearsal ran instead, and
the first green CI run is the container half.  **Phase 4 OPENED in the same
session: #119 + #402 SHIPPED** (the ruled pair, s416 §7.4 — the two
stringify-instead-of-overload shortcuts: s///-and-tr/// match sources now run
the `""` overload + tie FETCH; `p-string-concat` folds through the
overload-aware `p-.` when a piece carries a `.` handler, RUNTIME-ONLY, perl's
multiconcat probed byte-identical incl. the single-piece-stringifies rule).
Gate **155/5600**; sweep GATE clean TOTAL 18363 (+0); opbasic/concat.t
248/6 → 249/5 = the ONLY change-attributed mover ([perl #124160]).  Guard
`Pl/t/overload-01.t`; filed #416 (s/// no-match `0` vs perl `""`).
**Queue: the M–N remainder (push + #282-in-CI, user-scheduled) → phase 4
continues (the bug hunt) → tag decision (see the schedule finding) → the
flip unblock list.**
2b0000. **#401 eval half SHIPPED (s418c, Fable, 2026-08-21) — `state` in a
named sub reaches STRING EVAL; #401 CLOSED (both halves).**  The cell
`$x__state__N` is a defvar, never let-bound, so the eval alist could not
carry it and the route refused any sub containing a string eval.  Fix (the
task's sketch, on the M5 block-cell precedent): original→cell registered when
the DECL STATEMENT lowers (`_eval_state_captures` — visibility starts at
perl's masking point), scoped by `_lower_sub`'s save/restore WITHOUT the
`_let_bound_vars` wipe (nested subs inherit — op/sub.t's very shape; defvars
cannot be the wipe's unbound-at-call-time hazard); alist order = let-bound
(my-shadow wins) → state → span.  Waiver ONLY for a scalar decl at the top
level of the sub's own block; inner-block decls + containers keep the
refusal.  **The mandatory cache leg needed NO code** — #296-B1's key already
carries capture NAMES, the alist VALUE resolves at runtime; probed: two subs,
same eval text, `ABA`.  t/op/sub.t TRANSPILE-FAIL → **DIFF 53/12** (one row
better than s410's 52/13; spliced).  Eight probes identical to perl; emission
A/B: op/sub.t the ONLY mover in 1139 files; gate-SET scan: only its verdict.
Gate **155/5598** (guard `Pl/t/state-eval-01.t`); generation **v2-162**.
**Two Pl/t guards were STALE against this session's own fixes and repaired
in the same commit** (the s416 rule, missed once and now paid twice over):
eval-01.t's #363 drop-in-eval rows used the very statement the B2 fix
un-dropped (reproducer swapped to #335's shape, with a swap-again note),
and parser2-02.t t62 asserted the refusal #401 removed (now asserts the
inner-block residue still gates).  **Queue now: re-census → announce→DIE
flip → M–N release (#279 → #280 → #282 → #283).**
2b00. **Track B1 (#372) SHIPPED s417 — stacked filetests.**  `-f -d $x` is ONE
term and lowers to perl's `_`-chain (`-x $f && -w _ && -f _`), never a nest
(nesting is SILENT WRONG the other way: `-e -f "/etc/passwd"` is 1 in perl,
undef nested).  It needed NO `_term_extent`/`_reduce_term`/`$end_pars` change —
the operator loop already reduced prefix runs rightmost-first; the bug was one
line in the SHARED oracle `_is_print_term_start`, which answered 0 for a `-X`
Operator, so the `$_`-default pre-pass spliced `$_` into the MIDDLE of a run.
Exposed a PPI bug (§22): after a SCALAR/BLOCK filehandle PPI splits `-e` into
`-` + Word, so `print $fh -e $f` was a subtraction of a call to sub `e` —
repaired on ADJACENCY (`next_sibling`), which is perl's own discriminator
(`print $fh - e $f` IS `-(e($f))`).  Census 27 drops → 1; gate **153/5590**;
generation **v2-160**.  Guard `Pl/t/filetest-stack-01.t`.  Residue filed:
**#403** (a filetest's FALSE is a DEFINED `""` when the stat succeeded — the
whole `p--*` family conflates it with undef; do NOT assert definedness on a
filetest until it closes), **#404** (perl stacks through PARENS), **#405**
(`print $fh -3` writes to `$fh`).  **Queue now: #343 (B2 — mechanism measured s417, `docs/b2-stale-operand-ceiling-s417.md`, FIX NEEDS A FABLE DESIGN) → #401-eval filler → re-census → announce→DIE flip → M–N release.**  (`class NAME ;` DONE s417.)
2b0. **`docs/fable-answers-s415.md` + `docs/b1-operand-grammar-s416.md` — the s414+s415 batches RULED (s416, Fable, 2026-08-20) and THE B1 DESIGN.**  Both sessions APPROVED (gate/sweep/companion independently re-run; TOTAL 18369; 14 probes identical to perl); ONE review fix — parser2-02.t t60 was a stale twin guard of the refusal s415e removed, failing the gate since `829bcf5` (standing rule: grep Pl/t for a refusal's message text in the commit that removes it; gate rows now 5566).  Rulings: Track A deviations RATIFIED + drop-harvest-first standing; "refused is explained" IS the census intent; #401-eval → session L (cache-key leg mandatory); #402+#119 two tasks / one session / release phase 4; `class NAME ;` refusal AUTHORIZED, strict key only.  **B1 (#372) re-designed from measurement and UNBLOCKED — the b1 doc supersedes option-b-phase2-plan §2's sketch**: one predicate (`_is_print_term_start`: a `-X` Operator starts a term) + the print-argument leg + the `_`-chain desugaring (naive nesting is SILENT WRONG, probed); NO `_term_extent`/`$end_pars` changes.  **Queue: #372 → #343 → `class NAME ;` + #401-eval fillers → re-census → announce→DIE flip → M–N release.**
2b. **`docs/opus5-handoff-s413.md` — THE HANDOFF (s413, Fable, 2026-08-18; Fable time short): read FIRST.**  §1 where the project is; §2 Opus's first job = task #395: verify (gate + bench A/B + full sweep on the tip) and `--ff-only` merge branch `s413-lisp-dedup` (the six runtime dedup families + fix #394, Fable-verified); §3 the queue (remaining in-scope dedup families 2+10, 14/26, 38 → plan-post-s408 §2); §4 rules: **dup-census scope = COMPILER + RUNTIME ONLY (USER s413; tools may be replaced; tests never optimized)**, the census is a bug finder (probe the DIFFERENCE between copies vs perl before unifying — #393, #394), one sweep covers an IDENTICAL batch, an interrupted tool call may have RUN.

2c. **`docs/plan-one-compiler-s411.md` + `docs/dup-census-worklist-s411.md` — THE LIVE QUEUE since s411 (Fable, 2026-08-18; USER: "structural first, but not at any cost")**: the answer to #379, sized from measurements — the two generators differ in exactly TWO emission rules, the native attempt costs 9 % of compile time, `lower_embedded_block` declines 12 of 1 064 blocks, ~1 500 embedded blocks per corpus are compiled by v1 text and DISCARDED.  Phases **R** (`Pl/Passes.pm` + `PCL_OPT`, the optimization registry, #383) → **A** (one expression compiler: port the two rules, ONE parse, delete ExprToCL2, one dialect, #384) → **B** (one seam function `capture_v1`, PExpr `analysis_only`, hook-only blocks, #385) → **C** (the 12 decline shapes, #386) = 3–5 sessions to the release shape, replacing E5.1–E5.5's 9–17; E5.3 (`local` first) post-release; the extraction worklist #387 (`tools/dup-census.pl` + `tools/sub-call-census.pl`; verdicts EXTRACT / LEAVE / DELETED-BY / SUPERSEDED-BY) and the InterpScan consumers 2+3 port #388 interleave.  Standing rules: a found silent-wrong is FILED and jumps the queue only if it regresses a baseline or blocks a phase; a fix in code a phase deletes is wasted; Fable sessions rule asks + do structure.  `docs/fable-answers-s410.md` = the s410 asks, ruled short.  The plan-post-s408 queue resumes after Phase C (§6).

2d. **`docs/fable-answers-s408.md` + `docs/plan-post-s408.md` — the s408 batch RULED (s409, Fable, 2026-08-16) and THE LIVE QUEUE**: all seven s408 commits APPROVED as shipped (gate re-verified COLD 149/5442, only the 13 pclxs xs rows; sweep RE-RUN clean TOTAL 18513; 27 probes vs perl 5.40.3, no regression).  Standing rules added: **a census INCREASE is legal when it converts a worse failure (a crash-form) into a counted drop**; **a gate row count is compared against a measurement of the SAME tree** (the xs files produce 0–14 rows on their own); **an eval-mode drop DIES — "announce and continue" REJECTED** (it keeps the wrong value); a fragment mini-parse is the established pattern; **a `--quick` NOT-RUN row is never a mover** (#366 runner bug fixed: 11 files were re-run ALONE, +23 min).  **Three tasks head the next Opus session — #378** (anon `__SUB__` IMPLEMENTED as a self-reference rewrite: #368's die aborts op/sub.t 51/14 → 25/6, a cost s408 did not measure; the shape is modern Perl's recursive closure), **#377** (`sub outer { my $x = shift; my sub inner { $x } }` CRASHES unbound `$x__file__0` — p-raw-params binds the promoted name, no cell) and **#376** (the lexsub rename's three uncovered spellings: `my sub c; sub c {…}` fwd-decl SILENT WRONG across two scopes; `sub NAME` in the region defines the LEXICAL in perl; cross-package use crashes).  #374 half (b) corrected (position-aware renaming, not TERM TERM TERM).  **Queue (plan-post-s408 §2): H = #378 → #377 → #376 → #341 measured (→ #373 only if rows sit behind it) → I = #342-2 + #281 items 1+2+6 → J–L = Option B phase 2 (#371 → #372 → #343 → #369/#370 → the flip) → M–N = #279 → #280 → #282 → #283.  Fable: #281 design half, B1 operand grammar, boxed aggregates post-v0.1, #221 post-release.**

2e. **`docs/fable-answers-s406.md` — the s404 + s405 + s406 asks ALL RULED as one batch (s407, Fable, 2026-08-16): all three sessions APPROVED as shipped (gate independently re-verified 147/5355; sweep RE-RUN clean TOTAL 18517; try/catch, #347, #358, #348 and #362 probed live).  Two REVIEW FIXES landed (`e79f0a6`): a Word after `->` is a METHOD NAME and ends a term — `$o->name x 3` (#361 regression), `$o->w / $o->h / 2` (#351 regression), `$o->w*w()` (#354 hole) were one family; and **#362 CLOSED at its real cause** (`%to-number-raw` had no `functionp` arm — the compared-only side was frozen to a raw numeric slot; NOT `\&NAME` identity).  Rulings: #360 = PPI `custom_feature_include_cb` (core feature pragmas are LANGUAGE; **`use v5.40; try` is a whole-statement DROP today**) + `lib/experimental.pm` shim; **a DROP inside a string eval DIES** (#363, the server discards stderr); string-eval feature inheritance #364; #337 split confirmed; #359 behind the release, fd-3 announces; runner serial re-run #366 + process-group kill #367; anon `__SUB__` dies #368; imported `()`-sub bareword #365.  **Queue: `docs/plan-post-s400.md` §2d** — F=#337 → #360+#364 → #363 → #366+#367 → #342-2 → release leftovers.**
2f. `docs/fable-answers-s402.md` - **s402 review RULED (s403, 2026-08-15, quick — NOT re-verified, by USER instruction)**: s402 APPROVED as shipped.  Blanket `$SIG{__WARN__}` deletion STANDS though its condition was false — the only forms are fix-the-cause or `no warnings 'category'` at the narrowest scope (tree walkers get `'recursion'`; #352).  **General rule: a diagnostic that can fire during a RUN must answer "and on a warm cache?"** (`--module` OFF ratified; modules join the drop census via cached emission).  ir-spec §9.2 keeps discovered-by-stamp.  #351's repair keys on #266's callable classifier, layer `_repair_*`.  **#354 NEW: PPI 1.291 lexes `)*name` as a GLOB — `length($k)*length($k)` dropped whole (Data::Dump:325).**  #355 = one stderr-aware Pl/t helper (drop announcement FAILS the row).  #353 folds into session B.  Queue: B (#345 #349 #350 +#353) → #354+#351 → #355 → plan-post-s400 C–G.

2g. `docs/fable-answers-s400.md` + **`docs/plan-post-s400.md`** - **s399 + s400 asks ALL RULED (s401, 2026-08-15) and THE PLAN FOR THE COMING SESSIONS**: both batches APPROVED as shipped (gate independently re-verified 143/5278; sweep RE-RUN clean TOTAL 18516).  **WHAT-TO-RUN-WHEN table replaces the count rule** (Quick Reference above; #345 implements `--quick`).  Drop family: announce at the DROP site (#339 (b)), census = Option B phase 2's metric, DIE only as phase 2's last step, DROPS as a runner column (#343); file-level lvalue-sub drops keep dropping loudly.  #221 SCHEDULED post-v0.1.  #348 after #346 (first) → #347.  **TWO NEW SILENT-WRONGS from review probes: #349 (the checked-in artifacts RESET `@INC` when they load at first `pack` — closes #217 via `pl2cl --extension`) and #350 (file-top `require` hoisted above a runtime `push @INC`).**  Opus sessions A–G: #339+#343 pieces → #345+#349+#350 → #346+#342 piece 1 → #340 try → #277 installer → #337→#341 → #347→#348 → v0.1 rest.  Fable: #281 design → Option B phase 2 → boxed aggregates.  Five USER decisions still open (plan §4).
2h. `docs/fable-answers-s396.md` - **s395 + s396 asks ALL RULED (s397, 2026-08-15)**: both batches APPROVED as shipped (gate independently re-verified 141/5203 cold; sweep RE-RUN clean TOTAL 18539; ten refaliasing / n-at-a-time probes vs perl identical).  **Orphaned `pl2cl --server` ends ITSELF** (getppid tick, s397a) — the s396 PPID==1 reaper was inert under `systemd --user` (a subreaper adopts orphans); reapers stay as belt keyed on parent-is-a-reaper.  **NO new suite verdict for "measures perl internals"** — one citable not-supported class section instead; op/const-optree.t may register XDIFF.  #323 = own session; refaliasing-first ratified.  **#332 filed** (parenthesised-array refaliasing spellings silent wrong).  Queue: #331 artifacts (opener + staleness row) → #332 → internals registration → #323 → F-D → size try/lexsub/lex.t singles → v0.1; Fable: FOLD chunk 3 — **DONE s398 (Option B phase 1 complete, `docs/DECIDED.md` §s398); next Fable = #281 macro vocabulary → boxed aggregates.**  Fillers filed s398: #333–#336 (all pre-existing silent-wrongs found by the chunk-3 review probes).
2i. `docs/fable-answers-s385.md` - **s385 asks ALL RULED (s386, 2026-08-12)**: both s385 commits APPROVED as shipped (gate independently re-verified 138/5103 cold; eight probes vs perl). **#296 design call RATIFIED** (option (a) rename; do not re-litigate). **#296-B1 RULED: NOT progv** — eval-mode name resolution consults the CAPTURE ALIST before the special table (alist key ⇒ ordinary renamed-lexical path; five-row acceptance table vs perl in task #296). **#296-B2 DIAGNOSED**: sibling same-scope redeclaration — the earlier decl's rewrite region must STOP at a sibling redecl (two-line reproducer; the "not isolated" claim was a by-number TAP join onto the wrong region). **Standing rule (ask 3)**: bucket counts need the row TOTAL in both directions AND a row number is only meaningful within its own run. Shape-assertion expectation-edit conjuncts + sample-rename default (ask 4). #300 unscheduled (ask 5). runpcl/runt blank-line strip fixed (byte-compares were falsified). Queue: #296 finish → #291 → #292 → v0.1 track.
2j. `docs/fable-answers-s378.md` - **s378 asks ALL RULED (s379, 2026-08-09)**: both s378 commits APPROVED as shipped (gate independently re-verified 133/4787 cold; nine resolver probes vs perl). **Absorb-vs-companion rule refined** (zero-new-mechanism + loud + guarded = may absorb). **#287 ruled**: two halves ONE commit (drop $a/$b immunity + sort lowering binds the pair the block reads), after #237. **#237 ruled (b′)**: ONE shared variable-reference event scanner (from StringInterpolation.pm) + intuit_more classifier in the regex consumer only; deleting `_gen_interp_regex_pattern`'s walk is the acceptance bar; #286 NOT folded. Two review fixes shipped (`2af263f`): signature params now bind in the package-switch resolver; keyword-shaped hash keys no longer die. #288 filed (bareword call in switched region falls back to enclosing sub where perl dies). **USER-asked design review: `docs/var-handling-review-s379.md`** — 51% of Parser2.pm is variable identity, 26 interp-scanner sites; directions A (bind-once symbol table) / B (one scanner = #237) / C (one promotion engine) / D (defglobal, measure first); standing rules: no new scanner fixes, no new suffix family, no new scope walk. Queue: #237 → #287 → v0.1 track.
2k. `docs/fable-answers-s376.md` - **s376 asks ALL RULED (s377, 2026-08-09)**: all four s376 commits APPROVED as shipped (gate independently re-verified 133/4785; #276 six-shape probe + #275 import-list-plan spelling byte-identical to perl; #239 reproducer reproduced). **Expectation-rewrite rule is standing** (four conjuncts: perl-probed text, diff = exactly the divergence, edit STRENGTHENS, guard row in the same commit). **`builtin::` BLESSED as the shim-dispatch seam** for box-representation primitives (perl-shared names match perl; `prototype` stays CORE/runtime). **A `lib/` (or `cl/`) change makes the sweep NON-OPTIONAL** — invisible to corpus-diff. **#239 fix shape ruled**: sibling trigger on `_rewrite_var_uses`, four-way resolver from the existing scope walk (lexical incl. in-scope `our` → declaring pkg / magic / qualified / else → X), unclassifiable DIES (s372 gate-SET bar), variables only + probe the in-region CALL shape; NEW probe finding: the bare-block `our`-alias case diverges TODAY in the opposite direction (mandatory guard on #239). **uniqnum = hybrid key, measured 4-of-6** (integer text for |v| < 2^64, else pack "d"; undef→0 output). Two predicates stand (s370 style); filed #285 (no-plan diagnostic, stderr + exit 254) + #286 (ambiguous braces, deferred). Queue: #239 → #237 → v0.1 track.
2l. `docs/fable-answers-s374.md` - **s374 asks ALL RULED (s375, 2026-08-09)**: all four s374 commits APPROVED as shipped (gate independently re-verified 133/4773; #236 renderer byte-identical to perl, #234 four-shape probe identical, Ask-2 divergence probed PRE-EXISTING at f44e947). **#266's three-valued asymmetry IS the ruling** (`no` keeps answering CALL; no completion campaign — interleave on cause lines); the ask's mutual-recursion residue does NOT reproduce (probed). Package-blind unqualified path accepted unscheduled (re-raise like #191). Comment-linked interpolation twin stands. **Cadence ruled**: corpus-diff-identical + lib-reach byte-compare + green gate ⇒ same-session second sweep OPTIONAL. Queue: #275 → #276 → #238 → #239, then #237; FOLD (#153) begun s375 (Fable).
2m. `docs/fable-answers-s372.md` - **s372 asks ALL RULED (s373, 2026-08-09)**: #274 + #272 APPROVED as shipped (gate independently re-verified 132/4747; four block-boundary probes vs perl all match). **Ask 2 blessed as a standing narrowing**: a decline→die-only edit may meet the gate-SET bar with a both-populations die-scan + corpus-diff + sweep TOTAL/LOST (all three legs required). #269 park behind #196 confirmed; #271 ruled BEHIND #153's FOLD (its `pipe my ($r,$w)` shape joins the FOLD's acceptance probes); #272's map/sort/grep-block boundary probed costless, no residue. Queue: #266, then #236→#234→#235; FOLD (#153) stays Fable's.
2n. `docs/fable-answers-s370.md` - **s370 asks ALL RULED (s371, 2026-08-09)**: #267 APPROVED as shipped (gate independently re-verified 132/4744; ten probes vs perl identical incl. same-scalar-twice, k=2 vivification, named loop var). **Ask 1**: probes + guard rows ARE the bar when a shape occurs in no corpus — do not widen; corpus-diff-first ratified. **Ask 2**: the k=1 anchor-miss must DIE like k>1 (always a compiler self-inconsistency) — own filler commit, two-population gate SET + sweep TOTAL/LOST first (task #274). **Ask 3**: two comma walks CONFIRMED (provable agreement on qualifying lists; the veto is a superset); paired invariant comments required, no refactor. **§5**: mixed-list residue corrected — `@a`'s elements DO alias through the flattener; the miss is element-shaped slots + `values`. Queue: #269 → #274 → #272, #271 sizing, #266, #236→#234→#235; FOLD (#153) stays Fable's.
2o. `docs/fable-answers-s368.md` - **s368 asks ALL RULED (s369, 2026-08-09)**: s367 (#270) + s368 (#265 rename half) APPROVED as shipped (gate independently re-verified 132/4739 cold, all probes match perl). **#267 sizing decided**: the N=1 rule IS the N=k rule — all-single-scalar multi-element foreach lists emit `(vector E1 … Ek)` (shared #138 splitter, boxes never through `p-flatten-args`, mixed lists stay flattened boxless); TWO commits — wrapper switch first with its own ZERO-change full sweep, then the per-element box verdict; sweep TOTAL/LOST required. #269 do-not-delete confirmed, measure reachable rows first. #271 argument-run-lowering layer endorsed. **NEW residue #272** (review probe, pre-existing): embedded `my` inside an ANON sub body still veto-refused → global read → crash; condition should be "inside ANY sub body". Queue: #267 → #269 → fillers (#272, #271 sizing, #266, #236→#234→#235); FOLD (#153) stays Fable's.
2p. `docs/fable-answers-s365.md` - **s365 asks ALL RULED (s366, 2026-08-09)**: all eight s365 commits approved (gate independently re-verified 132/4737, every semantic ask probed live vs perl). **#254 + #252 CLOSED**; A-i's extent design STRUCK (s363 premise superseded); timeout registry ratified, NO blind suite retry; "one capture test, promoter = gate" standing; filler-scope rule (same mechanism + gate-SET measured + new axes filed). **NEW BUG #270 found by review probes**: `sub :prototype($) {…}` at expr start = SILENT DROP (`$)` lexes as magic var, block swallowed, repair declines silently) — decline must DIE + repair extends. Opus queue: #270 → #265 rename half (shape approved) → #267 (size first) → #269 (probe first) → fillers. #153 FOLD stays Fable's.
2q. `docs/fable-answers-s357.md` - **s357 APPROVED + the two E4.1-aftermath asks USER-decided (s358, 2026-08-08): #252 = SPLIT** — phase 1 (Text::Balanced) shipped s358: general forward-goto (nested catches, defer-decls-to-my-nesting), the `_reads_name_rx` false-self-ref fix, interp subscript-chain continuation; board 933 ok vs v1-era 780.  Phase 2 = `docs/e41-suite-families-plan.md` (#254, awaiting scope approval).  **#243 = PORT the 27 Pl/t callers (#255), then delete.**  Ratified: the audit populations are FOUR (sweep + board + Pl/t + perl suite) and live-v1-style audits run on a COLD cache.
2r. `docs/fable-answers-s352.md` - **s352 ask RULED (s353, 2026-08-07): the E4.1 flip stays HELD; the gate's 27 v1 routes are ALL pre-work, option (b+).**  Measurement independently reproduced event-for-event (gate green 131/4652).  Fix order M3→M6→M2→M1→M4→M5 (tasks #247/#248), re-running the instrumented gate after each family; step-2 precondition = zero TODO events across sweep + board + Pl/t gate (§5a.2 amended — s342c never measured Pl/t).  M1 = whitelist leading-`my` predicate on #226's collapse (Sub::Quote, 15/15 events verified zero-free-name); M2 = braced-interpolation `"${name}"` fix + PPI-narrowed blocker (the two Moo hits are string interpolation, probed); M5 (static-variable idiom) = one-session cap + stop-rule, only a fired stop-rule reopens #153-first.  s345 §2's "ZERO events" premise SUPERSEDED for the leading-statements shape; steps 1 + F8 approved as shipped.
2s. `docs/eval-region-measurements-s350.md` - **#240 step 2 SHIPPED s351, #240 CLOSED; ALL E4.1 pre-work is done.**  `p-eval-thunk` takes an optional region-package designator (emitted only by the #226 eval collapse) and binds `*package*` to it around the free-name resolution AND the body — so every "unqualified → current package" resolution in the runtime answers X, as perl says inside a `package X;` eval region.  All three spellings of the silent-wrong closed; the step-1 gate + Cast+Block arm and all three s350 instruments DELETED; the thunk is now emitted whenever a region is present even with no free names.  **E4.1 step 2's refusal-rephrase list is down to multi-switch + F6.**  This file holds the measurements the fix was sized against (§2b board numbers, §3 the 13-site `*package*` survey, §6 the forced-wrap probe); the ruling is `docs/fable-answers-s348.md` §2c.
2t. `docs/fable-answers-s348.md` - **s348 ask RULED (s349, 2026-08-07)**: s348 approved as shipped (gate independently re-verified 131/4648, all probes + routing reproduced). Wider #240 hole gets NO interim gate — **step 2 RE-SCOPED to the runtime route and PROMOTED pre-flip** (the s347 parking reason doesn't apply): bind `*package*` to X's CL package around `p-eval-thunk`'s free-name resolution AND body (NOT a `p-eval-lex-lookup`-only patch — that regresses the symref-without-`our` case, probed). Measurement first (miss-path instrumentation piggybacked on #230's audited sweep + the ~10-site `*package*` survey), stop-rule = (ii)-as-gate die-at-the-miss, one-session cap. Step-1 gate + arm deleted in the step-2 commit. Queue: #230/F6 (+instrumentation) → #240 step 2 → E4.1 steps 1–4 → STOP → Fable #153/E5.0.
2u. `docs/fable-answers-s346.md` - **s346 asks ALL RULED (s347, 2026-08-06)**: all four s346 commits approved (gate independently re-verified 131/4640). Gate-the-hole policy CONFIRMED as a standing rule; **#240 SPLIT** — step 1 PRE-FLIP (narrow the eval-region `our` gate to declare-then-use; the write-only `$VERSION`/`@ISA` idiom must pass, probed s347) + the file-mode `sub { package X; use M; }` guard row (a real fix #226 shipped unclaimed, verified live) in the same commit; step 2 (two-half emitter fix) = first post-E4.1 compiler item or with E5.4. **§5a.3 AMENDED**: zero UNEXPLAINED eval-mode fallbacks — ruled refusals excepted, perl-shaped + not-supported entries naming their owner in the step-2 commit. **F6 re-scope ACCEPTED**: locate first; top-level-`my` shape → extend `_oversized_top_decls`; eval/fresh_perl source → no pre-flip fix, ruled refusal. Queue: #240 step 1 → #230/F6 → E4.1 steps 1–4 → STOP → Fable #153/E5.0.
2v. `docs/fable-answers-s339.md` - **s339 asks ALL RULED (s340, 2026-08-03)**: both s339 commits approved as shipped (atomic cache write = the PRIMARY fix, warm-first the belt); XS announce-not-die ratified whole-file, NO die-across-the-boundary mechanism; getproto* approved with the fallback-host divergence accepted; stale baselines → task #223 (fail-baseline rows leave by EDIT, pass-baseline re-blesses only gate-green after a per-file audit, `# taken-at:` stamp). Queue: #223 → E4.1 (Opus, 1–2 sessions) → STOP, hand to Fable for #153/E5.0 steps 1–2.
2w. `docs/fable-answers-s337.md` - **s337 asks ALL RULED (s338, 2026-08-03)**: s337's three commits approved as shipped; #152 closes at the runtime (one bounded xs-grep extension, wider net REJECTED); closed-handle value → #220 (behind #153/E4.1); getprotobyname via /etc/protocols; warnings model → #221 UNSCHEDULED (default-off diagnostics stay absent, never unconditional); tie warning folds into the shared announce helper; #215 gains warm-first; near-green queue = standing filler + utf8::encode probe. Next-session batch = task #222, then E4.1, then #153/E5.0 (Fable steps 1–2).
2x. `docs/fable-answers-s334.md` - **s333+s334 asks ALL RULED (s335, 2026-08-03)**: no `ref-kind` slot CONFIRMED (s318 tag ruling superseded); ref strings uncached ACCEPTED; #211 parked behind #153 (leniency covers only the two measured spellings); sweep gate vs load noise = MemAvailable note + serial re-run of a LOST file, serial verdict replaces, report shows both (#215); #213 re-scoped — its TIME was `_ends_in_comment` (fixed s335 byte-identically, with the two W10 per-token `_ref_shadowed` loops), remaining = clamp-indent (a) cosmetic + `let*` runs (b) at E5, (c) REJECTED; "suspect X" tasks carry the discriminating measurement
2y. `docs/fable-answers-s331.md` - **s330+s331 asks ALL RULED (s332, 2026-08-03)**: harness `not ok`-not-die RATIFIED (#152 inherits the harness/runtime split); TAP-interleave hard rule; like-string-pattern = accepted divergence; UNSTABLE/LOST independent, no promotion; writes_args no-whitelist + refinement not scheduled; vivification rule probe-confirmed; #163 order = third path → tag → probes; §9 = the four probe-found writes_args false negatives (implicit-`$_` spellings) fixed s332
2z. `docs/fable-answers-s328.md` - **s328 asks ALL RULED (s329, 2026-08-02)**: rule-12 DIE-vs-ANNOUNCE = value-flows-onward test (gates #152); #201 File::Temp = probe the predicate, layer follows from who diverges; #202 first, then #204 (sweep TOTAL gate / LOST bucket), then #189; s329 review fixes: goto restores caller *wantarray*, veto exemption requires no free reference (residual → #205)
2a. `docs/fable-answers-s323.md` - **s323 asks ALL RULED (2026-08-02), incl. the ordered pre-/post-R1 plan (§7)**: #189 approved-with-amendments, POST-R1, warning stays as backstop; #193 principle-9 reading CONFIRMED, pre-R1 attempt authorized with the #142 stop-rule; #191 deferred (loud, rare); snapshot rule = stale-marker at crash-fix time + regenerate at quote points; USER: R1 CPAN half = four-dist baseline only, dist fetches blanket-OK'd, **full suite run every 3rd–5th change, not per change**.
2b. `docs/fable-answers-s321.md` - s321 asks ALL RULED (2026-08-02): #176 → (c) measurement fix now / bless post-R1; #177 → tainted registrations re-verify before R1 (backlog measured empty), per-row claims quote descriptions; XDIFF rows column approved post-R1. Previous rounds: `fable-answers-s318.md`, `fable-answers-s316v.md`
3. `docs/CODEGEN_DESIGN.md` - Code generation design notes

Not relevant now:
1. `docs/history/XS_BRIDGE_DESIGN.md` - superseded XS sketch; the active design is `docs/xs-shim-design.md` (libperlshim + host vtable, ready for implementation)
2. `docs/MOO_MOOSE_DESIGN.md` - Future Moo/Moose OO framework support plan
2. `docs/history/SESSION_SUMMARY.md` - Older detailed session history, use `docs/session-log.md` now


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
- `docs/parse-error-drop-census-s399.tsv` - **The #138 family, counted**: every file whose emitted CL contains a `;; PARSE ERROR:` progn (a statement the compiler could not lower, replaced by nil and execution continuing) — 33 files / 106 drops as of s420 (72/379 at the s399 measurement), with the compiler's own message per file.  Rows leave BY EDIT with their cause, never by re-blessing. A drop is NOT cosmetic: bless.t's is a test row that never runs, in a file the sweep reports as passing. Task #343 has the minimised trigger and says the fix belongs in Option B phase 2, not in the `$end_pars` region in place.
- `docs/tap-assertion-audit.md` - **What the TAP layer can and cannot claim** (#202, s330): the per-function inventory of reachable failure paths, the ten findings (unlike could not fail; eq_hash had never run; cmp_ok manufactured verdicts for `<=>`/`cmp`/`=~`/`!~`), the rule that **a claim that cannot be evaluated reports `not ok` naming the reason and only `plan()` dies**, why TAP descriptions must be Test::More's (they are join keys), and the two deliberate non-changes. Read before touching `cl/pcl-test.lisp`.
- `docs/test-debugging-runbook.md` - **HOW-TO procedure**: the faillog-driven inner loop, the FIX-vs-REGISTER decision tree, the skip-migration steps, baseline re-blessing. Read this before triaging perl-tests failures.
- `docs/xs-artifact-cache.md` - **XS artifact cache + XSLoader::load**: where a shim-built .so lives (`~/.pcl-cache/xs/abi-N/auto/...`), why the key is the pclxs ABI encoded in the PATH, why the compile is at install time, and what would change each decision. Written as decisions-with-alternatives because this is new ground.
- `docs/xs-blessed-ref-referent-bug.md` - **XS OO: DONE end to end (s315, task #115).** The full history of the blocker (s314 diagnosis → pclxs ABI-6 magic group → PCL's `xs-magic-set`/`xs-magic-get` + the `xs-ref-target` referent-identity fix + the 64-arg argv cap removal) with the rules that made it correct. Read the DONE section before touching magic/ref_target; guard `Pl/t/xs-03.t`.
- `docs/xs-abi5-and-destroy.md` - **what pclxs ABI 5 changes here, and what it costs**: nothing is broken (filehandles are the first OPTIONAL vtable capability group, so the pin can stay at abi 4), but DESTROY is now callable and needs no ABI bump — an unimplemented destructor leaks the C side of every T_PTROBJ object, which is bounded in a script and unbounded in a long-lived image. Has the performance section: cache `pclxs_has_destroy` per CLASS or pay a bridge crossing per finalized object.
- `docs/extensions.md` - **Extension loading**: `p-load-extension`, self-loading stubs, standalone binaries, adding new extensions

## Dependencies

- Perl 5.20+
- SBCL 2.5.2+ (the full suite has passed on 2.5.2 — the prior dev env until
  2026-05-31 — and on 2.6.0, the current one; the runtime uses some
  SBCL-internal symbols, so the supported floor is whatever the suite is
  actually validated against, not a guessed range)
- PPI (Perl parser)
- Moo (OO framework)
- Test::More

## Common Tasks

### Adding a new operator
1. Add to `Pl/PExpr/Config.pm` `%precedences`
2. If special handling needed, update `Pl/ExprToCL.pm`

### Adding a built-in function
1. Add to `Pl/PExpr/Config.pm` `known_no_of_params`
2. Code gen usually automatic via `pl-funcname`

### Adding a statement type
1. Add case in `Pl/Parser.pm` `_process_element()`
2. Create `_process_X_statement()` method

(v2, which is the only pipeline: a COMPOUND statement gets an arm in
`Pl/Parser2.pm`'s `_lower_compound`, and its runtime shape a macro in
`cl/pcl-runtime.lisp` — exported, or generated code cannot see it.  Worked
example, s405: `try`/`catch`/`finally` = one arm + `p-try` + a PPI repair,
because PPI leaves `finally` out of the statement it builds.)

## TODOs

### `&$foo(args)` / `&{expr}(args)` — Code Ref Call Syntax ✅ DONE (session 62)
`&$scalar(args)` and `&{expr}(args)` now generate `(pl-funcall-ref ...)` correctly.
`grep.t` fully passing (7/7). `closure.t` tests 1-7 pass; tests 8+ need Phase 2 closures.

### Phase 2 Closures — `defvar` + `let` = dynamic binding problem ✅ DONE (session 63)

**Status (session 63):** `_vars_referenced_in_closures` added to Parser.pm. `_with_declarations`
now renames captured `my` vars to `$i__lex__N` when `in_subroutine > 0`. `_process_variable_statement`
splits RHS parsing for renamed vars (handles `my $i = $i + 1` shadowing). `closure.t` 38→42/50.

**Remaining 8 failures** = `for my $n (0..4) { sub { $n } }` (foreach loop variable capture).
This requires `pl-foreach` macro changes to create a new binding per iteration — out of scope for now.

**KEY BUG to remember:** PPI's `find` returns `0` (not `undef`) when nothing found.
Always use `|| []` not `// []` when dereferencing results of `$elem->find(...)`.

**New test:** `Pl/t/closure-01.t` (8 tests, all passing).

### `map({key=>$_}, LIST)` — Hash Constructor Block in Paren-Form Map ✅ DONE (session 62)
`_block_is_hash_constructor()` added to PExpr.pm; `parse_hash_block_to_cl_string()` added to Parser.pm.
Both paren-form and block-form map/grep/sort now generate correct `(make-pl-box (pl-hash ...))`.
`grep.t` fully passing (7/7).

### Chained Method Calls
`$obj->method1()->method2()` fails — the parser emits a PARSE ERROR for the second `->` when the left-hand side is a method call result (not a simple variable). Example: `B->new()->name()`. Workaround: assign to a temp variable first. Needs investigation in `Pl/PExpr.pm` where postfix `->` is handled after a complete expression.

### Perl's Own Test Suite
Extract tests from Perl's source distribution (`t/` directory) to verify PCL.
Perl uses these to verify new Perl builds work correctly - they cover edge cases
and expected behavior comprehensively. Start with:
- `t/op/` - operator tests
- `t/base/` - basic functionality
- `t/uni/` - unicode (later)

These would provide authoritative verification of Perl semantics.

### Suggested Workflow: `perl-tests/` Failures → `Pl/t/` Tests

When investigating a failing `perl-tests/foo.t` file, consider first creating a
focused `Pl/t/foo-01.t` file that reproduces the specific failure modes as small,
targeted tests. Benefits:

- Faster iteration — no need to re-run the full 200-test file on every attempt
- Self-documenting — the test file records *what* fails and *why* at the unit level
- Regression protection — once fixed, the `Pl/t/` test prevents that bug returning
- Easier diagnosis — smaller test cases isolate whether the issue is codegen or runtime

**Pattern:**
1. Run `perl sweep-perl-tests.pl --jobs 1 perl-tests/foo.t` to see the failure count
2. Inspect the generated CL (`./pl2cl < perl-tests/foo.t > /tmp/foo.lisp`) for wrong output
3. Write `Pl/t/foo-01.t` with:
   - Transpilation tests (`like($cl, qr/expected-pattern/, 'desc')`) for codegen bugs
   - Runtime tests using the `run_cl()`/`test_cl()` pattern for semantic bugs
4. Fix the code against the `Pl/t/` tests, then verify the sweep count improves

See `Pl/t/sort-01.t` for an example (created session 93, documents sort.t failures).
