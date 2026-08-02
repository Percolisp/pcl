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
4. Only then probe (minimal scratch file, perl vs `./runpl`).

And symmetrically, when work SETTLES a question: add a one-line pointer to
`docs/DECIDED.md` in the same commit; put load-bearing semantic decisions
in `docs/ir-spec.md` (a code comment may point at the spec, but must never
be the only copy); record failed attempts and what killed them in the task,
so the task says what NOT to retry.

## Design Principles

1. **CPAN Compatibility**: Match Perl semantics exactly. The goal is to run real CPAN modules.

2. **Readable Generated Code — but speed wins**: Keep generated CL readable for Perl programmers where it is free. Use Perl-like naming (pl-sub, pl-print, $varname) so the output is recognizable. **Priority decision (2026-07-02): when readability and performance of the generated code conflict, performance wins** — never sacrifice a speed transform for prettier output. See `docs/where-the-time-goes.md` and `docs/codegen-rewrite-review.md`.

3. **Compile-Time Visibility**: Wrap sub definitions and variable declarations in `eval-when` so BEGIN blocks can call subs and access variables defined before them in source order.

4. **No Easy Write-Offs**: Don't dismiss problems as "documented limitations" without discussion. Every incompatibility is serious because it blocks CPAN code. If something doesn't work, either fix it or discuss the tradeoffs with the user before marking it as a limitation.

5. **Never Simplify Tests**: When a test fails, fix the code, not the test. This applies to both `perl-tests/` (authoritative Perl test suite) and `Pl/t/` (PCL regression tests). Do NOT weaken a `Pl/t/` test to a simpler form just because the original form fails due to an unrelated bug — keep the test semantically equivalent to what it's supposed to verify. Commenting out failing tests or replacing them with `ok(1, 'SKIP: ...')` hides bugs. If a feature is genuinely out of scope, discuss with the user first — then mark it via the **declarative skip-registry** (`cl/skip-registry.lisp`), NOT by editing the `perl-tests/*.t` file. The registry keys on the test description, cites a `docs/not-supported.md` reason, still runs the assertion, and flags itself stale if the test ever starts passing. See `docs/test-skip-registry.md`. Crashing/aborting tests are NEVER auto-skipped — they stay CRASH/PARTIAL as fix targets.

6. **Add Regression Tests for Bug Fixes**: When fixing a bug, add a test case to an existing test file that covers the fixed behavior. This prevents regressions.

   **The constraint is a file's RUN TIME, not its test count** (user, 2026-07-28, restated and refined 2026-08-01). `prove -j8` parallelises across files, so the gate's wall time ≈ the *slowest single file* — a file with many fast rows is fine, a file with a few slow ones is not. What makes a row slow: `test_transpile` runs a perl oracle **and** an SBCL transpile+run; `run_cl` spawns SBCL. So before adding, ask *how long does this file take*, and measure with `prove --timer Pl/t/<file>`; if the answer is heading past the current slowest file, start the next `transpile-test-NN.t` instead (copy an existing file's header/helpers). `transpile-test-01.t` (118 rows) and `transpile-test-07.t` (45, closed by the user in s321) are the two to leave alone; as of s321 the files are 01, 01b, 02, 03, 04, 04b, 05, 06, 07, 08, so the next new one is **`-09`**.

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
```

**Pipelines:** the v2 structured-emission pipeline (`Pl/Parser2.pm` +
`Pl/ExprToCL2.pm` + `Pl/VarAnnotator.pm` + `Pl/CLForm.pm`) is the **default**
since 2026-07-05 (W9); anything it can't lower dies "Parser2 TODO: …" and
falls back to v1 per file. `PCL_V1=1` forces the original v1 pipeline
(`Pl/Parser.pm` text-stream emission); `PCL_V2=1` is a no-op kept for old
scripts. Module-cache paths are keyed by `cl/pcl-runtime.lisp`'s
`*pcl-cache-generation*` **plus the effective pipeline** — bump the
generation string on any emission-changing commit, or stale cached module
transpiles will be reused. **Plan/status: `docs/v2-endgame-plan.md` (E1–E5 + §6 the two product
targets: beat-perl speed / clear macro-IR — perf worklist in
`docs/faster-codegen-suggestions.md`)
+ `docs/v2-opus5-execution-plan.md` (**the CURRENT ordered worklist +
per-session checklist + guardrails, s316t**; reader's overview:
`docs/v2-upcoming-work.md`).** `docs/v2-opus48-execution-plan.md` (E1/E2
era) and `docs/v2-completion-plan.md` (W-phase) are background. Verify emission changes with `tools/corpus-diff.pl`; track
gates with `perl tools/v2-census.pl`.

**Generated artifacts checked into the tree — regenerate them after an
emission-changing commit, or they keep running on the codegen that built
them.** `cl/pcl-pack.lisp` is `cl/pack-impl.pl` (pack/unpack, written in
Perl) transpiled by PCL plus a hand-written appendix: run
**`tools/rebuild-pack`**, then `perl sweep-perl-tests.pl --jobs 1
--timeout 380 perl-tests/pack.t` and `tools/sweep-diff.pl` (expect 0 new;
s316b: 5635 pass / 90 fail).  It was found at gen v2-30 against a v2-71
compiler — 40 generations of drift, which made every pack.t run a test of
the *old* emitter.  `cl/pcl-mro.lisp` comes from `lib/mro.pm` the same way.

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

- **126 test files, 4495 tests** with a built pclxs sibling (s326, measured);
  **4481 without** (arithmetic: minus the 14 xs rows).  The gate count is deterministic *per environment*, but it
  is conditional: `Pl/t/xs-01/02/03.t` (6+4+4 = **exactly 14** rows) resolve
  pclxs as `$FindBin::Bin/../../../pclxs` — **a sibling of the CHECKOUT** — and
  `plan skip_all` (contributing 0) when it is missing or `libpclxs.so` is not
  built.
  **GOTCHA that cost s319 an investigation: comparing against a `git worktree`
  silently subtracts those 14**, because the worktree lives elsewhere and its
  sibling path does not exist.  A worktree is still the right way to compare
  against HEAD (never a stash-copy) — just set `PCLXS_DIR=~/pclxs`, or expect
  and subtract the 14.  What must hold either way is `Result: PASS` and 125
  files.
- **XS conformance: 370 pass, 0 fail — fully green** against pclxs's corpus
  with real perl as oracle (`tools/pcl-conform`, session 315; XS OO/magic
  works — Digest::MD5's own md5-aaa.t is 256/256 under PCL).
  Not in the Pl/t gate — minutes, not seconds.
- **All passing**
- **Runtime: ~2:30 with `tools/prove-core`** (~5+ min with plain `prove -j8`;
  each test file spawns a new SBCL process)
- Full `perl-tests/` sweep: **689 blessed fails** in `docs/fail-baseline.tsv`,
  **66 files fully passing**, 18462 passing / 925 failing across 108 files
  (re-measured s323 with pack.t running; `sweep-diff.pl diff
  docs/fail-baseline.tsv .faillog` = **0 new / 0 fixed**, plus 2 rows the tool
  itself flags UNSTABLE — new fails ABOVE the abort point of postfixderef.t and
  ref.t, both already PARTIAL, so they are crash-file noise, not regressions.  690 → 689 because the `do.t` "$! is EISDIR on do dir"
  row was FIXED that session — removed by EDITING that one row out, never by
  re-blessing from a run, which would silently absorb anything else that
  moved).  Baseline from s315d —
  class-model target-first reads, $TODO honored again under :invert,
  no-match s/// write gate; fresh_perl/runperl children run under PCL via
  `tools/pclperl-for-tests`; `PCL_FRESH_PERL=real` restores the old compare
  mode.
  **pack.t and the TIMEOUT retry (#176, s322).**  A file that TIMEOUTs
  contributes NO rows, so `sweep-diff.pl` can only report its baseline
  failures as *unverified* — a regression inside a timing-out file is
  invisible, and the headline "0 new" says nothing about it.  pack.t is
  merely SLOWER than the timeout (~166 s), not hung, so the sweep now
  **retries a TIMEOUT once at 3× the timeout, at the end of the queue**
  (`--no-retry` disables); no operator has to remember `--timeout 400`.
  Measured s322: pack.t completes at 5636 pass / 89 fail and its failures are
  **identical to the blessed baseline** — 0 new, 0 fixed.
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
2a. `docs/fable-answers-s323.md` - **s323 asks ALL RULED (2026-08-02), incl. the ordered pre-/post-R1 plan (§7)**: #189 approved-with-amendments, POST-R1, warning stays as backstop; #193 principle-9 reading CONFIRMED, pre-R1 attempt authorized with the #142 stop-rule; #191 deferred (loud, rare); snapshot rule = stale-marker at crash-fix time + regenerate at quote points; USER: R1 CPAN half = four-dist baseline only, dist fetches blanket-OK'd, **full suite run every 3rd–5th change, not per change**.
2b. `docs/fable-answers-s321.md` - s321 asks ALL RULED (2026-08-02): #176 → (c) measurement fix now / bless post-R1; #177 → tainted registrations re-verify before R1 (backlog measured empty), per-row claims quote descriptions; XDIFF rows column approved post-R1. Previous rounds: `fable-answers-s318.md`, `fable-answers-s316v.md`
3. `CODEGEN_DESIGN.md` - Code generation design notes

Not relevant now:
1. `XS_BRIDGE_DESIGN.md` - superseded XS sketch; the active design is `docs/xs-shim-design.md` (libperlshim + host vtable, ready for implementation)
2. `MOO_MOOSE_DESIGN.md` - Future Moo/Moose OO framework support plan
2. `SESSION_SUMMARY.md` - Older detailed session history, use `docs/session-log.md` now


### Semantic Deep-Dives (read before touching these areas)
- `docs/pexpr-term-parsing-review.md` - **READ BEFORE TOUCHING PExpr.pm's operand/term machinery** (the `$end_pars` region, named-unary operands, postfix `->` chains, ~lines 2600-3700). The region is a maze of hand-derived boundary conditions; three s316v attempts to add a rule there all failed in ways probes did not catch (task #142 records them). The fix for that region is Option B (`_reduce_term`, task #153) — do not add guards there.
- `docs/v2-target-architecture.md` - **THE TARGET SHAPE of the v2 compiler** (s316t): the pipeline as data transformations, the Stmt-classifier/Facts/CLForm data structures, the pass plug-in contract for optimizations, module map, and the calibrated cost to v2-final. Read this FIRST when joining the compiler work; the E5 steps in `docs/v2-endgame-plan.md` converge on it.
- `docs/v2-code-review.md` - **The s316t design review** that produced the target: the token-splitter bug family (three precedence tables + probe-confirmed silent-drop), the seam-state inventory, the E4.1 reachability re-scope. Gap analysis behind the architecture doc.
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
