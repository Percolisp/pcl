# CLAUDE.md

This file provides guidance to Claude Code when working with this repository.

## Project Overview

**PCL** (Perl to Common Lisp) is a transpiler that converts Perl code to Common Lisp. It parses Perl using PPI, builds an AST, and generates CL code.

**Status:** V2 complete - expressions, statements, constants, OO support

**Important:** This is a Perl project. Use Perl (not Python/Ruby/etc.) for any scripting, one-liners, or helper scripts. This includes searching log files, processing text, and any task where a Perl one-liner via Bash would work — don't use subagents for what Perl can do directly.

## Design Principles

1. **CPAN Compatibility**: Match Perl semantics exactly. The goal is to run real CPAN modules.

2. **Readable Generated Code**: Keep generated CL readable for Perl programmers. Use Perl-like naming (pl-sub, pl-print, $varname) so the output is recognizable.

3. **Compile-Time Visibility**: Wrap sub definitions and variable declarations in `eval-when` so BEGIN blocks can call subs and access variables defined before them in source order.

4. **No Easy Write-Offs**: Don't dismiss problems as "documented limitations" without discussion. Every incompatibility is serious because it blocks CPAN code. If something doesn't work, either fix it or discuss the tradeoffs with the user before marking it as a limitation.

5. **Never Simplify Tests**: When a test fails, fix the code, not the test. Tests from Perl's own test suite are authoritative - they define correct behavior. Commenting out failing tests or replacing them with `ok(1, 'SKIP: ...')` hides bugs instead of fixing them. If a feature is genuinely out of scope, discuss with the user first.

6. **Add Regression Tests for Bug Fixes**: When fixing a bug, add a test case to an existing test file that covers the fixed behavior. This prevents regressions. Prefer adding to existing files over creating new ones. **Do NOT add to `transpile-test-01.t`** (118 tests) or other large `transpile-test-NN.t` files — each file spawns a new SBCL process, so file count matters less than test count per file. Add to the smallest `transpile-test-NN.t` file, or create a new one if needed.

7. **Document Complex Semantics in `docs/`**: When solving a problem involving tricky Perl-vs-CL semantics, write a `docs/topic-name.md` file explaining the problem, the solution, and edge cases. Reference it from CLAUDE.md's "Key Files to Read" section. This prevents re-investigating the same issue in future sessions. Examples: declaration ordering, wantarray context, string escapes.

8. **Do NOT work on wantarray/context fixes** unless the user explicitly requests it. See `docs/wantarray-context.md`.

9. **Assume Valid Perl Input**: PCL is a transpiler for functioning Perl code, not a validator. It does not need to detect or reject invalid Perl (syntax errors, non-associative operator chains, etc.). Tests that verify rejection of invalid Perl (e.g. `eval("sub { $a <=> $b <=> $c }")` returning `undef`) are out of scope and should be commented out, not implemented.

## Quick Reference

```bash
# Run all tests (from project root) — always use -j8 for parallel execution
prove -j8 Pl/t/

# Test single file
prove -v Pl/t/codegen-01.t

# Quick transpile test
echo 'my $x = 1 + 2;' | ./pl2cl
```

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

- **52 test files, 2481 tests**
- **All passing**
- **Runtime: ~2 min with `prove -j8`** (each test spawns a new SBCL process)

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
1. `docs/session-log.md` - Session history (compact, newest first)
2. `SESSION_SUMMARY.md` - Older detailed session history
2. `XS_BRIDGE_DESIGN.md` - Future XS/C extension plans
3. `CODEGEN_DESIGN.md` - Code generation design notes
4. `MOO_MOOSE_DESIGN.md` - Moo/Moose OO framework support plan

### Semantic Deep-Dives (read before touching these areas)
- `docs/declaration-ordering.md` - Perl vs CL compile/load phases, defvar/defun ordering, local/dynamic scoping
- `docs/wantarray-context.md` - Wantarray/context system (DO NOT implement without explicit user request)
- `docs/ppi-glob-disambiguation.md` - **HIGH PRIORITY BUG**: PPI misreads `< expr >` as glob, silently drops statements
- `docs/closure-lexical-scoping.md` - **NEXT TODO**: Why `defvar` breaks closures, plan for `$x__lex__N` renaming
- `docs/todo-features.md` - **Features left to implement** (tiered, with test counts and fix areas)
- `docs/not-supported.md` - **Deliberate non-support** (design decisions: `@_` aliasing, Unicode limits, etc.)
- `docs/v1-implementation-plan.md` - **V1 feature plan** (prioritized, with full implementation details for each item including `local $hash{key}`, bare-if return, string eval, etc.)

## Dependencies

- Perl 5.30+
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
