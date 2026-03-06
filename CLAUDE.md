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

- **46 test files, 2306 tests**
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
1. `SESSION_SUMMARY.md` - Detailed session history
2. `XS_BRIDGE_DESIGN.md` - Future XS/C extension plans
3. `CODEGEN_DESIGN.md` - Code generation design notes
4. `MOO_MOOSE_DESIGN.md` - Moo/Moose OO framework support plan

### Semantic Deep-Dives (read before touching these areas)
- `docs/declaration-ordering.md` - Perl vs CL compile/load phases, defvar/defun ordering, local/dynamic scoping
- `docs/wantarray-context.md` - Wantarray/context system (DO NOT implement without explicit user request)
- `docs/ppi-glob-disambiguation.md` - **HIGH PRIORITY BUG**: PPI misreads `< expr >` as glob, silently drops statements
- `docs/closure-lexical-scoping.md` - **NEXT TODO**: Why `defvar` breaks closures, plan for `$x__lex__N` renaming

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

### Phase 2 Closures — `defvar` + `let` = dynamic binding problem ⬅ NEXT TODO

**Status (session 62):** Anonymous subs now generate `(lambda ...)` instead of `(defun NAME ...) #'NAME`.
Simple closures work. But `sub bar { my $i = shift; sub { $i } }` still fails because
`$i` is `defvar`'d (SPECIAL) from the package-level `my $i`, so `let (($i ...))` inside `bar`
creates a DYNAMIC binding that unwinds when `bar` returns — the lambda sees the wrong value.

**Root cause:** `defvar` makes a CL symbol globally SPECIAL. All `let` bindings of that symbol
(even deep inside named subs) create dynamic bindings. CL has no "global lexical" declaration.

**Fix:** For `my` vars declared inside subroutines (`in_subroutine > 0`), use unique CL symbol
names (`$i__lex__N`) that are never `defvar`'d. Since they're not special, `let` creates lexical
bindings — lambdas capture the correct per-call copy.

**Implementation plan:** See `docs/closure-lexical-scoping.md` for the full plan.
Short version:
1. `_with_declarations` (Parser.pm): when `in_sub > 0`, rename `my $x → $x__lex__N` via
   `$lex_var_counter` (already declared). Update rename map (`state_var_renames`) so ExprToCL
   emits the unique name. Track unique names in `_let_bound_vars` too.
2. `_process_variable_statement` (Parser.pm): for `my $var = EXPR` where `$var` is being newly
   declared, parse only the **RHS tokens** with the outer rename for `$var` temporarily active
   (prevents `my $i = $i` self-assignment). Emit `(pl-my-= UNIQUE_NAME RHS_CL)` manually.

**Key difficulty:** Can't suppress rename for just the RHS of a full expression parse.
Solution: extract RHS tokens from `@parts` (tokens after `=`), parse them separately.

**Files to change:**
- `Pl/Parser.pm`: `_with_declarations` + `_process_variable_statement`
- Nothing else (ExprToCL already checks `state_var_renames`; PExpr already uses it)

**Expected impact:** Fixes `closure.t` tests 8+ (currently 38/50 pass) and the generator
pattern in `state.t` tests 19–23.

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
