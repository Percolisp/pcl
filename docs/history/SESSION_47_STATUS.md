# Session 47 Status — exists_sub.t work, in-package problem

## What We Were Working On

Fixing `exists_sub.t` from Perl's own test suite. This test covers `exists &sub`
(checking if a subroutine slot exists in the symbol table, even if only declared,
not defined).

## Two Bugs Found

### Bug 1 (FIXED): Package stack contamination between parse passes

`parse_file()` does two passes and shares the `Environment` object between them.
The first pass calls `push_package()` for every `package Foo;` statement but never
pops them. So when the second pass starts, `environment->current_package()` returns
the last package from the first pass (e.g. `P2`) instead of `main`.

**Fix applied:** At the start of `parse()` in `Parser.pm`, reset the package stack:
```perl
$self->environment->package_stack(['main']);
```

### Bug 2 (FIXED, but caused regressions): *package* restoration after bare blocks

**The problem:** Perl files are loaded by SBCL using `load`, which reads and evaluates
top-level forms sequentially: read form N, evaluate form N, read form N+1, etc.

When a bare block at the top level contains `(in-package :P2)`, that `in-package`
executes at runtime during evaluation of the block form. After the block form
completes, `*package*` is now `:P2`. When SBCL then reads the **next** top-level
form (e.g., `(box-set $has_t1 ...)`), it reads it with `*package* = :P2`, so the
symbol `$has_t1` is interned as `P2::$HAS_T1`, which is unbound.

This caused: `Unhandled UNBOUND-VARIABLE: The variable $HAS_T1 is unbound`

**Fix applied:** In `_process_bare_block` (Parser.pm), track when inline package
changes occur (`_had_inline_package` flag set in `_emit_package_preamble`), and
after the block closes, emit `(in-package :outer-pkg)` to restore `*package*`
before the next form is read.

```perl
# At start of _process_bare_block:
my $outer_pkg_name = $self->environment->current_package();
my $saved_inline_flag = $self->{_had_inline_package};
$self->{_had_inline_package} = 0;

# In _emit_package_preamble when _block_depth > 0:
$self->{_had_inline_package} = 1;

# After _with_declarations closes:
if ($self->{_had_inline_package}) {
    $self->_emit("(in-package $outer_cl_pkg)");
    $self->_emit("");
}
$self->{_had_inline_package} = $saved_inline_flag;
```

**BUT:** This fix caused a regression — the Perl sweep ran for 7+ minutes (hung).
The `(in-package :main)` emitted after bare blocks is apparently causing some test
to hang. This needs investigation.

**Hypothesis:** The `(in-package :main)` might be changing `*package*` inside a
sub or loop body in an unexpected way, causing infinite looping or a CL hang.
Or perhaps a test that uses `package` inside a bare block is now taking a different
code path.

### Bug 3 (NOT YET FIXED): `exists &sub` generates wrong CL

`exists &t1` should check if sub `t1` exists in the symbol table (even if only
declared, not defined). Currently generates:
```lisp
(pl-exists (pl-t1))   ; WRONG: calls t1, passes result to pl-exists (hash check)
```

Should generate:
```lisp
(make-pl-box (if (fboundp 'main::pl-t1) 1 nil))
```

For qualified names like `exists &P1::tmc`:
```lisp
(make-pl-box (if (fboundp 'P1::pl-tmc) 1 nil))
```

The fix should be in `ExprToCL.pm` around line 867 in the `exists` special handler.
When `@$kids == 2` and the argument node is a `PPI::Token::Symbol` with `&` sigil,
detect it and generate the `fboundp` check.

Similarly, `defined &sub` needs a fix — currently also wrong.

## What's in the Perl Source (`exists_sub.t`)

```perl
{ package P1; sub tmc { 1 }; package P2; @ISA = 'P1'; }
$has_t1 = ok( exists &t1, 't1 sub declared' );
```

The bare block at the top level switches packages to P1 and P2 via simple `package X;`
statements (not block form). After the block exits, Perl reverts to `main`.

## Current State of Code Changes

All changes in this session are uncommitted. Files modified:

- `Pl/Parser.pm`:
  - Added `_block_depth` attribute (tracks when we're inside a bare block)
  - `_emit_package_preamble`: when `_block_depth > 0`, emit inline (not new section)
  - `_process_bare_block`: save/restore `_cur_section`, track `_had_inline_package`,
    emit `(in-package :outer)` after block if packages changed — **MAY CAUSE HANGS**
  - `parse()`: reset `package_stack` to `['main']` at start

- `cl/pcl-runtime.lisp`:
  - `pl-sprintf`: added `%N$` positional format specifiers
  - `*pl-sprintf-caller*` dynamic var for sprintf/printf error messages
  - Integer overflow detection for format widths/indices
  - `pl-printf`: binds `*pl-sprintf-caller*` to "printf"

## What To Do Next Session

1. **Diagnose the hang**: The `(in-package :outer)` emit after bare blocks is
   causing a hang. Find which test file hangs and why. Possible causes:
   - Some test has a bare block inside a loop — `(in-package :main)` is emitted
     inside the loop body, causing an infinite loop in CL's package machinery
   - The `in-package` inside a block inside a sub gets compiled and causes issues
   - Consider a safer approach: only emit `(in-package :outer)` for blocks that
     are genuinely at the TOP LEVEL of the file (not inside subs, loops, etc.)
   - Alternative: wrap the block form itself in `(let ((*package* *package*)) ...)`
     to save/restore `*package*` without needing the post-block emit

2. **Fix `exists &sub`** in `ExprToCL.pm` (line ~867):
   Detect `PPI::Token::Symbol` argument with `&` sigil and generate `fboundp` check.

3. **Fix `defined &sub`** similarly (currently also generates wrong code).

4. **Verify PCL suite** still passes after all fixes.

5. **Run Perl sweep** (excluding bop.t, heredoc.t, sort.t).

## Why We Care About `(in-package :outer)` After Bare Blocks

In Perl, `package Foo;` inside a bare block scopes to that block. After the block,
you're back in the previous package. In CL, `(in-package :Foo)` is a RUNTIME side
effect that persists. SBCL's `load` reads the NEXT top-level form using `*package*`
as set by the previous form's evaluation. So if a bare block ends with
`(in-package :P2)` having executed, the next top-level form's symbols are interned
in P2, breaking symbol resolution.

The `(in-package :outer)` after the block is the post-block restoration to fix this.
The hang suggests this restoration is appearing in the wrong place (e.g., inside a
loop or sub), causing unexpected behavior.

## Test Results Before Session

- PCL suite: 47 files, 2402 tests, ALL PASSING ✓
- Perl suite: 2889 passing, 440 failing (session 45 baseline)
- sprintf2.t: 65/66 passing (was 19/66; 1 needs $SIG{__WARN__})
- exists_sub.t: 0/N passing (broken - see above)
