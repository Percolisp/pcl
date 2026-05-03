# PCL: Wantarray / Context Propagation — Implementation Plan

**Written:** 2026-05-03  
**Status:** Planned — not yet implemented

---

## Problem

`wantarray()` inside a user-defined Perl sub should return:
- `1` when the sub was called in list context
- `""` (false, defined) in scalar context
- `undef` in void context

PCL has a `*wantarray*` dynamic variable but three bugs prevent correct behaviour.

---

## Current State

- `*wantarray*` is a two-valued `defvar`, initially `nil` (`cl/pcl-runtime.lisp:386`)
- `p-wantarray` at line 7579 returns `*wantarray*` raw — CL `t`/`nil`, not Perl values
- `annotate_contexts` in `PExpr.pm` already propagates `LIST_CTX=1 / SCALAR_CTX=0 / VOID_CTX=2` to every expression node
- `gen_funcall` (ExprToCL.pm:1563–1567) already wraps list-context calls but does nothing for scalar/void
- `gen_methodcall` (ExprToCL.pm:1654–1657) and `gen_ref_funcall` (ExprToCL.pm:1682–1685): same one-sided wrapping

---

## Root Cause: Three Separate Bugs

### Bug 1 — Scalar-context calls leak surrounding list context

`gen_funcall` lines 1563–1567 (current):

```perl
  # Wrap in dynamic wantarray binding for list context
  if ($ctx == 1) {  # LIST_CTX = 1
    return "(let ((*wantarray* t)) $call)";
  }

  return $call;   # ← no binding; inherits whatever *wantarray* is in scope
```

If `outer()` is called in list context and inside it calls `my $x = inner()`,
`inner()` is in scalar context but `*wantarray*` is still `t` from the outer
dynamic scope. `inner()` falsely believes it is in list context.

**Fix** — replace those four lines with:

```perl
  my $wa = $ctx == LIST_CTX  ? 't'
         : $ctx == VOID_CTX  ? ':void'
         :                     'nil';
  return "(let ((*wantarray* $wa)) $call)";
```

The same pattern applies to `gen_methodcall` (line 1654) and `gen_ref_funcall` (line 1682).

**Exclusions** — built-ins on the exclusion list must NOT get the generic wrapping.
The existing guard already handles `reverse`, `localtime`, `gmtime`, `caller` via the
`if ($func_name =~ /^(reverse|localtime|gmtime|caller)$/)` block at lines 1558–1563.
`split` has its own special case at line 1550. All other entries in `%RUNTIME_NAMES`
(defined lines 95–118) are built-ins that either don't call `p-wantarray` or handle
context themselves — they should be excluded from the new generic wrapping.

Revised structure:

```perl
  my $ctx = $self->expr_o->get_node_context($node_id);

  # split, reverse, localtime, gmtime, caller — existing special cases unchanged
  ...

  # User sub calls: always bind *wantarray* so callee sees correct context.
  # Built-ins don't call p-wantarray, so no binding is needed for them.
  if (!exists $RUNTIME_NAMES{$func_name}) {
    my $wa = $ctx == LIST_CTX ? 't' : $ctx == VOID_CTX ? ':void' : 'nil';
    return "(let ((*wantarray* $wa)) $call)";
  }

  # Built-in: existing list-only wrapping (for wantarray-sensitive ones called
  # in list context from within a scalar-context scope)
  if ($ctx == LIST_CTX) {
    return "(let ((*wantarray* t)) $call)";
  }
  return $call;
```

### Bug 2 — `p-wantarray` returns CL booleans, not Perl values

`cl/pcl-runtime.lisp` line 7579:

```lisp
(defun p-wantarray ()
  "Perl wantarray"
  *wantarray*)       ; returns CL t or nil — wrong
```

Perl code that does `is(wantarray(), 1)` or `defined(wantarray())` needs the
correct typed values.

**Fix** — replace the body (line 7581):

```lisp
(defun p-wantarray ()
  "Perl wantarray(): 1 in list context, \"\" in scalar, undef in void."
  (cond ((eq *wantarray* t)     1)
        ((eq *wantarray* :void) (p-undef))
        (t                      "")))
```

`*wantarray*` stays CL-typed internally (`t` / `nil` / `:void`); the conversion
to Perl values happens only at the `p-wantarray` boundary.

Also update the `defvar` comment at line 386:

```lisp
(defvar *wantarray* nil
  "Context for the current call: t=list, nil=scalar, :void=void.")
```

### Bug 3 — `return expr` silences the caller's context

`child_context` in `PExpr.pm` (line 2986) has special cases for `map`, `grep`,
`sort`, `join`, `push`, `print` etc., but nothing for `return`. The `return`
funcall node's children fall through to the default:

```perl
  # Default: children inherit parent's context
  return $parent_ctx;   # line 3191
```

Since a `return` statement is at top-level statement context (SCALAR_CTX), its
argument `foo()` gets SCALAR_CTX. After Bug 1's fix, that becomes
`(let ((*wantarray* nil)) (pl-foo))`, which silences the caller's list context.

Correct Perl semantics: `return inner()` propagates the caller's context to
`inner()`. The fix is to emit **no** `*wantarray*` binding for the return value
expression — let it inherit the dynamic value already set by the caller.

#### Step 1 — add `INHERIT_CTX` to `PExpr.pm`

Lines 29–36 currently:

```perl
use constant {
    SCALAR_CTX => 0,
    LIST_CTX   => 1,
    VOID_CTX   => 2,
};

our @EXPORT_OK = qw(SCALAR_CTX LIST_CTX VOID_CTX);
```

Change to:

```perl
use constant {
    SCALAR_CTX  => 0,
    LIST_CTX    => 1,
    VOID_CTX    => 2,
    INHERIT_CTX => 3,   # inherit *wantarray* from dynamic scope; emit no binding
};

our @EXPORT_OK = qw(SCALAR_CTX LIST_CTX VOID_CTX INHERIT_CTX);
```

#### Step 2 — add `return` case in `child_context`

Inside the `if ($type eq 'funcall')` block (around line 3020), add before the
closing brace of that block:

```perl
      # return: propagate caller's context — emit no *wantarray* binding
      if ($func_name && $func_name eq 'return') {
        return INHERIT_CTX;
      }
```

#### Step 3 — guard in `gen_funcall`, `gen_methodcall`, `gen_ref_funcall`

In each of the three generators, before the new three-way wrapping, add:

```perl
  # INHERIT_CTX: return/tail position — do not override *wantarray*
  return $call if $ctx == INHERIT_CTX;
```

(Import `INHERIT_CTX` from `Pl::PExpr` in `ExprToCL.pm`'s `use` line at the top.)

---

## The Tail-Expression Case

The implicit return — the last expression of a sub body — has the same
requirement as explicit `return`.

```perl
sub outer { inner() }      # tail position; context must propagate
my @a = outer();           # inner() should see list context
```

`_process_block` in `Parser.pm` iterates `$block->children` (around line 2940).
The last significant child is processed via `_process_element`, which calls
`_parse_expression`, which calls `ExprToCL::generate()` with the expression's
annotated context (SCALAR_CTX — the default for statement level). After Bug 1's
fix that wraps with `(let ((*wantarray* nil)) ...)`, breaking propagation.

**Fix — option A (recommended):** set a flag on the PExpr object before
generating the last statement, clear it after:

In `ExprToCL.pm`, add a boolean attribute `tail_position` (default false).
In `gen_funcall`, `gen_methodcall`, and `gen_ref_funcall`, add alongside the
`INHERIT_CTX` guard:

```perl
  return $call if $self->tail_position;
```

In `_process_block` (Parser.pm, around line 2940), identify the last significant
child (skip whitespace, comments, semicolons) and bracket its `_process_element`
call:

```perl
  # ... inside the children loop, for the last significant child:
  if ($self->environment->in_subroutine > 0 && $child == $last_sig_child) {
    $self->{_expr_to_cl}->tail_position(1);
    $self->_process_element($child);
    $self->{_expr_to_cl}->tail_position(0);
  } else {
    $self->_process_element($child);
  }
```

`_process_block` already has `$tail_last_sig` for the if-without-else case —
extend that variable to cover ALL last statements, not just the if case.

Note: `_process_block` creates a fresh `ExprToCL` object per statement via
`_parse_expression` → `Pl::PExpr->new` → ExprToCL. The flag needs to be on
the `Parser` object and read by the ExprToCL instance through the environment
or a shared ref, or alternatively passed as a parameter to `_parse_expression`.
The simplest approach: add `tail_position` to `Parser`'s state, and have
`ExprToCL::gen_funcall` read it through `$self->environment->tail_position`.

---

## Context Propagation Illustrated

```perl
sub inner { wantarray() ? "list" : "scalar" }
sub outer { return inner() }
my @a = outer();    # → "list"
my $s = outer();    # → "scalar"
```

Generated CL after all fixes:

```lisp
(defun pl-inner ()
  (if (p-wantarray) "list" "scalar"))

(defun pl-outer ()
  ;; return's arg has INHERIT_CTX → no *wantarray* binding emitted
  (throw :p-return (p-return-value (pl-inner))))

;; call sites set context:
(let ((*wantarray* t))   (pl-outer))   ; my @a = outer()
(let ((*wantarray* nil)) (pl-outer))   ; my $s = outer()
```

Dynamic scoping does the rest: `pl-outer` runs with `*wantarray*` set by its
call site; `pl-inner` inherits it because no override is emitted.

---

## `p-return` Macro — No Change Needed

The `p-return` macro at `cl/pcl-runtime.lisp:4754` already reads `*wantarray*`
for multi-value returns:

```lisp
(defmacro p-return (&rest values)
  ...
  `(throw :p-return
     (if *wantarray*
         (vector ,@...)
         (p-return-value ,(car (last values))))))
```

This is correct: it reads the dynamic `*wantarray*` at return time, which is
whatever the caller set. No change needed here.

---

## `annotate_contexts` Gaps

Beyond the `return` fix above, a few cases need verification:

| Pattern | Current annotation | Needed |
|---------|-------------------|--------|
| `foo()` as bare statement | SCALAR_CTX (falls through default) | VOID_CTX |
| `return foo()` arg | SCALAR_CTX → fixed by INHERIT_CTX above | INHERIT_CTX |
| Tail expr of sub | SCALAR_CTX → fixed by tail_position flag | (flag approach) |
| `%h = foo()` | check `assignment_rhs_context` | LIST_CTX |
| `my ($x, $y) = foo()` | check `assignment_rhs_context` | LIST_CTX |
| `bar(foo())` where `bar` unknown | SCALAR_CTX (safe default) | stays SCALAR_CTX |

For the bare-statement void case: `annotate_contexts` is called from
`_parse_expression` in Parser.pm. Statement-level calls come in as SCALAR_CTX
(the initial context passed). Adding VOID_CTX would require Parser to detect
"this statement is called for side effects only" — doable but lower priority
since `wantarray() eq undef` tests are rare.

---

## Files Changed

| File | Location | Change |
|------|----------|--------|
| `cl/pcl-runtime.lisp` | line 386 | Update `defvar *wantarray*` comment |
| `cl/pcl-runtime.lisp` | line 7579–7581 | `p-wantarray`: 3-way cond |
| `Pl/PExpr.pm` | line 29–36 | Add `INHERIT_CTX = 3`; export it |
| `Pl/PExpr.pm` | `child_context` ~line 3020 | Add `return` → INHERIT_CTX case |
| `Pl/ExprToCL.pm` | top `use` line | Import `INHERIT_CTX` |
| `Pl/ExprToCL.pm` | `gen_funcall` lines 1563–1567 | Three-way wrapping; INHERIT_CTX guard; `%RUNTIME_NAMES` guard |
| `Pl/ExprToCL.pm` | `gen_methodcall` line 1654 | Three-way wrapping; INHERIT_CTX guard |
| `Pl/ExprToCL.pm` | `gen_ref_funcall` line 1682 | Three-way wrapping; INHERIT_CTX guard |
| `Pl/Parser.pm` | `_process_block` ~line 2940 | Tail-position flag for last significant child |
| `Pl/Environment.pm` | (or Parser state) | `tail_position` boolean accessor |
| `Pl/t/wantarray-01.t` | new file | Tests (see below) |

---

## Test Plan

```perl
# Pl/t/wantarray-01.t

# Bug 2: p-wantarray return values
test_cl('wantarray returns 1 in list context',
    'sub f { wantarray() }
     my @a = (f());
     print $a[0] == 1 ? "yes" : "no", "\n";',
    'yes');

test_cl('wantarray returns "" in scalar context',
    'sub f { wantarray() }
     my $s = f();
     print defined($s) && !$s ? "yes" : "no", "\n";',
    'yes');

test_cl('wantarray returns undef in void context',
    'sub f { print defined(wantarray()) ? "def" : "undef", "\n" }
     f();',
    'undef');

# Bug 1: scalar-context isolation (the leakage bug)
test_cl('scalar call inside list-context sub does not leak',
    'sub inner { wantarray() ? "list" : "scalar" }
     sub outer { my $x = inner(); $x }
     push my @a, outer();
     print $a[0], "\n";',
    'scalar');

# Bug 3: return propagation
test_cl('return propagates list context',
    'sub inner { wantarray() ? "list" : "scalar" }
     sub outer { return inner() }
     my @a = (outer());
     print $a[0], "\n";',
    'list');

test_cl('return propagates scalar context',
    'sub inner { wantarray() ? "list" : "scalar" }
     sub outer { return inner() }
     my $s = outer();
     print $s, "\n";',
    'scalar');

# Tail-position propagation
test_cl('tail expr propagates list context',
    'sub inner { wantarray() ? "list" : "scalar" }
     sub outer { inner() }
     my @a = (outer());
     print $a[0], "\n";',
    'list');

# Method calls
test_cl('wantarray in method call list context',
    'package T;
     sub new { bless {}, shift }
     sub ctx { wantarray() ? "list" : "scalar" }
     package main;
     my $t = T->new;
     my @a = $t->ctx;
     print $a[0], "\n";',
    'list');

test_cl('wantarray in method call scalar context',
    'package T;
     sub new { bless {}, shift }
     sub ctx { wantarray() ? "list" : "scalar" }
     package main;
     my $t = T->new;
     my $s = $t->ctx;
     print $s, "\n";',
    'scalar');
```

---

## Implementation Order

1. **`p-wantarray` 3-valued** (`cl/pcl-runtime.lisp` lines 7579–7581, 386) — 5 lines. Run suite; expect no regressions since existing callers treat `t`/`nil` as booleans.

2. **Add `INHERIT_CTX`** (`PExpr.pm` lines 29–36) — 2 lines. No behavioural change yet.

3. **`child_context` `return` case** (`PExpr.pm` ~line 3020) — 3 lines. No behavioural change until ExprToCL uses it.

4. **`gen_funcall` three-way wrapping** (`ExprToCL.pm` lines 1563–1567) — highest ROI; fixes the leakage bug. **Run full suite immediately** — regression risk from over-wrapping built-ins.

5. **`gen_methodcall` and `gen_ref_funcall`** — same pattern, 3 lines each.

6. **Tail-position flag** — `Parser.pm` + `Environment.pm` (or Parser state). Lower priority; most real code uses explicit `return`.

7. **VOID_CTX for bare statements** — fill in `annotate_contexts`; lowest priority.

8. **Run sweep, measure improvement, commit.**

---

## Risks

- **Over-wrapping built-ins (step 4)**: the `%RUNTIME_NAMES` guard means only user sub calls get three-way wrapping. Built-ins that inspect `*wantarray*` (`reverse`, `localtime`, etc.) keep their existing explicit handling. If anything in `%RUNTIME_NAMES` is missing, it will regress. **Mitigation:** full suite after step 4 before proceeding.

- **`p-return` multi-value path** (line 4763): reads `*wantarray*` to choose vector vs scalar. After the fix, `*wantarray*` inside the sub reflects the caller's context correctly — this should make multi-value returns work better, not worse.

- **String eval context** (`eval "code"`): runs a subprocess; `*wantarray*` is not transmitted to it. Known existing limitation; no change here.
