# PCL: Bare-`if` Implicit Return — Implementation Plan

**Written:** 2026-03-27
**Feature:** B1 in `docs/todo-features.md`
**Files to change:** `Pl/Parser.pm`
**Files to test:** `Pl/t/bareif-01.t` (new), `perl-tests/do.t`

---

## 1. The Problem

Perl returns the value of the **last expression evaluated** from a subroutine.
When the last statement is `if (COND) { BODY }` with no `else`:

- If COND is **true**: BODY runs; the last expression in BODY is returned.
- If COND is **false**: COND itself was the last thing evaluated, so **COND is returned**.

```perl
sub x { if(0)  { 5 } }   # returns 0   — COND was last evaluated
sub x { if("") { 5 } }   # returns ""  — COND was last evaluated
sub x { if($n) { 5 } }   # returns $n  — COND was last evaluated when $n is false
sub x { if(1)  { 5 } }   # returns 5   — BODY ran, 5 was last evaluated
```

PCL currently generates `(p-if COND (progn BODY))` which, when COND is false, returns
CL `nil` (= Perl `undef`). That is wrong.

---

## 2. Scope: What Needs the Fix and What Doesn't

### Broken (need the fix)

| Form | Why broken |
|------|-----------|
| `if (C) { B }` no else | `(p-if C ...)` → nil when C false |
| `if (C1) { B1 } elsif (C2) { B2 }` no else | Last false condition not captured |
| `unless (C) { B }` no else | `(p-unless C ...)` → nil when C true (body skipped) |
| `EXPR if C` (postfix) | Same `(p-if C EXPR)`, nil when C false |
| `EXPR unless C` (postfix) | Same |
| Nested: last stmt of a branch is itself an if-without-else | Recursive case |

### Already correct (no change needed)

| Form | Why OK |
|------|--------|
| `if (C) { B } else { E }` | Has else; both branches return their last expr |
| `if (...) { ... } elsif (...) { ... } else { E }` | Has final else |
| `while`/`until`/`for`/`foreach` | Already explicitly return `""` |
| Explicit `return EXPR` | Always correct |
| Simple expression as last stmt | CL progn returns it naturally |

---

## 3. Design: Parser-Level Transformation

### Why in the parser (not the runtime)

A runtime macro fix to `p-if` would work for the CL backend only.
Fixing it in the parser emits correct CL for ANY future compiler target
(JavaScript, Python, etc.) because the semantics are expressed in terms of
intermediate CL forms that are universally correct.

### The user's key insight

Inject a single result variable, thread it through every condition and every
branch last-expression, and return it after the if chain:

```perl
# Original:
sub foo {
    if ($a) { compute() }
    elsif ($b) { fallback() }
}

# Conceptual transformation (what the parser emits):
sub foo {
    my $RET;
    if ($RET = $a)   { $RET = compute()  }
    elsif ($RET = $b) { $RET = fallback() }
    return $RET;
}
```

**Why this handles `if/elsif` without else naturally:** each condition
is evaluated in order, and `$RET` is updated on every evaluation.
When no branch matches, `$RET` holds the value of the last condition
that was checked (and was false). That is exactly what Perl returns.

### Generated CL form

```lisp
(let ((--pcl-if-ret--42 nil))
  (p-if (setf --pcl-if-ret--42 $a)
    (progn
      (setf --pcl-if-ret--42 (pl-compute)))
    (p-if (setf --pcl-if-ret--42 $b)
      (progn
        (setf --pcl-if-ret--42 (pl-fallback)))))
  --pcl-if-ret--42)
```

---

## 4. Variable Naming

`$__ret__1` is a **legal Perl identifier** and must not be used (collision risk).
The result variable must be impossible to confuse with user code.

**Rule:** use a CL symbol whose name contains `.` — a character that is not
legal in Perl identifiers after the sigil.

**Format:** `--pcl-if-ret--N` where N is a counter (`$self->{_tail_ret_counter}++`).
Hyphens are not valid Perl identifiers starts after `$`, and the `--` prefix
makes it visually obvious this is a machine-generated internal binding.
This symbol does NOT carry a `$` prefix, further distinguishing it from every
Perl-derived variable in the generated CL (which all start with `$`).

```perl
sub _fresh_ret_var {
    my $self = shift;
    $self->{_tail_ret_counter} //= 0;
    return '--pcl-if-ret--' . $self->{_tail_ret_counter}++;
}
```

A counter is used (not a constant) so that if two if-chains appear at
the same lexical level (rare but possible in eval contexts), they get
distinct names.

---

## 5. When to Apply the Transformation

Only when:
1. `$self->{in_subroutine} > 0` — we are inside a sub body.
   (At file scope, implicit return values don't reach a caller.)
2. The last significant statement of the block is an if/unless compound
   **without a final `else` clause**.

"Last significant statement" = `($block->schildren)[-1]` — PPI's
`schildren()` already skips whitespace and comments.

`do { }` blocks inside a sub already satisfy condition 1 and work
correctly because `_process_block` is called recursively for them.

---

## 6. Detecting "if/unless without else"

```perl
sub _is_if_without_else {
    my ($self, $stmt) = @_;
    return 0 unless ref($stmt) eq 'PPI::Statement::Compound';

    my $first_word = undef;
    my $has_else   = 0;
    for my $child ($stmt->children) {
        my $ref = ref($child);
        next if $ref eq 'PPI::Token::Whitespace';
        if ($ref eq 'PPI::Token::Word') {
            my $w = $child->content;
            $first_word //= $w;
            $has_else = 1 if $w eq 'else';
        }
    }
    return 0 if $has_else;
    return ($first_word//'') eq 'if' || ($first_word//'') eq 'unless';
}
```

For postfix modifiers (`EXPR if C`) the PPI node is `PPI::Statement` (not
Compound), handled separately — see Section 9.

---

## 7. New Methods

### 7.1 `_process_if_tail($stmt, $ret_var)`

Replaces `_process_if_statement` when the if-statement is in tail position.
`$ret_var` is the already-declared CL symbol (no new `let` is opened here).

Algorithm:
1. Collect clauses exactly as `_process_if_statement` does (scan for
   `PPI::Token::Word` if/elsif/else and `PPI::Structure::Block` children).
2. Call `_with_declarations` for variable forward-declaration (unchanged).
3. Call `_generate_if_tail_clauses(\@clauses, $ret_var)`.

### 7.2 `_generate_if_tail_clauses($clauses, $ret_var)`

Recursive, mirrors `_generate_if_clauses` but with ret-var wrapping.

```
_generate_if_tail_clauses([$first, @rest], $ret_var):

  cond_cl = _parse_condition($first->{cond})

  # Wrap condition to capture value:
  wrapped_cond = "(setf $ret_var $cond_cl)"
  # For 'unless', negate after saving:
  if $first->{type} eq 'unless':
      wrapped_cond = "(progn (setf $ret_var $cond_cl) (p-not $ret_var))"

  emit "(p-if $wrapped_cond"
  emit "  (progn"
  _process_block_in_tail_context($first->{block}, $ret_var)
  emit "  )"

  if @rest:
    next = $rest[0]
    if next->{type} eq 'else':
      emit "  ;; else"
      emit "  (progn"
      _process_block_in_tail_context($next->{block}, $ret_var)
      emit "  )"
    else:
      # elsif: recurse (still no new let, same ret_var)
      _generate_if_tail_clauses(\@rest, $ret_var)
  else:
    # No else: DO NOT emit nil — ret_var already holds the last condition value
    # The p-if macro needs a nil placeholder only to balance parens:
    emit "  nil"

  emit ")"
```

**Key difference from `_generate_if_clauses`:** the `nil` in the no-else branch
is still emitted to balance the CL `(if ...)` syntax, but it is never reached
because `$ret_var` is always set before the `(p-if ...)` evaluates it. The
`--pcl-if-ret--N` returned after the `(p-if ...)` carries the correct value.

### 7.3 `_process_block_in_tail_context($block, $ret_var)`

Processes a block's contents where the last statement contributes to `$ret_var`.
Does NOT open a new `let` — that was opened by the caller.

Algorithm:
```
sig_stmts = $block->schildren
all_but_last = sig_stmts[0..-2]
last_stmt    = sig_stmts[-1]   (may be undef for empty block)

# Process all statements before the last normally:
_with_declarations($block, sub {
    for each stmt in all_but_last:
        _process_element(stmt)

    # Handle last statement:
    if not defined last_stmt:
        pass  # empty branch — ret_var holds condition value, that's correct

    elsif _is_if_without_else(last_stmt):
        # Recurse: inner if also captures into ret_var
        _process_if_tail(last_stmt, $ret_var)

    elsif ref(last_stmt) =~ /Statement$/ or ref(last_stmt) eq 'PPI::Statement::Expression':
        # Simple expression: parse to CL, wrap with setf
        cl = _parse_expression([last_stmt->schildren], last_stmt)
        _emit("(setf $ret_var $cl)")

    elsif ref(last_stmt) eq 'PPI::Statement::Variable':
        # my $x = val; — emit normally; return value is already correct
        # for the common case (single assignment), which CL returns as the val.
        # We emit both the declaration and then capture the last expr if possible.
        _process_element(last_stmt)
        # Best-effort: if it's `my $x = EXPR`, also set ret_var to $x
        # (complicated; defer — see Edge Cases section)

    else:
        # Compound non-if, loop, etc.: emit normally
        # ret_var is left at condition value (best effort)
        _process_element(last_stmt)
})
```

---

## 8. Changes to `_process_block`

`_process_block` gains a pre-scan step at the top (after `push_scope`):

```perl
sub _process_block {
    my ($self, $block) = @_;

    $self->environment->push_scope();
    my $start_depth = $self->{_local_let_depth} // 0;

    # --- NEW: tail-position if detection ---
    my $tail_ret_var;
    if ($self->{in_subroutine} > 0) {
        my @sig = $block->schildren;
        if (@sig && $self->_is_if_without_else($sig[-1])) {
            $tail_ret_var = $self->_fresh_ret_var();
        }
    }
    if ($tail_ret_var) {
        $self->_emit("(let (($tail_ret_var nil))");
        $self->indent_level($self->indent_level + 1);
    }
    # --- END NEW ---

    my @children = $block->children;
    my %skip;
    for my $i (0 .. $#children) {
        next if $skip{$i};
        my $child = $children[$i];
        my $ref = ref($child);
        next if $ref eq 'PPI::Token::Whitespace';
        next if $ref eq 'PPI::Token::Comment';

        if ($ref eq 'PPI::Statement::Compound') {
            my ($continue, $trailing) = $self->_find_continue_sibling(\@children, $i, \%skip);
            if ($continue) {
                $self->_process_compound_statement($child, $continue);
                $self->_process_trailing_tokens($trailing) if $trailing && @$trailing;
                next;
            }
        }

        # --- NEW: intercept last statement if it is the tail if ---
        if ($tail_ret_var) {
            my @sig = $block->schildren;
            if ($child == $sig[-1]) {
                $self->_process_if_tail($child, $tail_ret_var);
                next;
            }
        }
        # --- END NEW ---

        $self->_process_element($child);
    }

    # --- NEW: close tail let, emit ret_var as block return value ---
    if ($tail_ret_var) {
        $self->indent_level($self->indent_level - 1);
        $self->_emit("$tail_ret_var)");
    }
    # --- END NEW ---

    # Close let forms opened by local declarations
    my $end_depth = $self->{_local_let_depth} // 0;
    while ($end_depth > $start_depth) {
        $self->indent_level($self->indent_level - 1);
        $self->_emit(")  ;; end local");
        $self->{_local_let_depth}--;
        $end_depth--;
    }

    $self->environment->pop_scope();
}
```

Note: `$child == $sig[-1]` uses reference equality (Perl object identity) to
identify the last child, which is correct since PPI nodes are objects.

---

## 9. Postfix Modifiers (`EXPR if C`, `EXPR unless C`)

Postfix `if`/`unless` are handled in `_process_expression_statement`
(around line 835, the modifier detection block).

Currently: `$cl_code = "(p-if $cond_cl $expr_cl)"`.

With the fix: when this expression statement is the last statement of
a sub block, it needs the same treatment. Since `_process_expression_statement`
doesn't know about tail position, the cleanest approach is:

**In `_process_block`**, the pre-scan checks for postfix if/unless too:

```perl
sub _is_postfix_if_without_else {
    my ($self, $stmt) = @_;
    # PPI gives postfix if as PPI::Statement (not Compound)
    # with a PPI::Token::Word 'if'/'unless' somewhere in the middle
    return 0 unless ref($stmt) eq 'PPI::Statement';
    my @words = grep { ref($_) eq 'PPI::Token::Word' } $stmt->children;
    return grep { $_->content eq 'if' || $_->content eq 'unless' } @words;
}
```

When a postfix `if`/`unless` is the last statement (and `in_subroutine > 0`),
emit the tail form directly rather than delegating to
`_process_expression_statement`:

```lisp
;; EXPR if C  →
(let ((--pcl-if-ret--N nil))
  (if (p-true-p (setf --pcl-if-ret--N C))
    (setf --pcl-if-ret--N EXPR))
  --pcl-if-ret--N)
```

```lisp
;; EXPR unless C  →
(let ((--pcl-if-ret--N nil))
  (unless (p-true-p (setf --pcl-if-ret--N C))
    (setf --pcl-if-ret--N EXPR))
  --pcl-if-ret--N)
```

In both cases: `C` is evaluated and saved; if the body runs, `EXPR` overwrites it.
For `EXPR if C` when C is false: ret = C (the false condition). ✅
For `EXPR unless C` when C is true (body skipped): ret = C (the truthy value). ✅

---

## 10. `unless` Condition Wrapping Detail

`unless (C) { B }` — the PCL parse path goes through `_generate_if_clauses`
which emits `(p-not COND)` to negate for unless (line 1991 in current code).

In the tail version, the save-and-negate must be separated:

```lisp
;; unless ($x) { B }  →
(p-if (progn (setf --ret-- $x) (p-not --ret--))
  (progn
    (setf --ret-- B-last-expr))
  nil)
```

The `progn` evaluates the setf (capturing `$x`) then produces the negated
boolean for the `p-if` condition. When `$x` is truthy, the condition is false
(body skipped), and `--ret--` holds `$x` — the truthy condition value,
which is what Perl returns. ✅

When `$x` is falsy, the condition is true (body runs), and `--ret--` is
overwritten by the last expression in the body. ✅

---

## 11. Full Generated CL Examples

### Simple: `if ($a) { compute() }` as last sub statement

```lisp
(let ((--pcl-if-ret--0 nil))
  (p-if (setf --pcl-if-ret--0 $a)
    (progn
      (setf --pcl-if-ret--0 (pl-compute)))
    nil)
  --pcl-if-ret--0)
```

### Elsif chain: `if ($a) { 1 } elsif ($b) { 2 }` as last sub statement

```lisp
(let ((--pcl-if-ret--1 nil))
  (p-if (setf --pcl-if-ret--1 $a)
    (progn
      (setf --pcl-if-ret--1 1))
    (p-if (setf --pcl-if-ret--1 $b)
      (progn
        (setf --pcl-if-ret--1 2))
      nil))
  --pcl-if-ret--1)
```

Trace ($a=false, $b=false): sets 0→$a, 0→$b, neither branch runs → return $b. ✅
Trace ($a=false, $b=true): sets 0→$a, 0→$b, branch runs → 0 = 2 → return 2. ✅
Trace ($a=true): sets 0→$a, branch runs → 0 = 1 → return 1. ✅

### Nested: `if ($a) { if ($b) { 1 } }` as last sub statement

Outer if is the last stmt of the sub. Inner if is the last stmt of the outer branch.
They share **one** `--pcl-if-ret--N` (no new `let`):

```lisp
(let ((--pcl-if-ret--2 nil))
  (p-if (setf --pcl-if-ret--2 $a)
    (progn
      (p-if (setf --pcl-if-ret--2 $b)
        (progn
          (setf --pcl-if-ret--2 1))
        nil))
    nil)
  --pcl-if-ret--2)
```

Trace ($a=false): ret=0, no branch → return 0 (=$a). ✅
Trace ($a=true, $b=false): ret=$a, then ret=$b, inner branch skipped → return $b. ✅
Trace ($a=true, $b=true): ret=$a, ret=$b, inner branch runs, ret=1 → return 1. ✅

### If with else (NOT transformed):

`if ($a) { 1 } else { 2 }` — has else, processed normally:

```lisp
(p-if $a
  (progn 1)
  (progn 2))
```

Both branches return their last expression. No transformation needed. ✅

---

## 12. Edge Cases

### Empty branch body

```perl
if ($x) { }
```

`_process_block_in_tail_context` finds no significant last statement (empty block).
`--pcl-if-ret--N` holds `$x` (set by the condition capture). Return `$x`. ✅
When $x is false: returns $x. ✅
When $x is true: body runs (no-op), returns $x. ✅ (Perl also returns $x here.)

### Last stmt of branch is a loop

```perl
if ($x) { for my $i (1..5) { something() } }
```

`_process_block_in_tail_context` falls through to the `else` case: emits the loop
normally. The `for` returns `""` via `(return-from block-name "")`. But since
`--pcl-if-ret--N` was set to `$x` (truthy) at the condition step, and the loop
doesn't update it, the returned value will be `$x` (truthy) instead of `""`.

This is technically incorrect (Perl returns `""` from the loop). However, this
pattern — a loop as the last expression of an if branch, with the if being the
last statement of a function, and the caller relying on the return value — is
extremely rare in CPAN code. Document as a known limitation; defer fixing.

**If this matters:** the fix would be to add a special case in
`_process_block_in_tail_context` for loops: emit the loop, then emit
`(setf $ret_var "")`.

### `elsif` chain with a final `else`

`if ($a) { A } elsif ($b) { B } else { C }` — `_is_if_without_else` returns 0
(there IS an else). Processed by normal `_process_if_statement`. ✅
The else branch's last expression is naturally the return value.

### Last stmt of an `else` branch is a bare if-without-else

```perl
if ($a) { 1 } else { if ($b) { 2 } }
```

The outer if HAS an else → no tail transformation at sub level. But the else
branch's last statement is `if ($b) { 2 }` without else. When `_process_block`
is called for the else branch body, it sees `if ($b) { 2 }` as its last
significant statement → opens a new `let ((--pcl-if-ret--M nil))` for that
inner block. ✅ (This is a separate `let` from any outer one, which is correct.)

### `unless` with elsif (unusual but valid Perl)

`unless ($a) { ... } elsif ($b) { ... }` — `_is_if_without_else` returns 1
(no final else). `_generate_if_tail_clauses` handles the unless keyword via
the `unless` condition-negate path. ✅

### `_local_let_depth` interaction

`local` declarations inside an if branch open and close `let` forms tracked by
`_local_let_depth`. The tail `let` opened by `_process_block` is opened BEFORE
the block's statements are processed, so the `_local_let_depth` tracking is
unaffected — it still measures the delta within the block's own processing.
The tail `let` is closed AFTER all `_local_let_depth` cleanup. ✅

### `_with_declarations` interaction

`_with_declarations` (which handles `my` variable forward-declarations) is
called inside `_process_if_tail` / `_generate_if_tail_clauses` in the same way
`_process_if_statement` calls it. No change needed there.

---

## 13. Implementation Order

1. Add `_fresh_ret_var` (trivial)
2. Add `_is_if_without_else` (trivial)
3. Add `_is_postfix_if_without_else` (trivial)
4. Write `_process_block_in_tail_context` (processes all-but-last normally,
   dispatches on last-stmt type)
5. Write `_generate_if_tail_clauses` (mirrors `_generate_if_clauses` with setf wrapping)
6. Write `_process_if_tail` (thin wrapper: collect clauses, call `_generate_if_tail_clauses`)
7. Modify `_process_block` (pre-scan, open let, intercept last child, close let)
8. Handle postfix if/unless in `_process_expression_statement` or by intercepting
   in `_process_block`'s last-child check

---

## 14. Tests to Write First (`Pl/t/bareif-01.t`)

Write these before implementing to confirm the bug and drive the fix:

```perl
# --- Block-form if without else ---
test_cl('sub f { if(0) { 5 } }; print f()',       "0",   'if false cond returns cond');
test_cl('sub f { if("") { 5 } }; print f()',       "",    'if empty-str cond returns cond');
test_cl('sub f { my $n=0; if($n) { 5 } }; print f()', "0", 'if var false returns var');
test_cl('sub f { if(1) { 5 } }; print f()',        "5",   'if true cond returns body last expr');
test_cl('sub f { if(0) { 3; 5 } }; print f()',     "0",   'if false, multi-stmt body');

# --- unless without else ---
test_cl('sub f { unless(1) { 5 } }; print f()',    "1",   'unless true cond returns cond');
test_cl('sub f { unless("") { 5 } }; print f()',   "5",   'unless false cond returns body');

# --- elsif chain without else ---
test_cl('sub f { if(0){1} elsif(0){2} }; print f()', "0", 'elsif chain, all false, returns last cond');
test_cl('sub f { if(0){1} elsif(7){2} }; print f()', "2", 'elsif chain, second branch taken');

# --- Nested if-without-else ---
test_cl('sub f { if(1) { if(0) { 5 } } }; print f()', "0", 'nested if, inner false returns inner cond');
test_cl('sub f { if(1) { if(1) { 5 } } }; print f()', "5", 'nested if, inner true returns body');

# --- Postfix if ---
test_cl('sub f { 5 if 0 }; print f()',   "0",  'postfix if, false cond returns cond');
test_cl('sub f { 5 if 1 }; print f()',   "5",  'postfix if, true cond returns expr');

# --- Postfix unless ---
test_cl('sub f { 5 unless 0 }; print f()', "5",  'postfix unless, false cond → body runs');
test_cl('sub f { 5 unless 1 }; print f()', "1",  'postfix unless, true cond → skipped, return cond');

# --- Inside do {} ---
test_cl('sub f { my $r = do { if(0) { 5 } }; $r }; print f()', "0", 'do block tail if');

# --- Must NOT transform if there is an else ---
test_cl('sub f { if(0) { 5 } else { 9 } }; print f()', "9", 'if-else not affected');

# --- Non-last if not affected ---
test_cl('sub f { if(0){5}; 42 }; print f()', "42", 'non-last if, return is last expr');
```

---

## 15. Affected Perl Tests

- `perl-tests/do.t`: tests 9-10 (known; motivated this fix)
- Latent failures throughout many files where subs have a bare `if` as the last
  statement — these will show up in the sweep after the fix
