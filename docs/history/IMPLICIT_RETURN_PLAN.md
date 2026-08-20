# Plan: Implicit Block Return in List Context

## The Problem

When the last expression in a sub/eval/do block body is a comma list —
or an `if/elsif/else` whose branches return lists — the code generator
produces `(progn "a" "b" "c")` which CL reduces to just `"c"`.

```perl
sub foo { ('a', 'b', 'c') }
my ($a, $b, $c) = foo();   # gets only 'c' in $a, undef in $b/$c
```

`*wantarray*` is already set correctly at the call site (done since day
one). The runtime information is available — we just need to USE it in
the emitted code for the last expression of a block.

## What's Already Fixed

- **`pl-return` (explicit return)**: Now context-aware. `return ('a','b','c')`
  in list context returns a vector; in scalar context returns `'c'`.
  Fixed in `cl/pcl-runtime.lisp`.

## What Still Needs Fixing

Implicit returns — the last expression of a block body when there is no
explicit `return` statement.

## The Approach

Add a new runtime macro `pl-block-return` and make Parser.pm wrap the
last statement of every block body with it.

### Step 1: New runtime macro in `cl/pcl-runtime.lisp`

```lisp
(defmacro pl-block-return (&rest values)
  "Context-aware implicit return for last expression of a block.
   Unlike pl-return, does not (return-from nil ...) — lets CL's normal
   value-passing handle the return from the enclosing block/function."
  (if (= (length values) 1)
      (car values)                        ; single value — pass through
      `(if *wantarray*
           (vector ,@(mapcar (lambda (v) `(pl-return-value ,v)) values))
           (pl-return-value ,(car (last values))))))
```

Export it from the `:pcl` package.

### Step 2: Flag in Parser.pm

Use a flag `$self->{_last_stmt_block_return}` to signal that the next
emitted expression statement should be wrapped in `pl-block-return`.

Set it to `1` just before processing the last child of a block. Clear it
after use (or on any non-expression statement that consumes it).

### Step 3: Places to set the flag

In `Parser.pm`, set the flag before processing the **last statement** of:

1. **`_process_sub` block body** — the last child of the sub's
   `PPI::Structure::Block`.

2. **`parse_block_to_cl_string`** — same: last child of any block parsed
   this way (used for `eval { }`, anonymous subs, `grep`/`map` blocks).

3. **`_process_bare_block`** — bare `{ }` blocks used as `do`-equivalents
   (if they appear as expressions).

### Step 4: In `_process_expression_statement`

Check the flag when emitting a comma-list expression. If set:

```perl
if ($self->{_last_stmt_block_return} && $is_comma_list) {
    $self->{_last_stmt_block_return} = 0;
    # emit (pl-block-return val1 val2 ...) instead of (progn val1 val2 ...)
}
```

### Step 5: Recurse into `if/elsif/else`

When the last statement of a block is an `if/elsif/else`, the flag must
propagate into each branch's last statement. In `_process_if_statement`,
if `$self->{_last_stmt_block_return}` is set when the `if` is entered,
set it again before the last statement of each branch.

This is the recursive case — it naturally handles arbitrarily nested
if/elsif chains whose branches return lists.

### Step 6: `eval { }` call sites

Ensure `(let ((*wantarray* t)) ...)` is wrapped around
`(pl-eval-block (funcall #'--anon-block-N--))` at the call site, just
like it is for sub calls. Check `ExprToCL.pm`'s funcall generator —
verify the `eval`/`do` path sets `*wantarray*` from context.

### Step 7: `do { }` block support

`do BLOCK` is not yet parsed as an expression in PExpr.pm. Add it:
- Detect `Word: do` followed by `Structure::Block`
- Parse block via `parse_block_to_cl_string` (same as eval)
- Generate `(funcall #'--anon-block-N--)` (no `pl-eval-block` wrapper —
  `do` doesn't catch exceptions)
- Wrap call site with `(let ((*wantarray* t)) ...)` in list context

### sort and grep blocks too

### Worse than we thought

Also, it is even worse than we think:

```
perl -MData::Dump=dump -E 'sub x { if(0) { 5;} } $z=x(); say dump $z;'
  ==> 0

perl -MData::Dump=dump -E 'sub x { if(undef) { 5;} } $z=x(); say dump $z;'
  ==> undef

perl -MData::Dump=dump -E 'sub x { if("") { 5;} } $z=x(); say dump $z;'
  ==> ""

perl -MData::Dump=dump -E 'sub x { my $q=0; $w; for(1..3) { $q++; $w=$q*$q; } if (1) { 42; } if(1) { print "";  }  } $z=x(); say dump $z;'
  ==> 1
```

**Where is this defined?** `perlsub`: *"The return value of a subroutine is
the value of the last expression evaluated by that subroutine."* No special
case for `if` — it falls out naturally because the condition is itself an
expression that is always evaluated.

**What is happening in each example:**

- `if(0) { 5; }` — condition `0` is evaluated (false), body skipped. Last
  expression evaluated = `0`. Returns `0`.
- `if(undef) { 5; }` — condition `undef` evaluated (false). Returns `undef`.
- `if("") { 5; }` — condition `""` evaluated (false). Returns `""`.
- `if(1) { print ""; }` — condition `1` (true), body runs, `print ""` returns
  `1` (success). Last expression evaluated = `1`. Returns `1`.

**Implication for the plan:** for an `if` without `else`, the static
"wrap the last statement" approach is insufficient. The return value is
determined at runtime:

- Condition **true**: return last expr of body
- Condition **false**: return the **false value of the condition itself**

Current CL codegen produces `(if cond (progn body))` which returns NIL when
cond is false — wrong. The implicit-return case needs:

```lisp
(let ((#:c cond))
  (if #:c (progn body) #:c))
```

So the condition must be evaluated into a temp variable; that value is used
both for the branch decision and as the return if false.

`if/elsif/else` is unaffected (every branch is covered, no missing-else case).


## Key Insight: What "Last Statement" Means

- `('a', 'b', 'c')` → wrap in `pl-block-return` → context-aware ✓
- `$x` → single value, `pl-block-return` is a no-op ✓
- `if/elsif/else` → recurse into each branch ✓
- `return LIST` → already handled by fixed `pl-return`, skip ✓
- `while`/`for` loop body → loop return values not used, skip ✓

## What to Check for Rollback After Implementation

Run `prove -j8 Pl/t/` before and after. Any test that was written with
an explicit `return` to work around missing implicit-return behaviour
should still pass (explicit `return` goes through `pl-return`, which is
already fixed). Nothing to roll back in the PCL test suite is expected.

Check `docs/wantarray-context.md` — some of its "deferred" items may
now be resolved; update accordingly.

## Tests That Should Pass After This

In `perl-tests/`:

- `list.t` tests 30–38: `($a,$b,$c) = do { if (...) { ('a','b','c') } }`
- `list.t` test 39: `@a = ($x || (1,2,3))`
- Any test using a sub that implicitly returns a list without `return`
- `sub.t` and similar files testing return values
