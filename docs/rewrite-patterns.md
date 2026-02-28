# Compiler Rewrite Patterns for PCL

## The Problem

Parsing, semantic analysis, and code generation are currently mixed into
the same recursive descent through Parser.pm and ExprToCL.pm. Rewrites
get implemented as either:
- Flags threaded through state (`_last_stmt_block_return`)
- Post-processing on output strings (phase-2 reordering)

Both approaches scale poorly. This document captures better patterns.

---

## Pattern 1: Wrapper functions instead of flags

Instead of setting a flag and checking it later, pass a wrapper callback
to the generator:

```perl
# In _process_sub, for the last statement:
$self->_process_statement($last_stmt, wrapper => \&block_return_wrap);

sub block_return_wrap {
    my ($cl) = @_;
    return "(pl-block-return $cl)";
}
```

The generator applies the wrapper to whatever it emits. No state leaks
between statements, no need to clear flags. The wrapper is a plain
string-in/string-out function.

---

## Pattern 2: Dispatch table instead of if/elsif chains

```perl
my %NODE_GEN = (
    'comma_list' => \&gen_comma_list,
    'if'         => \&gen_if,
    'while'      => \&gen_while,
    # ...
);

sub generate {
    my ($self, $node) = @_;
    my $gen = $NODE_GEN{$node->type} or die "Unknown: " . $node->type;
    return $gen->($self, $node);
}
```

Each generator is a small, self-contained function. Adding a new transform
is adding one table entry and writing one function — no existing code changes.

---

## Pattern 3: Tree annotation pass before codegen

For the implicit return problem: run one pass over the PExpr tree *before*
ExprToCL that annotates nodes with metadata:

```perl
sub annotate_block_returns {
    my ($block_node) = @_;
    my $last = last_expr_child($block_node);
    $last->set_attr('is_block_return', 1);
    # Recurse into if-branches
    if ($last->type eq 'if') {
        annotate_block_returns($_) for $last->branches;
    }
}
```

The code generator just checks `$node->attr('is_block_return')` — no flags,
no threading. The logic of *what counts as a block return* lives entirely in
the annotation pass, not scattered through codegen.

---

## Pattern 4: Explicit named passes (most important)

Make the pipeline explicit rather than implicit:

```perl
my @PASSES = (
    \&pass_annotate_block_returns,
    \&pass_mark_named_unaries,
    \&pass_fix_ppi_negative_numbers,
    # ...
);

for my $pass (@PASSES) {
    $tree = $pass->($tree);
}
```

Each pass is independently testable. You can log the tree before/after each
pass to debug. Adding a new transform is appending one entry to `@PASSES` —
no existing passes change.

---

## Applying This: The Bare-`if` Return Value Problem

The implicit-return problem has a nasty case: `if(COND) { BODY }` with no
`else`. When COND is false, Perl returns the false value of COND itself
(not undef, not nil — the actual `0`, `""`, or `undef` that was evaluated).

```perl
sub x { if(0)    { 5; } }  # returns 0
sub x { if(undef){ 5; } }  # returns undef
sub x { if("")   { 5; } }  # returns ""
sub x { if(1) { print ""; } }  # returns 1 (return value of print)
```

Defined in `perlsub`: *"The return value of a subroutine is the value of
the last expression evaluated by that subroutine."* The condition is always
evaluated, so if it's false, it's the last thing evaluated.

Current CL codegen emits `(if cond (progn body))` which returns NIL on the
false branch — wrong.

**The fix** is a localized tree rewrite in the annotation pass. When an `if`
node is marked `is_block_return` and has no `else` branch, rewrite it:

```
if(COND) { BODY }
→
let(#c = COND) { if(#c) { BODY } else { #c } }
```

This rewrite happens on the tree before codegen. The code generator then
sees a normal `if/else` and handles it uniformly. `if/elsif/else` is
unaffected — every branch is already covered.

---

## What to Avoid

- **Flags that persist across function calls** — implicit coupling, easy to
  forget to clear, create action-at-a-distance bugs.
- **Post-processing generated strings with regexes** — fragile, hard to
  debug, breaks on whitespace variation.
- **Growing existing if/elsif chains** — every new case makes existing cases
  harder to understand.
