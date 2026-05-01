# Two-Phase Block Compiler — Analysis Pass + Scoped Codegen

## Context

PCL currently compiles a block (sub body, bare block, etc.) in a single pass:
`_with_declarations` scans for all `my` declarations via `_find_all_declarations`,
hoists them all to the top as one flat `(let (...) ...)`, then emits code. Two problems:

1. **Scoping bug** — a `my $a` mid-function shadows the package global `$a` for the
   entire function, even before the `my` appears. Root of ~120 substr.t failures.
   See `docs/let-scoping-problem.md`.

2. **Over-boxing** — every `my` scalar gets `(make-p-box nil)` even when it only holds
   integers (loop counters, indices). Plain CL integers would work and be faster.

Fix: split block compilation into two phases.

---

## Phase 1 — `Pl/BlockAnalyzer.pm` (new module, no codegen)

### Where the two analysis sub-passes live

- **Declaration collection** (`_collect_declarations`): purely syntactic, done at the
  **PPI token level**. Finds `my`/`our`/`state`/`local` keywords and their variable
  names, recording the statement index at which each appears. PExpr is not involved.

- **Usage/type analysis** (`_collect_usages`): done at the **OpcodeTree level**, after
  PExpr has parsed each statement expression. The OpcodeTree already encodes operator
  precedence correctly (e.g. `$a + $b * $c` produces a tree where `$b` and `$c` are
  children of `*`, not siblings of `+`). Walking it gives accurate context without
  re-implementing PExpr's precedence logic. Do NOT classify context from PPI sibling
  tokens — that approach is both fragile and redundant.

- **Closure-capture detection** (`_find_closure_captures`): PPI-level scan for `sub { }`
  bodies within the block, collecting all symbols they reference.

### Signature

```perl
package Pl::BlockAnalyzer;
use v5.30; use strict; use warnings; use Moo;

# Called once per block before any codegen.
# $block          — PPI element (PPI::Structure::Block or arrayref of statements)
# $outer          — hashref: varname => { type => 'my'|'our'|'state'|'constant',
#                                         cl_name => '...' }
#                   Built by Parser.pm from _let_bound_vars + environment.
# $pexpr_factory  — optional coderef ($stmt) => Pl::PExpr-or-undef
#                   If omitted, usage analysis is skipped (type_hint stays 'any').
# Returns an analysis hashref (see "Output" below).
sub analyze {
    my ($class, $block, $outer, $pexpr_factory) = @_;
    $outer //= {};
    my @stmts = _stmts_of($block);
    my $self  = bless { outer => $outer }, $class;

    my $decls      = $self->_collect_declarations(\@stmts);
    my %in_block   = map { $_ => 1 } map { @{$_->{vars}} } @$decls;
    my $captured   = $self->_find_closure_captures(\@stmts, \%in_block);
    my $usages     = $pexpr_factory
                     ? $self->_collect_usages(\@stmts, \%in_block, $pexpr_factory)
                     : {};
    my $outer_refs = $self->_find_outer_refs($usages, \%in_block);
    my $vars       = $self->_build_var_map($decls, $usages, $captured,
                                           $outer_refs, $outer);
    return {
        declarations => $decls,
        vars         => $vars,
        outer_refs   => $outer_refs,
    };
}
```

### Output data structure

```perl
{
  # My/our/state declarations in this block, in source order.
  # stmt_idx is 0-based index into the direct-child statements of the block.
  declarations => [
    {
      decl_type => 'my',            # 'my'|'our'|'state'|'local'
      vars      => ['$x', '@arr'],  # all variable names in this decl statement
      stmt_idx  => 3,               # which statement (0-based) first introduces them
      ppi_stmt  => $ppi_obj,        # the PPI statement node itself
    },
    ...
  ],

  # Per-variable info, keyed by Perl name ('$x', '@arr', '%h')
  vars => {
    '$i' => {
      sigil         => '$',
      scope         => 'local',    # 'local' = declared in this block
                                   # 'outer' = comes from enclosing scope
      decl_type     => 'my',       # undef if scope=='outer'
      decl_idx      => 0,          # index into declarations[] (undef if outer)
      decl_stmt_idx => 3,          # stmt_idx where this var first appears as 'my'
      captured      => 0,          # 1 if referenced inside an inner anonymous sub
      type_hint     => 'fixnum',   # 'fixnum'|'float'|'string'|'ref'|'any'
      usages        => [
        { stmt_idx => 0, role => 'read',  context => 'arith' },
        { stmt_idx => 0, role => 'write', context => 'arith' },
        ...
      ],
    },
    ...
  },

  # Variables used in this block but not declared here
  outer_refs => {
    '$a' => { outer_type => 'my'|'our'|'constant'|'unknown' },
  },
}
```

### `_stmts_of` helper

```perl
sub _stmts_of {
    my ($block) = @_;
    if (ref($block) eq 'ARRAY') {
        return grep { ref($_) && ref($_) !~ /Whitespace|Comment/ } @$block;
    }
    return grep { ref($_) && ref($_) !~ /Whitespace|Comment/ } $block->children;
}
```

### `_collect_declarations` (PPI-level)

Walk statements in order. For each `PPI::Statement::Variable`, detect the declarator
keyword and collect variable names with the statement index.

Do NOT recurse into: named sub definitions (`PPI::Statement::Sub` with a name token),
`BEGIN`/`END` blocks (`PPI::Statement::Scheduled`), or anonymous sub bodies (a
`PPI::Structure::Block` immediately preceded by the `sub` keyword). For bare blocks
(a `PPI::Structure::Block` with no preceding keyword), recurse but only keep `state`
declarations — same rules as the existing `_find_all_declarations`.

```perl
sub _collect_declarations {
    my ($self, $stmts) = @_;
    my @result;
    my $idx = 0;
    for my $stmt (@$stmts) {
        my $r = ref($stmt);
        if ($r eq 'PPI::Statement::Variable') {
            my @children = grep { ref($_) !~ /Whitespace/ } $stmt->children;
            my $decl_type = ref($children[0]) eq 'PPI::Token::Word'
                            ? $children[0]->content : '';
            next unless $decl_type =~ /^(my|our|state|local)$/;
            my @vars;
            for my $c (@children[1..$#children]) {
                my $cr = ref($c);
                if ($cr eq 'PPI::Token::Symbol') {
                    push @vars, $c->content; last;
                } elsif ($cr eq 'PPI::Structure::List') {
                    push @vars, map { $_->content }
                                grep { ref($_) eq 'PPI::Token::Symbol' }
                                $c->find('PPI::Token::Symbol') || [];
                    last;
                }
            }
            push @result, {
                decl_type => $decl_type,
                vars      => \@vars,
                stmt_idx  => $idx,
                ppi_stmt  => $stmt,
            } if @vars;
        }
        elsif ($stmt->can('children')) {
            for my $child ($stmt->children) {
                my $cr = ref($child);
                next unless $cr && $child->can('children');
                next if $cr eq 'PPI::Statement::Sub' || $cr eq 'PPI::Statement::Scheduled';
                if ($cr eq 'PPI::Structure::Block') {
                    my $prev = $child->sprevious_sibling;
                    next if $prev && ref($prev) eq 'PPI::Token::Word'
                            && $prev->content eq 'sub';
                    my $is_bare = !$prev;
                    my $inner   = $self->_collect_declarations([_stmts_of($child)]);
                    if ($is_bare) {
                        push @result, grep { $_->{decl_type} eq 'state' } @$inner;
                    } else {
                        push @result, map { { %$_, stmt_idx => $idx } } @$inner;
                    }
                }
            }
        }
        $idx++;
    }
    return \@result;
}
```

### `_collect_usages` (OpcodeTree-level)

`BlockAnalyzer` receives a `$pexpr_factory` coderef from `Parser.pm`. The factory takes a
PPI statement element and returns a freshly-parsed `Pl::PExpr` object (the same call
Parser.pm makes during normal codegen), or `undef` for non-expression statements.

`BlockAnalyzer` calls the factory once per statement, then walks the resulting OpcodeTree
with `_walk_tree`. This keeps all PExpr knowledge inside Parser.pm; BlockAnalyzer only
needs to know the OpcodeTree's `node_data` / `children_ids` API.

```perl
# In Parser.pm — factory passed to BlockAnalyzer::analyze
my $pexpr_factory = sub {
    my ($stmt) = @_;
    return undef unless $stmt->isa('PPI::Statement');
    my $expr_o = Pl::PExpr->new(env => $self->environment);
    eval { $expr_o->parse_expr_to_tree($stmt->children) };
    return $@ ? undef : $expr_o;
};
```

```perl
sub _collect_usages {
    my ($self, $stmts, $in_block, $pexpr_factory) = @_;
    my %usages;
    my $idx = 0;
    for my $stmt (@$stmts) {
        my $expr_o = $pexpr_factory->($stmt);
        if ($expr_o) {
            $self->_walk_tree($expr_o, $expr_o->node_top,
                              undef, 0, $idx, \%usages);
        }
        $idx++;
    }
    return \%usages;
}
```

### `_walk_tree` — OpcodeTree walker for usage collection

Internal operator nodes have `{type => 'OP', ...}` as their node data; leaf nodes are
the original PPI tokens. Walking down from a `+` parent to a `Symbol` child gives us
the arithmetic context without any guessing.

```perl
my %ARITH_OPS  = map { $_ => 1 } qw(+ - * / % ** ++ -- < > <= >= == != <=>
                                      += -= *= /= %= **=);
my %STRING_OPS = map { $_ => 1 } qw(. x eq ne lt gt le ge =~ !~ .= x=);
my %BOOL_OPS   = map { $_ => 1 } qw(&& || // ! not and or);
my %REF_OPS    = map { $_ => 1 } qw(\\ ->);

# op_type:   the operator of the PARENT node, or undef at the root.
# child_pos: 0 = left/first child, 1 = right/second (matters for '=' role).
sub _walk_tree {
    my ($self, $tree, $node_id, $parent_op, $child_pos, $stmt_idx, $usages) = @_;

    my $node = $tree->node_data($node_id);
    my $kids = $tree->children_ids($node_id);

    if (ref($node) && $node->isa('PPI::Token::Symbol')) {
        my $var  = $node->content;
        my $role = _role_from_parent($parent_op, $child_pos);
        my $ctx  = _context_from_op($parent_op);
        push @{$usages->{$var}}, {
            stmt_idx => $stmt_idx,
            role     => $role,
            context  => $ctx,
        };
        return;
    }

    my $op = ref($node) eq 'HASH' ? ($node->{type} // '') : '';
    for my $i (0 .. $#$kids) {
        $self->_walk_tree($tree, $kids->[$i], $op, $i, $stmt_idx, $usages);
    }
}

sub _role_from_parent {
    my ($op, $pos) = @_;
    return 'read' unless defined $op;
    return $pos == 0 ? 'write' : 'read'  if $op eq '=';
    return $pos == 0 ? 'both'  : 'read'  if $op =~ /^.+=$/;
    return 'both' if $op =~ /^\+\+$|^--$/;
    return 'read';
}

sub _context_from_op {
    my ($op) = @_;
    return 'unknown' unless defined $op;
    return 'arith'   if $ARITH_OPS{$op};
    return 'string'  if $STRING_OPS{$op};
    return 'bool'    if $BOOL_OPS{$op};
    return 'ref'     if $REF_OPS{$op};
    return 'call'    if $op eq 'funcall' || $op eq 'method_call';
    return 'unknown';
}
```

### `_find_closure_captures` (PPI-level)

```perl
sub _find_closure_captures {
    my ($self, $stmts, $in_block) = @_;
    my %captured;
    for my $stmt (@$stmts) {
        my $sub_kws = $stmt->find(
            sub { $_[1]->isa('PPI::Token::Word') && $_[1]->content eq 'sub' }
        ) || [];
        for my $kw (@$sub_kws) {
            my $first = $kw->snext_sibling;
            next if $first && $first->isa('PPI::Token::Word');  # named sub
            my $sib = $kw->next_sibling;
            $sib = $sib->next_sibling while $sib && !$sib->isa('PPI::Structure::Block');
            next unless $sib;
            my $syms = $sib->find('PPI::Token::Symbol') || [];
            $captured{$_->content} = 1
                for grep { $in_block->{$_->content} } @$syms;
        }
    }
    return \%captured;
}
```

### `_find_outer_refs`

```perl
sub _find_outer_refs {
    my ($self, $usages, $in_block) = @_;
    my %outer;
    for my $var (keys %$usages) {
        next if $in_block->{$var};
        my $otype = $self->{outer}{$var} ? $self->{outer}{$var}{type} : 'unknown';
        $outer{$var} = { outer_type => $otype };
    }
    return \%outer;
}
```

### `_build_var_map` and type inference

```perl
sub _build_var_map {
    my ($self, $decls, $usages, $captured, $outer_refs, $outer) = @_;
    my %vars;

    my $didx = 0;
    for my $d (@$decls) {
        for my $var (@{$d->{vars}}) {
            $vars{$var} = {
                sigil         => substr($var, 0, 1),
                scope         => 'local',
                decl_type     => $d->{decl_type},
                decl_idx      => $didx,
                decl_stmt_idx => $d->{stmt_idx},
                captured      => $captured->{$var} ? 1 : 0,
                type_hint     => 'any',
                usages        => $usages->{$var} // [],
            };
        }
        $didx++;
    }

    for my $var (keys %$outer_refs) {
        $vars{$var} = {
            sigil         => substr($var, 0, 1),
            scope         => 'outer',
            decl_type     => undef,
            decl_idx      => undef,
            decl_stmt_idx => undef,
            captured      => 0,
            type_hint     => 'any',
            usages        => $usages->{$var} // [],
        };
    }

    # Type inference for scalar locals only; skip captured vars (can't unbox safely)
    for my $var (keys %vars) {
        my $info = $vars{$var};
        next unless $info->{sigil} eq '$' && $info->{scope} eq 'local';
        next if $info->{captured};
        $info->{type_hint} = _infer_type($info->{usages});
    }

    return \%vars;
}

sub _infer_type {
    my ($usages) = @_;
    return 'any' unless @$usages;

    my %ctxs;
    $ctxs{$_->{context}}++ for @$usages;

    return 'any' if $ctxs{unknown} || $ctxs{call} || $ctxs{ref};

    my ($has_arith, $has_string) = ($ctxs{arith} || 0, $ctxs{string} || 0);
    return 'any'    if $has_arith && $has_string;
    return 'fixnum' if $has_arith && !$has_string;
    return 'string' if $has_string && !$has_arith;
    return 'any';
}
```

---

## Phase 2a — Scoping Fix in `Parser.pm`

### New helper: `_current_outer_scope`

```perl
sub _current_outer_scope {
    my ($self) = @_;
    my %outer;
    for my $v (keys %{$self->{_let_bound_vars} // {}}) {
        $outer{$v} = { type => 'my', cl_name => $v };
    }
    for my $v (keys %{$self->environment->state_var_renames // {}}) {
        $outer{$v} = { type => 'state',
                       cl_name => $self->environment->state_var_renames->{$v} };
    }
    for my $c (keys %{$self->environment->constants // {}}) {
        $outer{$c} = { type => 'constant', cl_name => '+' . $c . '+' };
    }
    for my $v (keys %{$self->environment->our_vars // {}}) {
        $outer{$v} = { type => 'our', cl_name => $v };
    }
    return \%outer;
}
```

### Rewrite `_with_declarations` (line 3283)

```perl
sub _with_declarations {
    my ($self, $elements, $emit_body) = @_;
    require Pl::BlockAnalyzer;
    my $analysis = Pl::BlockAnalyzer->analyze(
        $elements,
        $self->_current_outer_scope(),
        # Pass pexpr_factory only when type inference is enabled (Step 6).
        # For Steps 2–5, omit it so type_hint stays 'any' everywhere.
    );
    $self->_emit_scoped_block($analysis, $emit_body);
}
```

### New `_emit_scoped_block`

The scoping fix: emit nested `let` forms at the exact statement indices where `my`
declarations appear, rather than hoisting everything to the top.

```perl
sub _emit_scoped_block {
    my ($self, $analysis, $emit_body) = @_;

    my $decls      = $analysis->{declarations};
    my $vars       = $analysis->{vars};
    my $state_vars = $self->{_current_state_vars} // {};

    # Build stmt_idx → [decl entries] map for 'my' vars that aren't state vars.
    my %decls_at;
    my %seen_var;
    for my $d (@$decls) {
        next if $d->{decl_type} ne 'my';
        my @new_vars = grep { !$seen_var{$_}++ && !$state_vars->{$_} } @{$d->{vars}};
        push @{$decls_at{$d->{stmt_idx}}}, { %$d, vars => \@new_vars } if @new_vars;
    }

    unless (%decls_at) { $emit_body->(); return; }

    # Closure-capture rename computation (same __lex__N / __case__N logic as before).
    my (%new_renames, %old_renames, %cl_sym_seen);
    my $existing = $self->environment->state_var_renames // {};
    for my $d (@$decls) {
        next if $d->{decl_type} ne 'my';
        for my $var (@{$d->{vars}}) {
            next if $state_vars->{$var} || $seen_var{$var}++;
            my $vinfo = $vars->{$var} // {};
            if ($vinfo->{captured}) {
                my ($sigil, $bare) = ($var =~ /^([\$\@\%])(.+)$/);
                ($sigil, $bare) = ('$', $var) unless defined $bare;
                (my $slug = $bare) =~ s/[^a-zA-Z0-9]/_/g;
                my $u = sprintf('%s%s__lex__%d', $sigil, $slug, ++$lex_var_counter);
                $new_renames{$var} = $u;
                $old_renames{$var} = $existing->{$var};
            }
            my $cl_name = $new_renames{$var} // $var;
            my $lc = lc($cl_name);
            if ($cl_sym_seen{$lc}) {
                my ($sigil, $bare) = ($cl_name =~ /^([\$\@\%])(.+)$/);
                ($sigil, $bare) = ('$', $cl_name) unless defined $bare;
                (my $slug = $bare) =~ s/[^a-zA-Z0-9]/_/g;
                my $r = sprintf('%s%s__case__%d', $sigil, $slug, ++$lex_var_counter);
                $new_renames{$var} = $r;
                $cl_sym_seen{lc($r)} = $var;
            } else {
                $cl_sym_seen{$lc} = $var;
            }
        }
    }

    # Apply renames to environment
    my ($saved_env_renames, $saved_scope_renames);
    $saved_scope_renames = $self->{_current_scope_new_renames};
    if (%new_renames) {
        $saved_env_renames = $self->environment->state_var_renames // {};
        $self->environment->state_var_renames({ %$saved_env_renames, %new_renames });
        $self->{_current_scope_new_renames} = \%new_renames;
        $self->{_current_scope_old_renames} = \%old_renames;
    }

    # Per-statement hook: fires BEFORE each statement in _process_block,
    # opens a (let ...) whenever a 'my' declaration appears at this stmt_idx.
    my $old_let_vars = $self->{_let_bound_vars};
    my %all_let_vars = %{$old_let_vars // {}};
    my @pending_closes;
    my $stmt_counter = 0;

    my $saved_hook = $self->{_stmt_pre_hook};
    $self->{_stmt_pre_hook} = sub {
        my ($parser) = @_;
        my $idx = $stmt_counter++;
        return unless $decls_at{$idx};
        for my $d (@{$decls_at{$idx}}) {
            my @bindings;
            for my $var (@{$d->{vars}}) {
                my $lv    = $new_renames{$var} // $var;
                my $sigil = substr($lv, 0, 1);
                my $hint  = ($vars->{$var} // {})->{type_hint} // 'any';
                push @bindings, "($lv " . _let_init($sigil, $hint) . ")";
                $all_let_vars{$lv} = 1;
            }
            $parser->_emit("(let (" . join(" ", @bindings) . ")");
            $parser->indent_level($parser->indent_level + 1);
            push @pending_closes, 1;
        }
    };
    $self->{_let_bound_vars} = \%all_let_vars;

    $emit_body->();

    for (@pending_closes) {
        $self->indent_level($self->indent_level - 1);
        $self->_emit(")");
    }

    # Restore
    $self->{_let_bound_vars}          = $old_let_vars;
    $self->{_stmt_pre_hook}           = $saved_hook;
    if (%new_renames) {
        $self->environment->state_var_renames($saved_env_renames);
        $self->{_current_scope_new_renames} = $saved_scope_renames;
        delete $self->{_current_scope_old_renames};
    }
}

sub _let_init {
    my ($sigil, $hint) = @_;
    return '(make-array 0 :adjustable t :fill-pointer 0)' if $sigil eq '@';
    return "(make-hash-table :test #'equal)"              if $sigil eq '%';
    return '0'              if $hint eq 'fixnum';
    return '0.0d0'          if $hint eq 'float';
    return '""'             if $hint eq 'string';
    return '(make-p-box nil)';
}
```

### Wire the per-statement hook into `_process_block`

Add one line inside the statement loop (wherever whitespace/comments are already skipped):

```perl
for my $child ($block->children) {
    next if ...;  # existing whitespace/comment skip
    $self->{_stmt_pre_hook}->($self) if $self->{_stmt_pre_hook};
    $self->_process_element($child);
}
```

**Critical**: `$stmt_counter` in the hook and `_process_block`'s iteration must skip
exactly the same elements (both skip whitespace and comments), otherwise stmt indices
will be off by one.

---

## Phase 2b — Unboxed Codegen for `fixnum` Variables

When `type_hint eq 'fixnum'`, `_let_init` already emits `0` (plain CL integer) for the
`let` binding. The codegen for arithmetic operations on that variable must also avoid
boxing.

### Propagate fixnum set into ExprToCL

In `_emit_scoped_block`, before calling `$emit_body->()`:

```perl
my %fixnum_vars = map { $_ => 1 }
                  grep { ($vars->{$_}{type_hint} // '') eq 'fixnum' }
                  keys %$vars;
local $self->environment->{_fixnum_vars} = \%fixnum_vars;
```

### `gen_binary_op` shortcut in `ExprToCL.pm`

```perl
sub gen_binary_op {
    my ($self, $op, $kids, $node_id) = @_;
    ...
    # Fixnum shortcut: emit native CL arithmetic when both operands are fixnum.
    if ($self->_both_fixnum($kids)) {
        my %NATIVE = ('+' => '+', '-' => '-', '*' => '*',
                      '<' => '<', '>' => '>', '<=' => '<=', '>=' => '>=',
                      '==' => '=', '!=' => '/=');
        if (my $native = $NATIVE{$op}) {
            my $left  = $self->gen_node($kids->[0]);
            my $right = $self->gen_node($kids->[1]);
            return "($native $left $right)";
        }
    }
    ...
}

sub _both_fixnum {
    my ($self, $kids) = @_;
    my $fvs = $self->environment->{_fixnum_vars} // {};
    for my $kid_id (@$kids) {
        my $node = $self->expr_o->get_a_node($kid_id);
        next if ref($node) =~ /Number/ && $node->content =~ /^\d+$/;
        if (ref($node) eq 'PPI::Token::Symbol') {
            my $n = $self->rename($node->content);
            next if $fvs->{$n} || $fvs->{$node->content};
        }
        return 0;
    }
    return 1;
}
```

### `++` / `--` on fixnum vars

```perl
# In the incr/decr generator in ExprToCL.pm:
if ($fvs->{$cl_var_name}) {
    return $is_pre  ? "(incf $cl_var_name)"
                    : "(prog1 $cl_var_name (incf $cl_var_name))";
}
```

---

## Implementation Sequence

| Step | What | Verification |
|------|------|-------------|
| 1 | Create `Pl/BlockAnalyzer.pm` with all analysis methods. No Parser.pm changes. | `Pl/t/block-analyzer-01.t` (new unit tests for the data structure) |
| 2 | Add `_current_outer_scope` to Parser.pm. Wire `_with_declarations` to call analyzer but keep the OLD flat-let emit path — just verify analysis returns the right structure. | All existing tests still pass |
| 3 | Add `_stmt_pre_hook` slot to Parser.pm. Implement `_emit_scoped_block`. Patch `_process_block` to call the hook. Switch `_with_declarations` to use `_emit_scoped_block`. | `prove -j8 Pl/t/`; `./runt substr` pass count improves |
| 4 | Validate substr.t tests 1–10 recover. Run full sweep. | `perl sweep-perl-tests.pl --jobs 8` — fully-passing count must not drop |
| 5 | Replace `_vars_referenced_in_closures` calls with `$analysis->{vars}{$v}{captured}` lookups. | closure.t must not regress |
| 6 | Pass `$pexpr_factory` into `BlockAnalyzer::analyze`. Propagate `_fixnum_vars` into ExprToCL. Emit native CL ops for fixnum vars. **See Step 6 gap note below before starting.** | for.t / while.t; inspect generated CL manually |

---

## Files

| File | Action |
|------|--------|
| `Pl/BlockAnalyzer.pm` | **New** — ~300 lines |
| `Pl/t/block-analyzer-01.t` | **New** — unit tests for the data structure |
| `Pl/Parser.pm` | Replace `_with_declarations` (line 3283); add `_emit_scoped_block`, `_current_outer_scope`, `_let_init`; patch `_process_block` to call `_stmt_pre_hook` |
| `Pl/ExprToCL.pm` | Step 6 only: `_both_fixnum`, fixnum shortcut in `gen_binary_op`, `incf`/`decf` for `++`/`--` on fixnum vars |

---

## Key Invariants to Preserve

- **`p-my-=` vs `p-scalar-=`**: `p-scalar-=` has a `proclaim-special` side-effect that
  converts a variable to a CL dynamic special, breaking closures. `p-my-=` (box-set)
  skips this. `_let_bound_vars` tracks which names are let-bound; `_emit_scoped_block`
  must keep it accurate as each new `let` is opened and restored when it closes.
- **`state` vars**: still handled by `_process_sub_statement` before `_with_declarations`
  is called. `_emit_scoped_block` must skip them via the `$state_vars` guard.
- **`stmt_counter` alignment**: the hook's counter and `_process_block`'s iteration must
  skip identical elements. Any mismatch shifts all declaration indices by one.
- **Scoping fix only (Steps 1–5)**: `type_hint` stays `'any'` everywhere → `_let_init`
  always returns `(make-p-box nil)` for scalars → no behaviour change for boxing.
  This makes the scoping fix a safe prerequisite for the unboxing optimisation.

### Step 6 gap — `box-set` conflict with fixnum init

When `type_hint eq 'fixnum'`, `_let_init` returns `0` (plain CL integer) instead of
`(make-p-box nil)`. This creates a conflict in `_process_variable_statement`: the
existing code emits `(p-my-= $i RHS)`, which calls `box-set` on the let-bound variable.
`box-set` requires its first argument to be a `p-box` struct — passing a plain CL integer
blows up at runtime.

Two resolution options:

**Option A (preferred):** Teach `_process_variable_statement` to check whether the
variable is in the environment's `_fixnum_vars` set and, if so, emit `(setf $i RHS)`
instead of `(p-my-= $i RHS)`. The `(setf ...)` form works directly on plain CL
integers. Similarly, in `_emit_scoped_block`, the `let` binding already initialises the
variable to `0`, so no `p-my-=` call is needed for the declaration itself.

**Option B (safe):** Do not enable `_let_init` fixnum optimisation in Step 6 at all.
Keep `_let_init` returning `(make-p-box nil)` for scalars regardless of `type_hint`.
The type-inference machinery (PExpr factory, `_collect_usages`, `_build_var_map`) can
still be wired up and verified, but the `_fixnum_vars` propagation into ExprToCL is
deferred until the `p-my-=` / `setf` dispatch issue is resolved cleanly.

**Recommendation:** Implement Option B first (less risk, verifies the analysis pipeline
end-to-end), then circle back with Option A once the scoping fix is proven stable.

Do NOT start Step 6 without resolving this or the test suite will crash on every
function that declares a `my` integer variable.

## PExpr Objects: Save and Reuse

`Pl::PExpr` and its `OpcodeTree` are plain Perl objects — they live as long as something
holds a reference to them. There is no reason they must be discarded after the analysis
pass. The analysis pass should save them:

```perl
$analysis->{pexpr_cache}{$stmt_idx} = $expr_o;
```

The codegen pass then pulls the cached object instead of calling PExpr again. ExprToCL
annotates the OpcodeTree in-place via `set_metadata`, and reads those annotations back
during `generate()`. This is exactly what `ast-annotation-plan.md` requires: one
canonical OpcodeTree per statement, annotated once, consumed once.

**Benefits:**
- Each statement is PExpr'd once, not twice
- OpcodeTree metadata (`set_metadata`/`get_metadata`) survives into codegen — the
  annotation plan's `closure_captured`, `unboxable`, `lvalue` etc. all work correctly
- No counter-alignment problem for the scoping fix either: the cached `ppi_stmt`
  references are the same objects `_process_block` iterates, so identity comparison
  works directly

**One real caveat:** if something inside the block changes `Pl::Environment` mid-block
(a `use constant`, an `our` declaration, a `package` switch), the cached OpcodeTree was
built against the pre-change environment and may have stale renames. In practice this is
rare inside sub bodies. A conservative fallback: if a statement is a
`PPI::Statement::Include` or `PPI::Statement::Package`, skip the cache and re-parse it
at codegen time.

---

## VarAnnotator Sketch

The scope-stack walk is simpler than it sounds once you know the actual OpcodeTree node
types. Here is a concrete sketch.

### What the OpcodeTree actually contains

PExpr's `extract_declarations` **strips** the `my`/`our`/`state`/`local` keywords before
building the tree. So in the OpcodeTree, `my $x = 1` leaves just a `PPI::Token::Symbol`
for `$x` as a leaf — there is no `my_decl` node type. The declarations are recorded
separately in `$expr_o->declarations` as `[{ type => 'my', var => '$x' }, ...]`.

Anonymous subs (`sub { }`) become `anon_sub` nodes. `do { }` blocks become
`inline_lambda` nodes. Both are internal OpcodeTree nodes with `{type => 'anon_sub'}` or
`{type => 'inline_lambda'}` as their node data.

### The scope stack

Each frame is just a hashref: `{ vars => {varname => decl_node_id}, is_sub => 0|1 }`.

- `is_sub => 1`: this frame was pushed when entering an `anon_sub` — a closure boundary.
- `is_sub => 0`: this frame was pushed when entering an `inline_lambda` (do-block) or a
  bare block — a new lexical scope but NOT a closure boundary.

Detecting closure capture is one loop: walk the stack outward from innermost; if we find
the variable's declaration and crossed at least one `is_sub => 1` frame on the way, it
is a closure capture.

### Full VarAnnotator sketch (~80 lines)

```perl
package Pl::VarAnnotator;
use v5.30; use strict; use warnings; use Moo;

has expr_o      => (is => 'ro', required => 1);  # Pl::PExpr with OpcodeTree
has outer_scope => (is => 'ro', default => sub { {} });
# outer_scope: hashref varname => { type=>'my'|'our'|..., decl_node_id=>... }
# Built from BlockAnalyzer's analysis for the enclosing block.

sub annotate {
    my ($self, $root_id) = @_;
    my $tree = $self->expr_o;

    # Seed the initial scope frame with declarations from this expression
    # (extract_declarations already stripped 'my' etc. from the tree and put
    # them in $expr_o->declarations)
    my %frame_vars;
    for my $d (@{$tree->declarations}) {
        $frame_vars{$d->{var}} = { type => $d->{type}, decl_node_id => undef };
    }

    # Also include outer_scope as the bottom frame
    my @stack = (
        { vars => $self->outer_scope, is_sub => 0 },  # enclosing block scope
        { vars => \%frame_vars,       is_sub => 0 },  # this expression's my-vars
    );

    $self->_walk($root_id, \@stack);
}

sub _walk {
    my ($self, $node_id, $stack) = @_;
    my $tree = $self->expr_o;
    my $node = $tree->node_data($node_id);
    my $kids = $tree->children_ids($node_id);

    # Variable leaf: classify and check for closure capture
    if (ref($node) && $node->isa('PPI::Token::Symbol')) {
        my $var = $node->content;
        my ($decl, $captured) = _lookup($var, $stack);
        $tree->set_metadata($node_id, 'var_kind',      $decl ? $decl->{type} : 'package');
        $tree->set_metadata($node_id, 'closure_captured', 1) if $captured;
        if ($captured && defined $decl->{decl_node_id}) {
            $tree->set_metadata($decl->{decl_node_id}, 'closure_captured', 1);
        }
        return;
    }

    my $type = ref($node) eq 'HASH' ? ($node->{type} // '') : '';

    if ($type eq 'anon_sub') {
        # Closure boundary: new scope frame with is_sub => 1
        push @$stack, { vars => {}, is_sub => 1 };
        $self->_walk($_, $stack) for @$kids;
        pop @$stack;
        return;
    }

    if ($type eq 'inline_lambda') {
        # do { } block: new scope but NOT a closure boundary
        push @$stack, { vars => {}, is_sub => 0 };
        $self->_walk($_, $stack) for @$kids;
        pop @$stack;
        return;
    }

    # All other nodes: recurse into children
    $self->_walk($_, $stack) for @$kids;
}

# Search stack from innermost to outermost.
# Returns ($decl_hashref_or_undef, $was_captured).
sub _lookup {
    my ($var, $stack) = @_;
    my $crossed_sub = 0;
    for my $frame (reverse @$stack) {
        $crossed_sub ||= $frame->{is_sub};
        if (exists $frame->{vars}{$var}) {
            return ($frame->{vars}{$var}, $crossed_sub && !$frame->{is_sub});
        }
    }
    return (undef, 0);
}
```

That is the entire core. The scope stack is a list of plain hashrefs. `_lookup` is five
lines. `_walk` has three cases: variable leaf, `anon_sub`, everything else.

### Why I overcounted the effort earlier

The 4–5 session estimate was for the **full** `ast-annotation-plan.md` VarAnnotator:
`var_kind` on every leaf, `var_decl_node` pointing to declaration nodes, `loop_var`,
`sort_special`, `unboxable` disqualifier analysis, AND the ASTAnnotator passes
(`returns_list`, `needs_wantarray`, `lvalue`). All together.

Just the closure-capture detection above — which is what BlockAnalyzer currently needs to
replace `_vars_referenced_in_closures` and `_find_closure_captures` — is ~80 lines and
probably a few hours, not a full session. The `unboxable` analysis is another pass of
similar size on top of it.

The genuinely time-consuming part is the `lvalue` annotation (propagating top-down through
the tree, handling all the cases in `ast-annotation-plan.md` section C) and `returns_list`
(bottom-up, handling all the list-returning builtins). Those require careful testing
because they replace existing working-but-fragile code in ExprToCL.

---

## Relationship to `docs/ast-annotation-plan.md`

`ast-annotation-plan.md` (written session 119) is the long-term architecture for
**expression-level** analysis: `lvalue`, `returns_list`, `needs_wantarray`, `unboxable`,
and a proper `VarAnnotator` with a scope stack that tracks closure capture correctly
across multiple nesting levels. That plan annotates OpcodeTree nodes directly via
`set_metadata`/`get_metadata`.

This document (`two-phase-compiler.md`) fills the gap that plan left at the
**block/statement level**: the concrete scoping fix (`stmt_idx`, `_emit_scoped_block`,
`_stmt_pre_hook`) that lets `my` bindings start at their actual declaration point instead
of being hoisted to the sub top. The annotation plan mentions `var_decl_node` but never
specifies the codegen emission mechanism for inline-let.

**Merged picture:**

| Concern | Authority |
|---------|-----------|
| `lvalue` annotation, `returns_list`, `needs_wantarray` | `ast-annotation-plan.md` |
| `VarAnnotator` scope stack, `closure_captured` on decl nodes | `ast-annotation-plan.md` |
| Comprehensive `unboxable` analysis (ref-taking, tie, local) | `ast-annotation-plan.md` |
| Scoping fix: inline-let at `stmt_idx`, `_emit_scoped_block` | This document |
| `_current_outer_scope()`, `$pexpr_factory` decoupling | This document |

When implementing, prefer the annotation plan's `VarAnnotator` scope stack over
`BlockAnalyzer._find_closure_captures` (PPI-level, less accurate for deeply nested
anonymous subs). Prefer the annotation plan's `unboxable` disqualifier list over
`type_hint == 'fixnum'` inference (more comprehensive).

## See Also

- `docs/let-scoping-problem.md` — the scoping bug this plan fixes
- `docs/ast-annotation-plan.md` — expression-level annotation plan (lvalue, returns_list, unboxable, VarAnnotator)
- `docs/closure-lexical-scoping.md` — `__lex__N` renaming for closure capture
- `docs/declaration-ordering.md` — `defvar` vs `defun` ordering at module load time
