package Pl::BlockAnalyzer;

# Copyright (c) 2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

# Two-phase block analysis: scan declarations and closure captures from PPI
# before any code generation.  The analysis drives _emit_scoped_block in
# Parser.pm, which opens let-bindings at the exact statement where each 'my'
# declaration first appears instead of hoisting everything to the sub top.
#
# See docs/two-phase-compiler.md for the full design.

use v5.20;
use strict;
use warnings;

use Moo;

# ── Entry point ──────────────────────────────────────────────────────────────

# analyze($class, $block, $outer)
#   $block         — PPI element (Block/Document/etc.) or arrayref of PPI elements
#   $outer         — hashref: perl_name => { type=>'my'|'our'|..., cl_name=>'...' }
#
# Returns an analysis hashref (see docs/two-phase-compiler.md "Output").
sub analyze {
    my ($class, $block, $outer) = @_;
    $outer //= {};

    my @stmts   = _stmts_of($block);
    my $self    = bless { outer => $outer }, $class;

    my $decls      = $self->_collect_declarations(\@stmts);
    my %in_block   = map { $_ => 1 } map { @{$_->{vars}} } @$decls;
    my $captured   = $self->_find_closure_captures(\@stmts, \%in_block);
    # Usage collection (the OpcodeTree walk behind an optional $pexpr_factory
    # 4th argument) was never wired: no caller has ever passed one, so $usages
    # was always empty and the walk was dead.  Deleted in #303; the consumers
    # below see exactly what they always saw.
    my $usages     = {};
    my $outer_refs = $self->_find_outer_refs($usages, \%in_block);
    my $vars       = $self->_build_var_map($decls, $usages, $captured,
                                           $outer_refs, $outer);
    return {
        declarations => $decls,
        vars         => $vars,
        outer_refs   => $outer_refs,
    };
}

# ── Statement list helper ─────────────────────────────────────────────────────

sub _stmts_of {
    my ($block) = @_;
    if (ref($block) eq 'ARRAY') {
        return grep { ref($_) && ref($_) !~ /Whitespace|Comment/ } @$block;
    }
    return grep { ref($_) && ref($_) !~ /Whitespace|Comment/ } $block->children;
}

# ── Declaration collection (PPI-level) ───────────────────────────────────────
#
# Walks the direct-child statements of the block.  For PPI::Statement::Variable
# nodes, records the declarator type and all variable names declared.
#
# For compound statements (while, for, if, etc.), recurses into their block
# children to find inner declarations, but maps them to the compound statement's
# ppi_stmt so _emit_scoped_block can open the let-binding before the compound
# statement.  Inner declarations of bare blocks bubble up only for 'state'.
#
# Does NOT recurse into: named subs, BEGIN/END blocks, anonymous sub bodies.

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
            if ($decl_type =~ /^(my|our|state|local)$/) {
                my @vars;
                for my $c (@children[1..$#children]) {
                    my $cr = ref($c);
                    if ($cr eq 'PPI::Token::Symbol') {
                        push @vars, $c->content;
                        last;
                    } elsif ($cr eq 'PPI::Structure::List') {
                        my $syms = $c->find('PPI::Token::Symbol') || [];
                        push @vars, map { $_->content } @$syms;
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
        } elsif (ref($stmt) && $stmt->can('children')) {
            # Compound statement (while/for/if/etc.): recurse into its block
            # children to find inner declarations, remapping ppi_stmt to $stmt
            # so the hook fires when this compound statement is about to be
            # processed.
            for my $child ($stmt->children) {
                my $cr = ref($child);
                next unless $cr && $child->can('children');
                next if $cr eq 'PPI::Statement::Sub'
                     || $cr eq 'PPI::Statement::Scheduled';
                if ($cr eq 'PPI::Structure::Block') {
                    my $prev = $child->sprevious_sibling;
                    # Skip anonymous sub bodies
                    next if $prev
                            && ref($prev) eq 'PPI::Token::Word'
                            && $prev->content eq 'sub';
                    my $inner = $self->_collect_declarations([_stmts_of($child)]);
                    # Only 'state' vars bubble up from a nested block (bare OR a
                    # while/for/if body).  'my' vars in those blocks are lexically
                    # scoped to the block and are handled by that block's own
                    # _with_declarations; hoisting them here would open a spurious
                    # parent-level let that reuses the same closure-capture rename
                    # as a same-named outer var and shadows it (broke closure.t
                    # bizz(): `my $i = $i` in an else branch read nil, not 7).
                    push @result,
                        grep { $_->{decl_type} eq 'state' } @$inner;
                }
            }
        }
        $idx++;
    }

    return \@result;
}

# ── Closure-capture detection (PPI-level) ────────────────────────────────────
#
# Scans for anonymous sub bodies within the block's statements and records
# which variables declared in this block are referenced inside them.

sub _find_closure_captures {
    my ($self, $stmts, $in_block) = @_;
    my %captured;

    for my $stmt (@$stmts) {
        next unless ref($stmt) && $stmt->can('find');
        my $sub_kws = $stmt->find(
            sub { $_[1]->isa('PPI::Token::Word') && $_[1]->content eq 'sub' }
        ) || [];
        for my $kw (@$sub_kws) {
            # Skip named subs: first non-whitespace sibling after 'sub' is a Word.
            my $first = $kw->next_sibling;
            $first = $first->next_sibling
                while $first && $first->isa('PPI::Token::Whitespace');
            next if $first && $first->isa('PPI::Token::Word');

            # Walk forward to find the anonymous sub's body block.
            my $sib = $kw->next_sibling;
            $sib = $sib->next_sibling
                while $sib && !$sib->isa('PPI::Structure::Block');
            next unless $sib;

            my $syms = $sib->find('PPI::Token::Symbol') || [];
            $captured{$_->content} = 1
                for grep { $in_block->{$_->content} } @$syms;
        }
    }

    return \%captured;
}

# ── Outer-reference detection ─────────────────────────────────────────────────

sub _find_outer_refs {
    my ($self, $usages, $in_block) = @_;
    my %outer;
    for my $var (keys %$usages) {
        next if $in_block->{$var};
        my $otype = $self->{outer}{$var}
                    ? $self->{outer}{$var}{type} : 'unknown';
        $outer{$var} = { outer_type => $otype };
    }
    return \%outer;
}

# ── Variable map construction ─────────────────────────────────────────────────

sub _build_var_map {
    my ($self, $decls, $usages, $captured, $outer_refs, $outer) = @_;
    my %vars;

    my $didx = 0;
    for my $d (@$decls) {
        for my $var (@{$d->{vars}}) {
            next if exists $vars{$var};  # first declaration wins
            $vars{$var} = {
                sigil         => substr($var, 0, 1),
                scope         => 'local',
                decl_type     => $d->{decl_type},
                decl_idx      => $didx,
                decl_stmt_idx => $d->{stmt_idx},
                captured      => ($captured->{$var} ? 1 : 0),
                type_hint     => 'any',
                usages        => ($usages->{$var} // []),
            };
        }
        $didx++;
    }

    for my $var (keys %$outer_refs) {
        next if exists $vars{$var};
        $vars{$var} = {
            sigil         => substr($var, 0, 1),
            scope         => 'outer',
            decl_type     => undef,
            decl_idx      => undef,
            decl_stmt_idx => undef,
            captured      => 0,
            type_hint     => 'any',
            usages        => ($usages->{$var} // []),
        };
    }

    # Type inference: scalar locals only, not captured (can't safely unbox)
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

    my $has_arith  = $ctxs{arith}  // 0;
    my $has_string = $ctxs{string} // 0;
    return 'any'    if $has_arith && $has_string;
    return 'fixnum' if $has_arith && !$has_string;
    return 'string' if $has_string && !$has_arith;
    return 'any';
}

1;
