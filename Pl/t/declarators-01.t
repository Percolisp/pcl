#!/usr/bin/env perl
#          -*-Mode: CPerl -*-

# Test declarator handling: my, our, state, local

use v5.30;
use strict;
use warnings;

use lib ".";

use Data::Dump qw/dump/;
use PPI;
use PPI::Dumper;

use Test::More tests => 63;
BEGIN { use_ok('Pl::PExpr') };


# Helper to get PPI expression from code string
sub _get_ppi_part {
    my $doc     = shift;
    my $stmt_ix = shift // 0;
    my @stmts   = $doc->children();
    my @exprs   = $stmts[$stmt_ix]->children();
    return \@exprs;
}

# Helper to parse an expression string, returning both tree and declarations
sub parse_expr {
    my $code = shift;
    my $doc  = PPI::Document->new(\$code);
    my $expr = _get_ppi_part($doc);
    my $parser = Pl::PExpr->new(e => $expr, full_PPI => $doc);
    my ($node_id, $decls) = $parser->parse_expr_to_tree($expr);
    return ($parser, $node_id, $decls);
}

# Helper to get node type
sub get_type {
    my ($parser, $node_id) = @_;
    my $node = $parser->get_a_node($node_id);
    return $node->{type} if ref($node) eq 'PPIreference';
    return $node->content() if $node->can('content');
    return ref($node);
}


# ========================================
diag "";
diag "-------- Basic 'my' declaration:";

{
    my ($parser, $node_id, $decls) = parse_expr('my $x = 10');
    ok(defined($node_id), 'my $x = 10 - parsed');
    is(get_type($parser, $node_id), '=', 'AST has = at root (not my)');

    is(scalar(@$decls), 1, 'One declaration found');
    is($decls->[0]{type}, 'my', 'Declaration type is my');
    is($decls->[0]{var}, '$x', 'Declaration variable is $x');
}

# ========================================
diag "";
diag "-------- 'our' declaration:";

{
    my ($parser, $node_id, $decls) = parse_expr('our $config = {}');
    ok(defined($node_id), 'our $config = {} - parsed');
    is(get_type($parser, $node_id), '=', 'AST has = at root');

    is(scalar(@$decls), 1, 'One declaration found');
    is($decls->[0]{type}, 'our', 'Declaration type is our');
    is($decls->[0]{var}, '$config', 'Declaration variable is $config');
}

# ========================================
diag "";
diag "-------- 'state' declaration:";

{
    my ($parser, $node_id, $decls) = parse_expr('state $count = 0');
    ok(defined($node_id), 'state $count = 0 - parsed');
    is(get_type($parser, $node_id), '=', 'AST has = at root');

    is(scalar(@$decls), 1, 'One declaration found');
    is($decls->[0]{type}, 'state', 'Declaration type is state');
    is($decls->[0]{var}, '$count', 'Declaration variable is $count');
}

# ========================================
diag "";
diag "-------- 'local' declaration:";

{
    my ($parser, $node_id, $decls) = parse_expr('local $/ = undef');
    ok(defined($node_id), 'local $/ = undef - parsed');
    is(get_type($parser, $node_id), '=', 'AST has = at root');

    is(scalar(@$decls), 1, 'One declaration found');
    is($decls->[0]{type}, 'local', 'Declaration type is local');
    is($decls->[0]{var}, '$/', 'Declaration variable is $/');
}

# ========================================
diag "";
diag "-------- Complex expression with declarator:";

{
    my ($parser, $node_id, $decls) = parse_expr('my $total = $price * (1 + $tax)');
    ok(defined($node_id), 'my $total = $price * (1 + $tax) - parsed');
    is(get_type($parser, $node_id), '=', 'AST has = at root');

    is(scalar(@$decls), 1, 'One declaration found');
    is($decls->[0]{type}, 'my', 'Declaration type is my');
    is($decls->[0]{var}, '$total', 'Declaration variable is $total');
}

# ========================================
diag "";
diag "-------- Expression without declarator:";

{
    my ($parser, $node_id, $decls) = parse_expr('$x = $y + $z');
    ok(defined($node_id), '$x = $y + $z - parsed');
    is(get_type($parser, $node_id), '=', 'AST has = at root');

    is(scalar(@$decls), 0, 'No declarations found');
}

# ========================================
diag "";
diag "-------- Backward compatibility (scalar context):";

{
    my $code = 'my $x = 10';
    my $doc  = PPI::Document->new(\$code);
    my $expr = _get_ppi_part($doc);
    my $parser = Pl::PExpr->new(e => $expr, full_PPI => $doc);

    # Scalar context - should just return node_id
    my $node_id = $parser->parse_expr_to_tree($expr);
    ok(defined($node_id), 'Scalar context returns node_id');
    ok(!ref($node_id) || ref($node_id) ne 'ARRAY', 'Scalar context returns scalar, not array');

    # Can still access declarations via method
    my $decls = $parser->declarations;
    is(scalar(@$decls), 1, 'Declarations accessible via method');
    is($decls->[0]{type}, 'my', 'Declaration type correct');
    is($decls->[0]{var}, '$x', 'Declaration var correct');
}

# ========================================
diag "";
diag "-------- List declarations:";

{
    my ($parser, $node_id, $decls) = parse_expr('my ($x, $y) = (1, 2)');
    ok(defined($node_id), 'my ($x, $y) = (1, 2) - parsed');
    is(get_type($parser, $node_id), '=', 'AST has = at root');

    is(scalar(@$decls), 2, 'Two declarations found');
    is($decls->[0]{type}, 'my', 'First declaration type is my');
    is($decls->[0]{var}, '$x', 'First declaration variable is $x');
    is($decls->[1]{type}, 'my', 'Second declaration type is my');
    is($decls->[1]{var}, '$y', 'Second declaration variable is $y');
}

# ========================================
diag "";
diag "-------- List with array:";

{
    my ($parser, $node_id, $decls) = parse_expr('my ($a, @rest) = @array');
    ok(defined($node_id), 'my ($a, @rest) = @array - parsed');

    is(scalar(@$decls), 2, 'Two declarations found');
    is($decls->[0]{var}, '$a', 'First var is $a');
    is($decls->[1]{var}, '@rest', 'Second var is @rest');
}

# ========================================
diag "";
diag "-------- List with hash:";

{
    my ($parser, $node_id, $decls) = parse_expr('my (%hash) = @pairs');
    ok(defined($node_id), 'my (%hash) = @pairs - parsed');

    is(scalar(@$decls), 1, 'One declaration found');
    is($decls->[0]{var}, '%hash', 'Declaration variable is %hash');
}

# ========================================
diag "";
diag "-------- our with list:";

{
    my ($parser, $node_id, $decls) = parse_expr('our ($x, $y)');
    ok(defined($node_id), 'our ($x, $y) - parsed');

    is(scalar(@$decls), 2, 'Two declarations found');
    is($decls->[0]{type}, 'our', 'Declaration type is our');
    is($decls->[0]{var}, '$x', 'First var is $x');
    is($decls->[1]{var}, '$y', 'Second var is $y');
}

# ========================================
diag "";
diag "-------- local with list:";

{
    my ($parser, $node_id, $decls) = parse_expr('local ($/, $\)');
    ok(defined($node_id), 'local ($/, $\) - parsed');

    is(scalar(@$decls), 2, 'Two declarations found');
    is($decls->[0]{type}, 'local', 'Declaration type is local');
    is($decls->[0]{var}, '$/', 'First var is $/');
    is($decls->[1]{var}, '$\\', 'Second var is $\\');
}

# ========================================
diag "";
diag "-------- state with list:";

{
    my ($parser, $node_id, $decls) = parse_expr('state ($count, $total) = (0, 0)');
    ok(defined($node_id), 'state ($count, $total) = (0, 0) - parsed');

    is(scalar(@$decls), 2, 'Two declarations found');
    is($decls->[0]{type}, 'state', 'Declaration type is state');
    is($decls->[0]{var}, '$count', 'First var is $count');
    is($decls->[1]{var}, '$total', 'Second var is $total');
}

done_testing();
