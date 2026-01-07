#!/usr/bin/env perl
#          -*-Mode: CPerl -*-

# Test missing operators: &&, ||, &&=, ||=, x=

use v5.30;
use strict;
use warnings;

use lib ".";

use Data::Dump qw/dump/;
use PPI;
use PPI::Dumper;

use Test::More tests => 23;
BEGIN { use_ok('Pl::PExpr') };


# Helper to get PPI expression from code string
sub _get_ppi_part {
    my $doc     = shift;
    my $stmt_ix = shift // 0;
    my @stmts   = $doc->children();
    my @exprs   = $stmts[$stmt_ix]->children();
    return \@exprs;
}

# Helper to parse an expression string
sub parse_expr {
    my $code = shift;
    my $doc  = PPI::Document->new(\$code);
    my $expr = _get_ppi_part($doc);
    my $parser = Pl::PExpr->new(e => $expr, full_PPI => $doc);
    my $node_id = $parser->parse_expr_to_tree($expr);
    return ($parser, $node_id);
}

# Helper to get node type
sub get_type {
    my ($parser, $node_id) = @_;
    my $node = $parser->get_a_node($node_id);
    return $node->{type} if ref($node) eq 'PPIreference';
    return $node->content() if $node->can('content');
    return ref($node);
}

# Helper to get children types
sub get_children_types {
    my ($parser, $node_id) = @_;
    my $kids = $parser->get_node_children($node_id);
    return [ map { get_type($parser, $_) } @$kids ];
}


# ========================================
diag "";
diag "-------- Logical AND (&&):";

{
    my ($parser, $node_id) = parse_expr('$x && $y');
    ok(defined($node_id), '&& operator - parsed');
    is(get_type($parser, $node_id), '&&', '&& operator - correct type');
}

# ========================================
diag "";
diag "-------- Logical OR (||):";

{
    my ($parser, $node_id) = parse_expr('$x || $y');
    ok(defined($node_id), '|| operator - parsed');
    is(get_type($parser, $node_id), '||', '|| operator - correct type');
}

# ========================================
diag "";
diag "-------- Defined-or (//):";

{
    my ($parser, $node_id) = parse_expr('$x // $y');
    ok(defined($node_id), '// operator - parsed');
    is(get_type($parser, $node_id), '//', '// operator - correct type');
}

# ========================================
diag "";
diag "-------- Precedence tests:";

# && has higher precedence than ||
{
    my ($parser, $node_id) = parse_expr('$x || $y && $z');
    ok(defined($node_id), '$x || $y && $z - parsed');
    # Should parse as $x || ($y && $z) because && > ||
    is(get_type($parser, $node_id), '||', 'Top operator is ||');
}

# && has higher precedence than =
{
    my ($parser, $node_id) = parse_expr('$x = $y && $z');
    ok(defined($node_id), '$x = $y && $z - parsed');
    # Should parse as $x = ($y && $z) because && > =
    is(get_type($parser, $node_id), '=', 'Top operator is =');
}

# && with comparisons
{
    my ($parser, $node_id) = parse_expr('$x > 5 && $y < 10');
    ok(defined($node_id), '$x > 5 && $y < 10 - parsed');
    is(get_type($parser, $node_id), '&&', 'Top operator is && (comparisons bind tighter)');
}

# ========================================
diag "";
diag "-------- Assignment operators:";

{
    my ($parser, $node_id) = parse_expr('$x &&= $y');
    ok(defined($node_id), '&&= operator - parsed');
    is(get_type($parser, $node_id), '&&=', '&&= operator - correct type');
}

{
    my ($parser, $node_id) = parse_expr('$x ||= $y');
    ok(defined($node_id), '||= operator - parsed');
    is(get_type($parser, $node_id), '||=', '||= operator - correct type');
}

{
    my ($parser, $node_id) = parse_expr('$str x= 3');
    ok(defined($node_id), 'x= operator - parsed');
    is(get_type($parser, $node_id), 'x=', 'x= operator - correct type');
}

# ========================================
diag "";
diag "-------- Complex expressions:";

{
    my ($parser, $node_id) = parse_expr('$config ||= {}');
    ok(defined($node_id), '$config ||= {} - parsed');
    is(get_type($parser, $node_id), '||=', 'Common idiom: ||= with hash ref');
}

{
    my ($parser, $node_id) = parse_expr('$x || $y && $z // $w');
    ok(defined($node_id), '$x || $y && $z // $w - parsed');
    # && (20) > || = // (19), left-assoc: ($x || ($y && $z)) // $w
    is(get_type($parser, $node_id), '//', 'Complex: // at top (same prec as ||, left-to-right)');
}

done_testing();
