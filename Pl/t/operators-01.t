#          -*-Mode: CPerl -*-

# Test additional operators: unary minus/plus, bitwise, xor

use v5.32;
use strict;
use warnings;

use lib ".";

use Data::Dump qw/dump/;
use PPI;
use PPI::Dumper;
use Test::More tests => 41;

BEGIN { use_ok('Pl::PExpr') };


# ----------------------------------------------------------------------
# Helper functions

sub parse_expr {
  my $code    = shift;

  my $doc     = PPI::Document->new(\$code);
  my $expr    = _get_ppi_part($doc);
  my $expr_o  = Pl::PExpr->new(e => $expr, full_PPI => $doc);
  my $node_id = $expr_o->parse_expr_to_tree($expr);

  return ($expr_o, $node_id);
}


sub _get_ppi_part {
  my $doc     = shift;
  my $stmt_ix = shift // 0;

  my @stmts;
  my @parts;
  if (ref($doc) eq 'PPI::Document') {
    @stmts    = $doc->children();
    @parts    = $stmts[$stmt_ix]->children();
  } elsif (ref($doc) eq 'PPI::Statement') {
    @parts    = $doc->children();
  } else {
    die "Code is not PPI::Document?? Is: " . ref($doc);
  }

  return \@parts;
}


sub get_node_info {
  my ($expr_o, $node_id) = @_;

  my $node = $expr_o->get_a_node($node_id);
  my $kids = $expr_o->get_node_children($node_id);

  my $type;
  my $content;
  if ($expr_o->is_internal_node_type($node)) {
    $type    = 'internal';
    $content = $node->{type};
  } else {
    $type    = ref($node);
    $content = $node->content() if $node->can('content');
  }

  return ($type, $content, $kids, $node);
}


# ======================================================================
# Unary Minus/Plus
# ======================================================================

diag "-------- Unary minus: -\$y + 5";
{
  my ($expr_o, $node_id) = parse_expr('-$y + 5');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  # Top should be +
  is($content, '+', '-$y + 5: top is +');

  my @kid_nodes = $expr_o->get_nodes(@$kids);

  # First child should be prefix_op (unary minus)
  my $neg = $kid_nodes[0];
  ok($expr_o->is_internal_node_type($neg), '-$y + 5: first child is internal');
  is($neg->{type}, 'prefix_op', '-$y + 5: first child is prefix_op');

  # Second child should be 5
  is($kid_nodes[1]->content(), '5', '-$y + 5: second child is 5');
}


diag "-------- Unary minus after operator: 5 * -\$x";
{
  my ($expr_o, $node_id) = parse_expr('5 * -$x');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  # Top should be *
  is($content, '*', '5 * -$x: top is *');

  my @kid_nodes = $expr_o->get_nodes(@$kids);

  # First child should be 5
  is($kid_nodes[0]->content(), '5', '5 * -$x: first child is 5');

  # Second child should be prefix_op (unary minus)
  my $neg = $kid_nodes[1];
  ok($expr_o->is_internal_node_type($neg), '5 * -$x: second child is internal');
  is($neg->{type}, 'prefix_op', '5 * -$x: second child is prefix_op');
}


diag "-------- Double unary: -\$x * -\$y";
{
  my ($expr_o, $node_id) = parse_expr('-$x * -$y');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($content, '*', '-$x * -$y: top is *');

  my @kid_nodes = $expr_o->get_nodes(@$kids);

  # Both children should be prefix_op
  ok($expr_o->is_internal_node_type($kid_nodes[0]), '-$x * -$y: lhs is internal');
  is($kid_nodes[0]->{type}, 'prefix_op', '-$x * -$y: lhs is prefix_op');

  ok($expr_o->is_internal_node_type($kid_nodes[1]), '-$x * -$y: rhs is internal');
  is($kid_nodes[1]->{type}, 'prefix_op', '-$x * -$y: rhs is prefix_op');
}


diag "-------- Unary plus: +\$x";
{
  my ($expr_o, $node_id) = parse_expr('+$x');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($type, 'internal', '+$x: top is internal');
  is($content, 'prefix_op', '+$x: type is prefix_op');
}


# ======================================================================
# Bitwise Operators
# ======================================================================

diag "-------- Bitwise AND: \$x & \$y";
{
  my ($expr_o, $node_id) = parse_expr('$x & $y');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($content, '&', '$x & $y: top is &');
  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is($kid_nodes[0]->content(), '$x', '$x & $y: lhs is $x');
  is($kid_nodes[1]->content(), '$y', '$x & $y: rhs is $y');
}


diag "-------- Bitwise OR: \$x | \$y";
{
  my ($expr_o, $node_id) = parse_expr('$x | $y');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($content, '|', '$x | $y: top is |');
  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is($kid_nodes[0]->content(), '$x', '$x | $y: lhs is $x');
  is($kid_nodes[1]->content(), '$y', '$x | $y: rhs is $y');
}


diag "-------- Bitwise XOR: \$x ^ \$y";
{
  my ($expr_o, $node_id) = parse_expr('$x ^ $y');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($content, '^', '$x ^ $y: top is ^');
  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is($kid_nodes[0]->content(), '$x', '$x ^ $y: lhs is $x');
  is($kid_nodes[1]->content(), '$y', '$x ^ $y: rhs is $y');
}


diag "-------- Bitwise precedence: \$x & \$y | \$z";
{
  # & has higher precedence than |, so should parse as ($x & $y) | $z
  my ($expr_o, $node_id) = parse_expr('$x & $y | $z');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  # Top should be | (lowest precedence of the two)
  is($content, '|', '$x & $y | $z: top is |');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is(scalar(@$kids), 2, '$x & $y | $z: has 2 children');

  # RHS should be $z
  is($kid_nodes[1]->content(), '$z', '$x & $y | $z: rhs is $z');
}


# ======================================================================
# Logical XOR
# ======================================================================

diag "-------- Logical xor: \$x xor \$y";
{
  my ($expr_o, $node_id) = parse_expr('$x xor $y');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($content, 'xor', '$x xor $y: top is xor');
  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is($kid_nodes[0]->content(), '$x', '$x xor $y: lhs is $x');
  is($kid_nodes[1]->content(), '$y', '$x xor $y: rhs is $y');
}


diag "-------- xor precedence: \$a = \$x xor \$y";
{
  # xor has lower precedence than =, so should parse as ($a = $x) xor $y
  my ($expr_o, $node_id) = parse_expr('$a = $x xor $y');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  # Top should be xor (lowest precedence)
  is($content, 'xor', '$a = $x xor $y: top is xor');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is(scalar(@$kids), 2, '$a = $x xor $y: has 2 children');

  # RHS should be $y
  is($kid_nodes[1]->content(), '$y', '$a = $x xor $y: rhs is $y');
}


# ======================================================================
# Combined tests
# ======================================================================

diag "-------- Combined: -\$x & \$y";
{
  # Unary - has higher precedence than &
  my ($expr_o, $node_id) = parse_expr('-$x & $y');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($content, '&', '-$x & $y: top is &');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  ok($expr_o->is_internal_node_type($kid_nodes[0]), '-$x & $y: lhs is internal');
  is($kid_nodes[0]->{type}, 'prefix_op', '-$x & $y: lhs is prefix_op');
  is($kid_nodes[1]->content(), '$y', '-$x & $y: rhs is $y');
}


diag "-------- Combined: \$x | \$y && \$z";
{
  # | has higher precedence than &&, so should parse as ($x | $y) && $z
  my ($expr_o, $node_id) = parse_expr('$x | $y && $z');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  # Top should be && (lower precedence)
  is($content, '&&', '$x | $y && $z: top is &&');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is(scalar(@$kids), 2, '$x | $y && $z: has 2 children');

  # RHS should be $z
  is($kid_nodes[1]->content(), '$z', '$x | $y && $z: rhs is $z');
}
