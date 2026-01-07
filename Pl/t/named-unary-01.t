#          -*-Mode: CPerl -*-

# Test named unary operator precedence
# Named unary operators (defined, ref, scalar, exists, delete) should
# bind tighter than binary operators, taking only the immediately following term.

use v5.32;
use strict;
use warnings;

use lib ".";

use Data::Dump qw/dump/;
use PPI;
use PPI::Dumper;
use Test::More tests => 26;

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
# defined
# ======================================================================

diag "-------- defined \$x && \$y (should be (defined \$x) && \$y)";
{
  my ($expr_o, $node_id) = parse_expr('defined $x && $y');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  # Top should be && (the binary operator binds after defined takes $x)
  is($content, '&&', 'defined $x && $y: top is &&');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is(scalar(@$kids), 2, 'defined $x && $y: has 2 children');

  # First child should be funcall(defined)
  ok($expr_o->is_internal_node_type($kid_nodes[0]), 'defined $x && $y: lhs is internal');
  is($kid_nodes[0]->{type}, 'funcall', 'defined $x && $y: lhs is funcall');

  # Second child should be $y
  is($kid_nodes[1]->content(), '$y', 'defined $x && $y: rhs is $y');
}


diag "-------- defined \$x || \$y";
{
  my ($expr_o, $node_id) = parse_expr('defined $x || $y');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($content, '||', 'defined $x || $y: top is ||');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  ok($expr_o->is_internal_node_type($kid_nodes[0]), 'defined $x || $y: lhs is funcall');
  is($kid_nodes[1]->content(), '$y', 'defined $x || $y: rhs is $y');
}


# ======================================================================
# ref
# ======================================================================

diag "-------- ref \$x eq 'HASH'";
{
  my ($expr_o, $node_id) = parse_expr("ref \$x eq 'HASH'");
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  # Top should be eq
  is($content, 'eq', "ref \$x eq 'HASH': top is eq");

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is(scalar(@$kids), 2, "ref \$x eq 'HASH': has 2 children");

  # First child should be funcall(ref)
  ok($expr_o->is_internal_node_type($kid_nodes[0]), "ref \$x eq 'HASH': lhs is funcall");
  is($kid_nodes[0]->{type}, 'funcall', "ref \$x eq 'HASH': lhs is funcall type");
}


# ======================================================================
# scalar
# ======================================================================

diag "-------- scalar \@arr == 5";
{
  my ($expr_o, $node_id) = parse_expr('scalar @arr == 5');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  # Top should be ==
  is($content, '==', 'scalar @arr == 5: top is ==');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is(scalar(@$kids), 2, 'scalar @arr == 5: has 2 children');

  # First child should be funcall(scalar)
  ok($expr_o->is_internal_node_type($kid_nodes[0]), 'scalar @arr == 5: lhs is funcall');
}


# ======================================================================
# exists
# ======================================================================

diag "-------- exists \$hash{key} && \$x";
{
  my ($expr_o, $node_id) = parse_expr('exists $hash{key} && $x');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  # Top should be &&
  is($content, '&&', 'exists $hash{key} && $x: top is &&');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is(scalar(@$kids), 2, 'exists $hash{key} && $x: has 2 children');

  # Second child should be $x
  is($kid_nodes[1]->content(), '$x', 'exists $hash{key} && $x: rhs is $x');
}


# ======================================================================
# delete
# ======================================================================

diag "-------- delete \$hash{key} || \$default";
{
  my ($expr_o, $node_id) = parse_expr('delete $hash{key} || $default');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  # Top should be ||
  is($content, '||', 'delete $hash{key} || $default: top is ||');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is(scalar(@$kids), 2, 'delete $hash{key} || $default: has 2 children');

  # Second child should be $default
  is($kid_nodes[1]->content(), '$default', 'delete $hash{key} || $default: rhs is $default');
}


# ======================================================================
# Combined / edge cases
# ======================================================================

diag "-------- defined \$x && defined \$y";
{
  my ($expr_o, $node_id) = parse_expr('defined $x && defined $y');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  # Top should be &&
  is($content, '&&', 'defined $x && defined $y: top is &&');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is(scalar(@$kids), 2, 'defined $x && defined $y: has 2 children');

  # Both children should be funcalls
  ok($expr_o->is_internal_node_type($kid_nodes[0]), 'defined $x && defined $y: lhs is funcall');
  ok($expr_o->is_internal_node_type($kid_nodes[1]), 'defined $x && defined $y: rhs is funcall');
}
