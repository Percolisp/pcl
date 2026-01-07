#          -*-Mode: CPerl -*-

# Test dereference operations: $$ref, @$ref, %$ref, &$ref()
# PPI represents these as PPI::Token::Cast followed by PPI::Token::Symbol

use v5.32;
use strict;
use warnings;

use lib ".";

use Data::Dump qw/dump/;
use PPI;
use PPI::Dumper;
use Test::More tests => 66;

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


# Get top-level node info
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


# ----------------------------------------------------------------------
# Test 1: Scalar dereference $$ref

diag "-------- Scalar deref: \$\$ref";
{
  my ($expr_o, $node_id) = parse_expr('$$ref');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  # Expected: prefix_op with Cast '$' and Symbol '$ref'
  is($type, 'internal', '$$ref: top node is internal');
  is($content, 'prefix_op', '$$ref: top node type is prefix_op');

  # Check children
  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is(scalar(@$kids), 2, '$$ref: has 2 children');

  # First child should be the Cast operator '$'
  my $cast_node = $kid_nodes[0];
  is(ref($cast_node), 'PPI::Token::Cast', '$$ref: first child is Cast');
  is($cast_node->content(), '$', '$$ref: cast is $');

  # Second child should be the symbol $ref
  my $sym_node = $kid_nodes[1];
  is(ref($sym_node), 'PPI::Token::Symbol', '$$ref: second child is Symbol');
  is($sym_node->content(), '$ref', '$$ref: symbol is $ref');
}


# ----------------------------------------------------------------------
# Test 2: Array dereference @$ref

diag "-------- Array deref: \@\$ref";
{
  my ($expr_o, $node_id) = parse_expr('@$ref');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($type, 'internal', '@$ref: top node is internal');
  is($content, 'prefix_op', '@$ref: top node type is prefix_op');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is(scalar(@$kids), 2, '@$ref: has 2 children');

  is(ref($kid_nodes[0]), 'PPI::Token::Cast', '@$ref: first child is Cast');
  is($kid_nodes[0]->content(), '@', '@$ref: cast is @');
  is($kid_nodes[1]->content(), '$ref', '@$ref: symbol is $ref');
}


# ----------------------------------------------------------------------
# Test 3: Hash dereference %$ref

diag "-------- Hash deref: \%\$ref";
{
  my ($expr_o, $node_id) = parse_expr('%$ref');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($type, 'internal', '%$ref: top node is internal');
  is($content, 'prefix_op', '%$ref: top node type is prefix_op');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is(scalar(@$kids), 2, '%$ref: has 2 children');

  is(ref($kid_nodes[0]), 'PPI::Token::Cast', '%$ref: first child is Cast');
  is($kid_nodes[0]->content(), '%', '%$ref: cast is %');
  is($kid_nodes[1]->content(), '$ref', '%$ref: symbol is $ref');
}


# ----------------------------------------------------------------------
# Test 4: Deref in expression

diag "-------- Deref in expression: \$\$ref + 1";
{
  my ($expr_o, $node_id) = parse_expr('$$ref + 1');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  # Top should be '+' operator
  is($content, '+', '$$ref + 1: top node is +');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is(scalar(@$kids), 2, '$$ref + 1: has 2 children');

  # First child should be the deref (prefix_op)
  my $deref_node = $kid_nodes[0];
  ok($expr_o->is_internal_node_type($deref_node), '$$ref + 1: first child is internal');
  is($deref_node->{type}, 'prefix_op', '$$ref + 1: first child is prefix_op');

  # Second child should be 1
  is($kid_nodes[1]->content(), '1', '$$ref + 1: second child is 1');
}


# ----------------------------------------------------------------------
# Test 5: Code dereference &$ref (without parens)

diag "-------- Code deref: \&\$ref";
{
  my ($expr_o, $node_id) = parse_expr('&$ref');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($type, 'internal', '&$ref: top node is internal');
  is($content, 'prefix_op', '&$ref: top node type is prefix_op');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is(scalar(@$kids), 2, '&$ref: has 2 children');

  is(ref($kid_nodes[0]), 'PPI::Token::Cast', '&$ref: first child is Cast');
  is($kid_nodes[0]->content(), '&', '&$ref: cast is &');
  is($kid_nodes[1]->content(), '$ref', '&$ref: symbol is $ref');
}


# ----------------------------------------------------------------------
# Test 6: Glob dereference *$ref

diag "-------- Glob deref: \*\$ref";
{
  my ($expr_o, $node_id) = parse_expr('*$ref');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($type, 'internal', '*$ref: top node is internal');
  is($content, 'prefix_op', '*$ref: top node type is prefix_op');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is(scalar(@$kids), 2, '*$ref: has 2 children');

  is(ref($kid_nodes[0]), 'PPI::Token::Cast', '*$ref: first child is Cast');
  is($kid_nodes[0]->content(), '*', '*$ref: cast is *');
  is($kid_nodes[1]->content(), '$ref', '*$ref: symbol is $ref');
}


# ======================================================================
# Complex derefs with braced expressions
# ======================================================================

# ----------------------------------------------------------------------
# Test 7: Braced scalar deref @{$ref}

diag "-------- Braced array deref: \@{\$ref}";
{
  my ($expr_o, $node_id) = parse_expr('@{$ref}');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($type, 'internal', '@{$ref}: top is internal');
  is($content, 'prefix_op', '@{$ref}: type is prefix_op');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is(scalar(@$kids), 2, '@{$ref}: has 2 children');
  is($kid_nodes[0]->content(), '@', '@{$ref}: cast is @');
  is($kid_nodes[1]->content(), '$ref', '@{$ref}: operand is $ref');
}


# ----------------------------------------------------------------------
# Test 8: Braced hash deref %{$ref}

diag "-------- Braced hash deref: \%{\$ref}";
{
  my ($expr_o, $node_id) = parse_expr('%{$ref}');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($type, 'internal', '%{$ref}: top is internal');
  is($content, 'prefix_op', '%{$ref}: type is prefix_op');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is($kid_nodes[0]->content(), '%', '%{$ref}: cast is %');
  is($kid_nodes[1]->content(), '$ref', '%{$ref}: operand is $ref');
}


# ----------------------------------------------------------------------
# Test 9: Complex deref with array access ${$arr[0]}

diag "-------- Complex: \${\$arr[0]}";
{
  my ($expr_o, $node_id) = parse_expr('${$arr[0]}');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($type, 'internal', '${$arr[0]}: top is internal');
  is($content, 'prefix_op', '${$arr[0]}: type is prefix_op');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is($kid_nodes[0]->content(), '$', '${$arr[0]}: cast is $');

  # Second child should be array access
  my $arr_acc = $kid_nodes[1];
  ok($expr_o->is_internal_node_type($arr_acc), '${$arr[0]}: operand is internal');
  is($arr_acc->{type}, 'a_acc', '${$arr[0]}: operand is a_acc');
}


# ----------------------------------------------------------------------
# Test 10: Complex deref with hash access ${$hash{key}}

diag "-------- Complex: \${\$hash{key}}";
{
  my ($expr_o, $node_id) = parse_expr('${$hash{key}}');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($type, 'internal', '${$hash{key}}: top is internal');
  is($content, 'prefix_op', '${$hash{key}}: type is prefix_op');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is($kid_nodes[0]->content(), '$', '${$hash{key}}: cast is $');

  my $hash_acc = $kid_nodes[1];
  ok($expr_o->is_internal_node_type($hash_acc), '${$hash{key}}: operand is internal');
  is($hash_acc->{type}, 'h_acc', '${$hash{key}}: operand is h_acc');
}


# ----------------------------------------------------------------------
# Test 11: Very complex: ${$foo{bar}[4]} = 5

diag "-------- Very complex: \${\$foo{bar}[4]} = 5";
{
  my ($expr_o, $node_id) = parse_expr('${$foo{bar}[4]} = 5');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  # Top should be assignment
  is($content, '=', '${$foo{bar}[4]} = 5: top is =');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is(scalar(@$kids), 2, '${$foo{bar}[4]} = 5: has 2 children');

  # LHS should be the deref
  my $lhs = $kid_nodes[0];
  ok($expr_o->is_internal_node_type($lhs), 'LHS is internal');
  is($lhs->{type}, 'prefix_op', 'LHS is prefix_op (deref)');

  # RHS should be 5
  is($kid_nodes[1]->content(), '5', 'RHS is 5');
}


# ----------------------------------------------------------------------
# Test 12: Nested array deref @{$arr_ref->[0]}

diag "-------- Nested: \@{\$arr_ref->[0]}";
{
  my ($expr_o, $node_id) = parse_expr('@{$arr_ref->[0]}');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($type, 'internal', '@{$arr_ref->[0]}: top is internal');
  is($content, 'prefix_op', '@{$arr_ref->[0]}: type is prefix_op');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is($kid_nodes[0]->content(), '@', '@{$arr_ref->[0]}: cast is @');

  # Operand should be a_ref_acc (arrow array access)
  my $acc = $kid_nodes[1];
  ok($expr_o->is_internal_node_type($acc), '@{$arr_ref->[0]}: operand is internal');
  is($acc->{type}, 'a_ref_acc', '@{$arr_ref->[0]}: operand is a_ref_acc');
}
