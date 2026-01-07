#          -*-Mode: CPerl -*-

# Test package-qualified names: Foo::bar(), $Foo::x, etc.
# PPI represents these as single tokens (Word or Symbol)

use v5.32;
use strict;
use warnings;

use lib ".";

use Data::Dump qw/dump/;
use PPI;
use PPI::Dumper;
use Test::More tests => 33;

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


# ----------------------------------------------------------------------
# Test 1: Package-qualified function call with parens

diag "-------- Package function: Foo::bar()";
{
  my ($expr_o, $node_id) = parse_expr('Foo::bar()');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($type, 'internal', 'Foo::bar(): top is internal');
  is($content, 'funcall', 'Foo::bar(): type is funcall');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is(scalar(@$kids), 1, 'Foo::bar(): has 1 child (just func name)');
  is($kid_nodes[0]->content(), 'Foo::bar', 'Foo::bar(): func name preserved');
}


# ----------------------------------------------------------------------
# Test 2: Package function with arguments

diag "-------- Package function with args: Foo::bar(1, 2)";
{
  my ($expr_o, $node_id) = parse_expr('Foo::bar(1, 2)');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($type, 'internal', 'Foo::bar(1,2): top is internal');
  is($content, 'funcall', 'Foo::bar(1,2): type is funcall');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is(scalar(@$kids), 3, 'Foo::bar(1,2): has 3 children');
  is($kid_nodes[0]->content(), 'Foo::bar', 'Foo::bar(1,2): func name');
  is($kid_nodes[1]->content(), '1', 'Foo::bar(1,2): first arg');
  is($kid_nodes[2]->content(), '2', 'Foo::bar(1,2): second arg');
}


# ----------------------------------------------------------------------
# Test 3: Nested package function

diag "-------- Nested package: Foo::Bar::baz()";
{
  my ($expr_o, $node_id) = parse_expr('Foo::Bar::baz()');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($type, 'internal', 'Foo::Bar::baz(): top is internal');
  is($content, 'funcall', 'Foo::Bar::baz(): type is funcall');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is($kid_nodes[0]->content(), 'Foo::Bar::baz', 'Foo::Bar::baz(): nested name preserved');
}


# ----------------------------------------------------------------------
# Test 4: Package scalar variable

diag "-------- Package scalar: \$Foo::x";
{
  my ($expr_o, $node_id) = parse_expr('$Foo::x');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($type, 'PPI::Token::Symbol', '$Foo::x: is Symbol token');
  is($content, '$Foo::x', '$Foo::x: content preserved');
}


# ----------------------------------------------------------------------
# Test 5: Package array variable

diag "-------- Package array: \@Foo::arr";
{
  my ($expr_o, $node_id) = parse_expr('@Foo::arr');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($type, 'PPI::Token::Symbol', '@Foo::arr: is Symbol token');
  is($content, '@Foo::arr', '@Foo::arr: content preserved');
}


# ----------------------------------------------------------------------
# Test 6: Package hash variable

diag "-------- Package hash: \%Foo::hash";
{
  my ($expr_o, $node_id) = parse_expr('%Foo::hash');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($type, 'PPI::Token::Symbol', '%Foo::hash: is Symbol token');
  is($content, '%Foo::hash', '%Foo::hash: content preserved');
}


# ----------------------------------------------------------------------
# Test 7: Package variable in expression

diag "-------- Package var in expr: \$Foo::x + 1";
{
  my ($expr_o, $node_id) = parse_expr('$Foo::x + 1');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($content, '+', '$Foo::x + 1: top is +');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is(scalar(@$kids), 2, '$Foo::x + 1: has 2 children');
  is($kid_nodes[0]->content(), '$Foo::x', '$Foo::x + 1: first child is $Foo::x');
  is($kid_nodes[1]->content(), '1', '$Foo::x + 1: second child is 1');
}


# ----------------------------------------------------------------------
# Test 8: Assignment to package variable

diag "-------- Assignment: \$Foo::x = 10";
{
  my ($expr_o, $node_id) = parse_expr('$Foo::x = 10');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($content, '=', '$Foo::x = 10: top is =');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is(scalar(@$kids), 2, '$Foo::x = 10: has 2 children');
  is($kid_nodes[0]->content(), '$Foo::x', '$Foo::x = 10: lhs is $Foo::x');
  is($kid_nodes[1]->content(), '10', '$Foo::x = 10: rhs is 10');
}


# ----------------------------------------------------------------------
# Test 9: Package function as argument

diag "-------- Pkg func as arg: foo(Bar::baz())";
{
  my ($expr_o, $node_id) = parse_expr('foo(Bar::baz())');
  my ($type, $content, $kids, $node) = get_node_info($expr_o, $node_id);

  is($type, 'internal', 'foo(Bar::baz()): top is internal');
  is($content, 'funcall', 'foo(Bar::baz()): type is funcall');

  my @kid_nodes = $expr_o->get_nodes(@$kids);
  is($kid_nodes[0]->content(), 'foo', 'foo(Bar::baz()): outer func is foo');

  # Second child should be the inner funcall
  my $inner = $kid_nodes[1];
  ok($expr_o->is_internal_node_type($inner), 'foo(Bar::baz()): arg is internal node');
  is($inner->{type}, 'funcall', 'foo(Bar::baz()): arg is funcall');
}
