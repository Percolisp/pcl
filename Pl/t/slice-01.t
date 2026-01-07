#          -*-Mode: CPerl -*-

# Test array and hash slices

use v5.32;
use strict;
use warnings;

use lib ".";

use Data::Dump qw/dump/;

use PPI;
use PPI::Dumper;

use Test::More tests => 47;
BEGIN { use_ok('Pl::PExpr') };

my $code;
my $doc;
my $expr_o;
my $expr;
my $node_id;

# ----------------------------------------------------------------------
# Helper functions

sub parse_expr {
  my $code    = shift;

  my $doc     = PPI::Document->new(\$code);
  my $expr    = _get_ppi_part($doc);
  my $expr_o  = Pl::PExpr->new(
      e => $expr,
      full_PPI => $doc,
  );

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

sub verify_node_type {
  my ($expr_o, $node_id, $expected_type, $msg) = @_;
  
  my $node = $expr_o->get_a_node($node_id);
  
  if ($expr_o->is_internal_node_type($node)) {
    # Internal node (like slice_a_acc, progn, funcall, etc)
    pass("$msg - is internal node");
    is($node->{type}, $expected_type, "$msg - type is $expected_type");
  } else {
    # PPI token (like operators, variables, literals)
    pass("$msg - is PPI token");
    is($node->content(), $expected_type, "$msg - content is $expected_type");
  }
}

sub verify_slice {
  my ($expr_o, $node_id, $expected_type, $var_name, $index_count, $msg) = @_;
  
  verify_node_type($expr_o, $node_id, $expected_type, $msg);
  
  my $children = $expr_o->get_node_children($node_id);
  
  # First child is the variable
  my $var_node = $expr_o->get_a_node($children->[0]);
  is($var_node->content(), $var_name, "$msg - variable name");
  
  # Remaining children are indices/keys
  is(scalar(@$children) - 1, $index_count, "$msg - has $index_count indices");
}

# ----------------------------------------------------------------------
# Tests

diag "";
diag "-------- Element Access (Not Slices):";

# $array[0] - single element, scalar
($expr_o, $node_id) = parse_expr('$array[0]');
verify_node_type($expr_o, $node_id, 'a_acc', "Array element access");

my $children = $expr_o->get_node_children($node_id);
my $var = $expr_o->get_a_node($children->[0]);
is($var->content(), '$array', "Element access uses \$array");

# $hash{key} - single element, scalar
($expr_o, $node_id) = parse_expr('$hash{key}');
verify_node_type($expr_o, $node_id, 'h_acc', "Hash element access");

$children = $expr_o->get_node_children($node_id);
$var = $expr_o->get_a_node($children->[0]);
is($var->content(), '$hash', "Hash access uses \$hash");


diag "";
diag "-------- Array Slices:";

# @array[0] - slice of one element
($expr_o, $node_id) = parse_expr('@array[0]');
verify_slice($expr_o, $node_id, 'slice_a_acc', '@array', 1, 
             "Array slice with 1 index");

# @array[0, 1, 2] - slice of multiple elements
($expr_o, $node_id) = parse_expr('@array[0, 1, 2]');
verify_slice($expr_o, $node_id, 'slice_a_acc', '@array', 3,
             "Array slice with 3 indices");

# @array[0..5] - slice with range
($expr_o, $node_id) = parse_expr('@array[0..5]');
verify_node_type($expr_o, $node_id, 'slice_a_acc', "Array slice with range");

$children = $expr_o->get_node_children($node_id);
$var = $expr_o->get_a_node($children->[0]);
is($var->content(), '@array', "Slice variable");

# Index should be a range operator
my $index = $expr_o->get_a_node($children->[1]);
is(ref($index), "PPI::Token::Operator", "Range operator ..");
# ok($expr_o->is_internal_node_type($index), "Slice index is internal node");
# is($index->{type}, '..', "Slice index is range operator");

# @array[$i, $j] - slice with variables
($expr_o, $node_id) = parse_expr('@array[$i, $j]');
verify_slice($expr_o, $node_id, 'slice_a_acc', '@array', 2,
             "Array slice with variable indices");


diag "";
diag "-------- Hash Slices:";

# @hash{key} - slice of one key
($expr_o, $node_id) = parse_expr('@hash{key}');
verify_slice($expr_o, $node_id, 'slice_h_acc', '@hash', 1,
             "Hash slice with 1 key");

# @hash{"key1", "key2", "key3"} - slice of multiple keys
($expr_o, $node_id) = parse_expr('@hash{"key1", "key2", "key3"}');
verify_slice($expr_o, $node_id, 'slice_h_acc', '@hash', 3,
             "Hash slice with 3 keys");

# @hash{qw(a b c)} - slice with qw
($expr_o, $node_id) = parse_expr('@hash{qw(a b c)}');
verify_node_type($expr_o, $node_id, 'slice_h_acc', "Hash slice with qw");

# @hash{$k1, $k2} - slice with variables
($expr_o, $node_id) = parse_expr('@hash{$k1, $k2}');
verify_slice($expr_o, $node_id, 'slice_h_acc', '@hash', 2,
             "Hash slice with variable keys");

# @hash{@keys} - slice with array
($expr_o, $node_id) = parse_expr('@hash{@keys}');
verify_slice($expr_o, $node_id, 'slice_h_acc', '@hash', 1,
             "Hash slice with array of keys");


diag "";
diag "-------- Assignment with Slices:";

# @result = @array[0, 2, 4]
($expr_o, $node_id) = parse_expr('@result = @array[0, 2, 4]');
verify_node_type($expr_o, $node_id, '=', "Assignment with array slice");

$children = $expr_o->get_node_children($node_id);
my $rhs = $expr_o->get_a_node($children->[1]);
is($rhs->{type}, 'slice_a_acc', "RHS is array slice");

# @values = @hash{qw(name age city)}
($expr_o, $node_id) = parse_expr('@values = @hash{qw(name age city)}');
verify_node_type($expr_o, $node_id, '=', "Assignment with hash slice");

$children = $expr_o->get_node_children($node_id);
$rhs = $expr_o->get_a_node($children->[1]);
is($rhs->{type}, 'slice_h_acc', "RHS is hash slice");


done_testing();
