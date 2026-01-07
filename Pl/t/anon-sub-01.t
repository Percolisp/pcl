#          -*-Mode: CPerl -*-

# Test anonymous subroutines in map/grep/sort

use v5.32;
use strict;
use warnings;

use lib ".";

use Data::Dump qw/dump/;

use PPI;
use PPI::Dumper;

use Test::More tests => 29;
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

sub verify_funcall {
  my ($expr_o, $node_id, $func_name, $msg) = @_;
  
  my $node = $expr_o->get_a_node($node_id);
  ok($expr_o->is_internal_node_type($node), "$msg - is internal node");
  is($node->{type}, 'funcall', "$msg - is funcall");
  
  my $children = $expr_o->get_node_children($node_id);
  my $func_node = $expr_o->get_a_node($children->[0]);
  is($func_node->content(), $func_name, "$msg - function name is $func_name");
  
  return $children;
}

sub verify_has_anon_sub {
  my ($expr_o, $children, $msg) = @_;
  
  # Second child should be anon_sub (first arg to map/grep/sort)
  ok(scalar(@$children) >= 2, "$msg - has at least 2 children");
  
  my $block_node = $expr_o->get_a_node($children->[1]);
  ok($expr_o->is_internal_node_type($block_node), "$msg - first arg is internal node");
  is($block_node->{type}, 'anon_sub', "$msg - first arg is anon_sub");
}

# ----------------------------------------------------------------------
# Tests

diag "";
diag "-------- map with Block:";

# map { $_ * 2 } @array
($expr_o, $node_id) = parse_expr('map { $_ * 2 } @array');
my $children = verify_funcall($expr_o, $node_id, 'map', "map with block");
verify_has_anon_sub($expr_o, $children, "map has anon_sub");

# Check that it has 3 children: map, block, @array
is(scalar(@$children), 3, "map has 3 children (func, block, array)");


diag "";
diag "-------- grep with Block:";

# grep { $_ > 5 } @list
($expr_o, $node_id) = parse_expr('grep { $_ > 5 } @list');
$children = verify_funcall($expr_o, $node_id, 'grep', "grep with block");
verify_has_anon_sub($expr_o, $children, "grep has anon_sub");

is(scalar(@$children), 3, "grep has 3 children (func, block, list)");


diag "";
diag "-------- sort with Block:";

# sort { $a <=> $b } @numbers
($expr_o, $node_id) = parse_expr('sort { $a <=> $b } @numbers');
$children = verify_funcall($expr_o, $node_id, 'sort', "sort with block");
verify_has_anon_sub($expr_o, $children, "sort has anon_sub");

is(scalar(@$children), 3, "sort has 3 children (func, block, numbers)");


diag "";
diag "-------- Hash vs Block Disambiguation:";

# Hash constructor (has =>)
($expr_o, $node_id) = parse_expr('{ key => value }');
my $node = $expr_o->get_a_node($node_id);
is($node->{type}, 'hash_init', "Hash with => is hash_init, not anon_sub");

# Block in map (no =>)
($expr_o, $node_id) = parse_expr('map { $_ * 2 } @x');
$children = $expr_o->get_node_children($node_id);
my $block = $expr_o->get_a_node($children->[1]);
is($block->{type}, 'anon_sub', "Block without => becomes anon_sub in map");


diag "";
diag "-------- Block Metadata:";

# TODO: Block metadata tracking not yet implemented
# The get_metadata call would need to be on $expr_o->node_tree
# and the block_context would need to be set during parsing
SKIP: {
  skip "Block metadata not yet implemented", 1;
  ($expr_o, $node_id) = parse_expr('map { $_ } @x');
  $children = $expr_o->get_node_children($node_id);
  my $block_id = $children->[1];
  my $metadata = $expr_o->node_tree->get_metadata($block_id, 'block_context');
  is($metadata, 'map', "Block metadata records function context");
}


diag "";
diag "-------- Non-Block Functions:";

# Regular function calls should not create anon_sub
($expr_o, $node_id) = parse_expr('push @array, $value');
$children = verify_funcall($expr_o, $node_id, 'push', "push is regular funcall");

# First arg should NOT be anon_sub
my $first_arg = $expr_o->get_a_node($children->[1]);
ok(!($expr_o->is_internal_node_type($first_arg) && $first_arg->{type} eq 'anon_sub'),
   "push first arg is not anon_sub");
