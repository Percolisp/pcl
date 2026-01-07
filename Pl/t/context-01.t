#          -*-Mode: CPerl -*-

# Test context annotation in the expression parser

use v5.32;
use strict;
use warnings;

use lib ".";

use Data::Dump qw/dump/;

use PPI;
use PPI::Dumper;

use Test::More tests => 47;
BEGIN { use_ok('Pl::PExpr', qw(SCALAR_CTX LIST_CTX VOID_CTX)) };

my $code;
my $doc;
my $expr_o;
my $expr;
my $node_id;

# ----------------------------------------------------------------------
# Helper functions

sub parse_and_annotate {
  my $code    = shift;
  my $ctx     = shift // SCALAR_CTX;

  diag "Code: $code";
  my $doc     = PPI::Document->new(\$code);
  my $expr    = _get_ppi_part($doc);
  my $expr_o  = Pl::PExpr->new(e => $expr);

  my $node_id = $expr_o->parse_expr_to_tree($expr);
  $expr_o->annotate_contexts($node_id, $ctx);

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

sub test_node_ctxt {
  my ($expr_o, $node_id, $expected_ctx, $msg) = @_;
  
  my $actual_ctx = $expr_o->get_node_context($node_id);
  is($actual_ctx, $expected_ctx, $msg);
}

# ----------------------------------------------------------------------
# Tests

diag "";
diag "-------- Basic Context Tests:";

# Simple scalar expression
($expr_o, $node_id) = parse_and_annotate('$x + $y', SCALAR_CTX);
test_node_ctxt($expr_o, $node_id, SCALAR_CTX, "Top-level + in scalar context");

my $kids = $expr_o->get_node_children($node_id);
test_node_ctxt($expr_o, $kids->[0], SCALAR_CTX, "Left op  + inherits scalar");
test_node_ctxt($expr_o, $kids->[1], SCALAR_CTX, "Right op + inherits scalar");

# Simple list expression
($expr_o, $node_id) = parse_and_annotate('$x + $y', LIST_CTX);
test_node_ctxt($expr_o, $node_id, LIST_CTX, "Top-level + in list context");

$kids = $expr_o->get_node_children($node_id);
test_node_ctxt($expr_o, $kids->[0], LIST_CTX, "Left op + inherits list");
test_node_ctxt($expr_o, $kids->[1], LIST_CTX, "Right op + inherits list");


diag "";
diag "-------- Assignment Context Tests:";

# Scalar assignment: RHS should be scalar
($expr_o, $node_id) = parse_and_annotate('$x = @array');
test_node_ctxt($expr_o, $node_id, SCALAR_CTX, "Assignment in scalar context");

$kids = $expr_o->get_node_children($node_id);
test_node_ctxt($expr_o, $kids->[0], SCALAR_CTX, "LHS of assignment is scalar");
test_node_ctxt($expr_o, $kids->[1], SCALAR_CTX,
	       "RHS of scalar assignment is scalar");

# Array assignment: RHS should be list
($expr_o, $node_id) = parse_and_annotate('@x = @array');
test_node_ctxt($expr_o, $node_id, SCALAR_CTX, "Array assignment expression");

$kids = $expr_o->get_node_children($node_id);
test_node_ctxt($expr_o, $kids->[0], LIST_CTX,
	       "LHS of array assignment is list");
test_node_ctxt($expr_o, $kids->[1], LIST_CTX,
	       "RHS of array assignment is list");

# Hash assignment: RHS should be list  
($expr_o, $node_id) = parse_and_annotate('%hash = @array');
$kids = $expr_o->get_node_children($node_id);
test_node_ctxt($expr_o, $kids->[0], LIST_CTX, "LHS of hash assignment is list");
test_node_ctxt($expr_o, $kids->[1], LIST_CTX, "RHS of hash assignment is list");

# Chained assignment
($expr_o, $node_id) = parse_and_annotate('$x = $y = $z');
test_node_ctxt($expr_o, $node_id, SCALAR_CTX, "Outer assignment");

$kids = $expr_o->get_node_children($node_id);
my $inner_assign_id = $kids->[1];
test_node_ctxt($expr_o, $inner_assign_id, SCALAR_CTX,
	       "Inner assignment in scalar context");

my $inner_children = $expr_o->get_node_children($inner_assign_id);
test_node_ctxt($expr_o, $inner_children->[1], SCALAR_CTX,
	       "Innermost RHS in scalar context");


diag "";
diag "-------- Ternary Operator Context:";

($expr_o, $node_id) = parse_and_annotate('$x ? $y : $z', SCALAR_CTX);
test_node_ctxt($expr_o, $node_id, SCALAR_CTX, "Ternary in scalar context");

$kids = $expr_o->get_node_children($node_id);
test_node_ctxt($expr_o, $kids->[0], SCALAR_CTX, "Ternary condition is scalar");
test_node_ctxt($expr_o, $kids->[1], SCALAR_CTX,
	       "Ternary true branch inherits scalar");
test_node_ctxt($expr_o, $kids->[2], SCALAR_CTX,
	       "Ternary false branch inherits scalar");

($expr_o, $node_id) = parse_and_annotate('$x ? $y : $z', LIST_CTX);
test_node_ctxt($expr_o, $node_id, LIST_CTX, "Ternary in list context");

$kids = $expr_o->get_node_children($node_id);
test_node_ctxt($expr_o, $kids->[0], SCALAR_CTX,
	       "Ternary condition always scalar");
test_node_ctxt($expr_o, $kids->[1], LIST_CTX,
	       "Ternary true branch inherits list");
test_node_ctxt($expr_o, $kids->[2], LIST_CTX,
	       "Ternary false branch inherits list");


diag "";
diag "-------- Array/Hash Constructor Context:";

($expr_o, $node_id) = parse_and_annotate('[1, 2, 3]', SCALAR_CTX);
test_node_ctxt($expr_o, $node_id, SCALAR_CTX,
	       "Array constructor in scalar context");

$kids = $expr_o->get_node_children($node_id);
test_node_ctxt($expr_o, $kids->[0], LIST_CTX,
	       "Array constructor elements in list context");

($expr_o, $node_id) = parse_and_annotate('{a => 1, b => 2}', SCALAR_CTX);
test_node_ctxt($expr_o, $node_id, SCALAR_CTX,
	       "Hash constructor in scalar context");

$kids = $expr_o->get_node_children($node_id);
test_node_ctxt($expr_o, $kids->[0], LIST_CTX,
	       "Hash constructor elements in list context");


diag "";
diag "-------- Comma/Progn Context:";

($expr_o, $node_id) = parse_and_annotate('$x, $y, $z', SCALAR_CTX);
test_node_ctxt($expr_o, $node_id, SCALAR_CTX, "',' expression in scalar ctxt");

$kids = $expr_o->get_node_children($node_id);
test_node_ctxt($expr_o, $kids->[0], LIST_CTX, "1st el of ',' list in LIST_CTX");
test_node_ctxt($expr_o, $kids->[1], LIST_CTX, "2nd el of ',' list in LIST_CTX");
test_node_ctxt($expr_o, $kids->[2], LIST_CTX, "3rd el of ',' list in LIST_CTX");


diag "";
diag "-------- Function Call Context:";

# Note: These tests may need adjustment based on how function calls
# are represented in your tree. Testing basic principle.

# Regular function: arguments inherit
($expr_o, $node_id) = parse_and_annotate('foo($x, $y)', SCALAR_CTX);
test_node_ctxt($expr_o, $node_id, SCALAR_CTX, "funcall in scalar context");

# Could add tests for grep/map forcing list context on their list argument
# once those are fully implemented


diag "";
diag "-------- Nested Context Propagation:";

# Complex nested expression
($expr_o, $node_id) = parse_and_annotate('$x + ($y * $z)', SCALAR_CTX);
test_node_ctxt($expr_o, $node_id, SCALAR_CTX, "Outer + in scalar");

$kids = $expr_o->get_node_children($node_id);
test_node_ctxt($expr_o, $kids->[0], SCALAR_CTX, "\$x inherits scalar");

my $mult_id = $kids->[1];
# The tree_val node wraps the parenthesized expression
my $mult_children = $expr_o->get_node_children($mult_id);
if (scalar(@$mult_children) > 0) {
  my $actual_mult = $mult_children->[0];
  test_node_ctxt($expr_o, $actual_mult, SCALAR_CTX, "() * inherits scalar");
  
  my $mult_operands = $expr_o->get_node_children($actual_mult);
  test_node_ctxt($expr_o, $mult_operands->[0], SCALAR_CTX,
		 "\$y inherits scalar");
  test_node_ctxt($expr_o, $mult_operands->[1], SCALAR_CTX,
		 "\$z inherits scalar");
}


diag "";
diag "-------- Assignment in Expression:";

# Assignment within larger expression
($expr_o, $node_id) = parse_and_annotate('($x = 5) + 2', SCALAR_CTX);
test_node_ctxt($expr_o, $node_id, SCALAR_CTX, "Outer + expression");

$kids = $expr_o->get_node_children($node_id);
# First child should be the tree_val containing the assignment
my $tree_val_id = $kids->[0];
my $tree_val_children = $expr_o->get_node_children($tree_val_id);
if (scalar(@$tree_val_children) > 0) {
  my $assign_id = $tree_val_children->[0];
  test_node_ctxt($expr_o, $assign_id, SCALAR_CTX,
		 "Assignment within expression inherits scalar");
  
  my $assign_children = $expr_o->get_node_children($assign_id);
  test_node_ctxt($expr_o, $assign_children->[1], SCALAR_CTX,
		 "RHS of nested assignment is scalar");
}


diag "";
diag "-------- Context Name Helper:";

is($expr_o->context_name(SCALAR_CTX), 'SCALAR', "SCALAR_CTX name");
is($expr_o->context_name(LIST_CTX), 'LIST', "LIST_CTX name");
is($expr_o->context_name(VOID_CTX), 'VOID', "VOID_CTX name");
is($expr_o->context_name(999), 'UNKNOWN', "Unknown context name");


done_testing();
