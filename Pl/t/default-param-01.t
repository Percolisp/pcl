#          -*-Mode: CPerl -*-

# Test default $_ parameter handling

use v5.32;
use strict;
use warnings;

use lib ".";

use Data::Dump qw/dump/;

use PPI;
use PPI::Dumper;

use Test::More tests => 48;
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
      full_PPI => $doc,  # Keep PPI document alive
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

sub verify_funcall_with_param {
  my ($expr_o, $node_id, $func_name, $param_count, $msg) = @_;
  
  my $node = $expr_o->get_a_node($node_id);
  
  ok($expr_o->is_internal_node_type($node), "$msg - is internal node");
  is($node->{type}, 'funcall', "$msg - is funcall");
  
  my $children = $expr_o->get_node_children($node_id);
  
  # First child is function name
  my $func_node = $expr_o->get_a_node($children->[0]);
  is($func_node->content(), $func_name, "$msg - function name");
  
  # Check parameter count (excluding function name)
  is(scalar(@$children) - 1, $param_count, "$msg - parameter count");
}

sub verify_has_underscore_param {
  my ($expr_o, $node_id, $msg) = @_;
  
  my $children = $expr_o->get_node_children($node_id);
  
  # Should have 2 children: function name + $_
  is(scalar(@$children), 2, "$msg - has 2 children (func + param)");
  
  # Second child should be $_
  my $param_node = $expr_o->get_a_node($children->[1]);
  ok($param_node->can('content'), "$msg - parameter is a token");
  is($param_node->content(), '$_', "$msg - parameter is \$_");
}

# ----------------------------------------------------------------------
# Tests

diag "";
diag "-------- Functions with Implicit \$_:";

# chomp with no args should get $_
($expr_o, $node_id) = parse_expr('chomp');
verify_funcall_with_param($expr_o, $node_id, 'chomp', 1, "chomp with no args");
verify_has_underscore_param($expr_o, $node_id, "chomp gets implicit \$_");

# chomp with explicit arg should NOT get $_
($expr_o, $node_id) = parse_expr('chomp($x)');
verify_funcall_with_param($expr_o, $node_id, 'chomp', 1, "chomp with explicit arg");
my $children = $expr_o->get_node_children($node_id);
my $param = $expr_o->get_a_node($children->[1]);
is($param->content(), '$x', "chomp uses explicit \$x, not \$_");

# lc with no args
($expr_o, $node_id) = parse_expr('lc');
verify_funcall_with_param($expr_o, $node_id, 'lc', 1, "lc with no args");
verify_has_underscore_param($expr_o, $node_id, "lc gets implicit \$_");

# uc with no args
($expr_o, $node_id) = parse_expr('uc');
verify_funcall_with_param($expr_o, $node_id, 'uc', 1, "uc with no args");
verify_has_underscore_param($expr_o, $node_id, "uc gets implicit \$_");

# length with no args
($expr_o, $node_id) = parse_expr('length');
verify_funcall_with_param($expr_o, $node_id, 'length', 1, "length with no args");
verify_has_underscore_param($expr_o, $node_id, "length gets implicit \$_");


diag "";
diag "-------- Standalone Regex (implies \$_ =~):";

# Standalone regex should become $_ =~ /pattern/
($expr_o, $node_id) = parse_expr('/pattern/');
my $node = $expr_o->get_a_node($node_id);
ok($expr_o->is_internal_node_type($node), "Standalone regex creates operator node");
is($node->{type}, '=~', "Standalone regex becomes =~ operator");

$children = $expr_o->get_node_children($node_id);
is(scalar(@$children), 2, "=~ has 2 operands");

# Left operand should be $_
my $left = $expr_o->get_a_node($children->[0]);
is($left->content(), '$_', "Left operand of =~ is \$_");

# Right operand should be regex
my $right = $expr_o->get_a_node($children->[1]);
ok($right->can('content'), "Right operand is a token");
like($right->content(), qr/pattern/, "Right operand is the regex");


diag "";
diag "-------- Functions WITHOUT Implicit \$_:";

# Functions that don't default to $_ should work normally
($expr_o, $node_id) = parse_expr('time');
verify_funcall_with_param($expr_o, $node_id, 'time', 0, "time takes 0 params");

($expr_o, $node_id) = parse_expr('rand');
verify_funcall_with_param($expr_o, $node_id, 'rand', 0, "rand with no args (no \$_)");


done_testing();
