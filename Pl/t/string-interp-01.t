#          -*-Mode: CPerl -*-

# Test string interpolation in the expression parser

use v5.32;
use strict;
use warnings;

use lib ".";

use Data::Dump qw/dump/;

use PPI;
use PPI::Dumper;

use Test::More tests => 102;
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

sub verify_string_concat {
  my ($expr_o, $node_id, $expected_parts, $msg) = @_;
  
  my $node = $expr_o->get_a_node($node_id);
  
  # Check if it's a string_concat node
  if ($expr_o->is_internal_node_type($node)) {
    is($node->{type}, 'string_concat', "$msg - is string_concat");
    
    my $children = $expr_o->get_node_children($node_id);
    is(scalar(@$children), scalar(@$expected_parts), 
       "$msg - has correct number of parts");

    # Test the tests:
    if (scalar(@$children) != scalar(@$expected_parts)) {
      say dump $children;
      say dump $expected_parts;
      exit 0;
    }

    for my $i (0 .. $#{$expected_parts}) {
      my $child_node = $expr_o->get_a_node($children->[$i]);
      my $expected = $expected_parts->[$i];
      
      if ($expected =~ /^"/) {
        # Expecting a string literal
        ok($child_node->can('content'), "$msg - part $i is a token");
        # Compare content (with quotes)
        is($child_node->content(), $expected, "$msg - part $i content");
      } elsif ($expected =~ /^[\$\@%]/) {
        # Expecting a variable
        ok($child_node->can('content'), "$msg - part $i is a token");
        is($child_node->content(), $expected, "$msg - part $i is variable");
      } else {
        # Expecting some other node type
        if ($expr_o->is_internal_node_type($child_node)) {
          is($child_node->{type}, $expected, "$msg - part $i is $expected");
        }
      }
    }
  } else {
    fail("$msg - expected string_concat node");
  }
}

# ----------------------------------------------------------------------
# Tests

diag "";
diag "-------- Plain Strings (No Interpolation):";

# Single-quoted strings never interpolate
($expr_o, $node_id) = parse_expr("'Hello \$name'");
my $node = $expr_o->get_a_node($node_id);
ok(!$expr_o->is_internal_node_type($node),
   "Single-quoted string is not interpolated");
is($node->content(), "'Hello \$name'",
   "Single-quoted string content preserved");

# Double-quoted without variables
($expr_o, $node_id) = parse_expr('"Hello World"');
$node = $expr_o->get_a_node($node_id);
ok(!$expr_o->is_internal_node_type($node),
   "Plain double-quoted string is not interpolated");
is($node->content(), '"Hello World"', "Plain string content preserved");


diag "";
diag "-------- Simple Variable Interpolation:";

# Simple scalar variable
($expr_o, $node_id) = parse_expr('"Hello $name"');
 # say "|" x 70, "Node tree:"; say dump $expr_o->node_tree();
verify_string_concat($expr_o, $node_id, ['"Hello "', '$name'], 
                     "Simple scalar variable");

# Variable at start
($expr_o, $node_id) = parse_expr('"$greeting World"');
verify_string_concat($expr_o, $node_id, ['$greeting', '" World"'], 
                     "Variable at start");

# Variable at end
($expr_o, $node_id) = parse_expr('"Hello $name"');
verify_string_concat($expr_o, $node_id, ['"Hello "', '$name'], 
                     "Variable at end");

# Multiple variables
($expr_o, $node_id) = parse_expr('"$first $last"');
verify_string_concat($expr_o, $node_id, ['$first', '" "', '$last'], 
                     "Multiple variables");

# Just a variable
($expr_o, $node_id) = parse_expr('"$var"');
$node = $expr_o->get_a_node($node_id);
ok($node->can('content'), "Just a variable returns variable node");
is($node->content(), '$var', "Variable content correct");


diag "";
diag "-------- Array Variable Interpolation:";

# Array variable (joins with $")
($expr_o, $node_id) = parse_expr('"Array: @array"');
verify_string_concat($expr_o, $node_id, ['"Array: "', '@array'], 
                     "Array variable");

# Multiple array variables
($expr_o, $node_id) = parse_expr('"@arr1 and @arr2"');
verify_string_concat($expr_o, $node_id, ['@arr1', '" and "', '@arr2'], 
                     "Multiple arrays");


diag "";
diag "-------- Array Element Interpolation:";

# Array element with numeric index
($expr_o, $node_id) = parse_expr('"First: $array[0]"');
$node = $expr_o->get_a_node($node_id);
ok($expr_o->is_internal_node_type($node), "Array element creates concat node");
is($node->{type}, 'string_concat', "Array element is string_concat");

my $children = $expr_o->get_node_children($node_id);
is(scalar(@$children), 2, "Array element has 2 parts");

# First part should be literal
my $part1 = $expr_o->get_a_node($children->[0]);
is($part1->content(), '"First: "', "Array element literal part");

# Second part should be array access
my $part2 = $expr_o->get_a_node($children->[1]);
ok($expr_o->is_internal_node_type($part2), "Array access is internal node");
is($part2->{type}, 'a_acc', "Array access type");

# Array element with variable index
($expr_o, $node_id) = parse_expr('"Value: $array[$i]"');
$node = $expr_o->get_a_node($node_id);
is($node->{type}, 'string_concat', "Array element with variable index");


diag "";
diag "-------- Hash Element Interpolation:";

# Hash element with bareword key
($expr_o, $node_id) = parse_expr('"Name: $hash{name}"');
$node = $expr_o->get_a_node($node_id);
ok($expr_o->is_internal_node_type($node), "Hash element creates concat node");
is($node->{type}, 'string_concat', "Hash element is string_concat");

$children = $expr_o->get_node_children($node_id);
is(scalar(@$children), 2, "Hash element has 2 parts");

# Second part should be hash access
my $hash_acc = $expr_o->get_a_node($children->[1]);
ok($expr_o->is_internal_node_type($hash_acc), "Hash access is internal node");
is($hash_acc->{type}, 'h_acc', "Hash access type");

# Hash element with variable key
($expr_o, $node_id) = parse_expr('"Value: $hash{$key}"');
$node = $expr_o->get_a_node($node_id);
is($node->{type}, 'string_concat', "Hash element with variable key");


diag "";
diag "-------- Braced Expression Interpolation:";

# Simple expression
($expr_o, $node_id) = parse_expr('"Result: ${$x + $y}"');
$node = $expr_o->get_a_node($node_id);
is($node->{type}, 'string_concat', "Braced expression");

$children = $expr_o->get_node_children($node_id);
is(scalar(@$children), 2, "Braced expression has 2 parts");

# Second part should be the expression
my $expr_part = $expr_o->get_a_node($children->[1]);
ok(defined $expr_part, "Expression part exists");

# Complex expression
($expr_o, $node_id) = parse_expr('"Sum: ${$x * 2 + $y}"');
$node = $expr_o->get_a_node($node_id);
is($node->{type}, 'string_concat', "Complex braced expression");


diag "";
diag "-------- Escape Sequences:";

# Escaped dollar sign
($expr_o, $node_id) = parse_expr('"Price: \$5.00"');
$node = $expr_o->get_a_node($node_id);
ok($node->can('content'), "Escaped dollar is not interpolated");
is($node->content(), '"Price: \$5.00"', "Escaped dollar preserved");

# Escaped at sign
($expr_o, $node_id) = parse_expr('"Email: user\@example.com"');
$node = $expr_o->get_a_node($node_id);
ok($node->can('content'), "Escaped at is not interpolated");

# Newline escape
($expr_o, $node_id) = parse_expr('"Line1\\nLine2"');
$node = $expr_o->get_a_node($node_id);
ok($node->can('content'), "Newline escape is not interpolated");


diag "";
diag "-------- Edge Cases:";

# Empty string
($expr_o, $node_id) = parse_expr('""');
$node = $expr_o->get_a_node($node_id);
ok($node->can('content'), "Empty string");
is($node->content(), '""', "Empty string content");

# String with only spaces
($expr_o, $node_id) = parse_expr('"   "');
$node = $expr_o->get_a_node($node_id);
ok($node->can('content'), "String with spaces");
is($node->content(), '"   "', "Spaces preserved");

# Variable with underscore
($expr_o, $node_id) = parse_expr('"Value: $my_var"');
verify_string_concat($expr_o, $node_id, ['"Value: "', '$my_var'], 
                     "Variable with underscore");

# Multiple interpolations
($expr_o, $node_id) = parse_expr('"$a$b$c"');
verify_string_concat($expr_o, $node_id, ['$a', '$b', '$c'], 
                     "Consecutive variables");


diag "";
diag "-------- Complex Cases:";

# Mix of literals and variables
($expr_o, $node_id) = parse_expr('"The value of $var is $value here"');
verify_string_concat($expr_o, $node_id, 
                     ['"The value of "', '$var', '" is "', '$value', '" here"'],
                     "Mix of literals and variables");

# Array element and hash element
($expr_o, $node_id) = parse_expr('"Array[$i]=$arr[$i], Hash{k}=$h{k}"');
$node = $expr_o->get_a_node($node_id);
is($node->{type}, 'string_concat', "Complex with array and hash elements");


diag "";
diag "-------- Regression Tests:";

# Make sure regular expressions still work
($expr_o, $node_id) = parse_expr('/pattern/');
$node = $expr_o->get_a_node($node_id);
is $node->{type}, "=~", "Simple regexp gets '\$_ =~` prefixed";

# Make sure operators still work in strings without variables
($expr_o, $node_id) = parse_expr('"2 + 2"');
$node = $expr_o->get_a_node($node_id);
ok($node->can('content'), "Operators in plain string");
is($node->content(), '"2 + 2"', "Plain string with operators");


done_testing();


# - -  - Just testing:
sub _dump_expr_vals {
  my($code, $expr, $expr_o, $node_id) = @_;

  (my $node)  = $expr_o->get_nodes($node_id);
  my $kid_ids = $expr_o->get_node_children($node_id);
  my @kid_ns  = $expr_o->get_nodes(@$kid_ids);

  say "/" x 50, " Dump of expr";
  say "------- Node $node_id, dumping result for: $code";
  say "Full expr in PPI:";
  dump $expr;
  say "X" x 70;
  say "Top node: ", dump $node;
  say "Node tree object:";
  say dump $expr_o->node_tree();
  say "Tree hierarchy:";
  say $expr_o->debug_dump_tree($node_id);
  say "Objects that are children of the top node:";
  say dump \@kid_ns;
  say "/" x 50, " End of Dump";
}
