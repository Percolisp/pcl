#          -*-Mode: CPerl -*-

# Test the expression parser:

use v5.32;
use strict;
use warnings;

use lib ".";

use Data::Dump qw/dump/;

use PPI;
use PPI::Dumper;


use Test::More tests => 64;
BEGIN { use_ok('Pl::PExpr') };


# XXXXX Add test for if "hanging comma" works, like '[5, 6, 7,]'


my $doc       = PPI::Document->new(\'$foo+10;');
my $expr1     = _get_ppi_part($doc);
my $expr_o    = Pl::PExpr->new(e => $expr1);
ok( $expr_o->is_var($expr1->[0]),    "is_var");
ok(!$expr_o->is_var($expr1->[1]),    "is_var 2");
ok( $expr_o->is_atomic($expr1->[0]), "is_atomic");
ok(!$expr_o->is_atomic($expr1->[1]), "is_atomic 2");
ok( $expr_o->is_number($expr1->[2]), "is_number");
ok(!$expr_o->is_number($expr1->[1]), "is_number 2");
ok( $expr_o->is_token_operator($expr1->[1]), "is_op");
ok(!$expr_o->is_token_operator($expr1->[2]), "is_op 2");

# my $code2         = '$foo->{$bar}->foobar(22, 42 + 12)';
# my $code2         = '$foo->{$bar}->[5]';
# my $code2         = '$foo->[1]->[2]';
# my $code2         = '$foo->() # No parameters break parsing?';
# my $code2         = '$foo[3][4]->(5)';
# my $code2         = '5 + foo';
# Pl::PExpr::SET_DEBUG(0);
# $doc          = PPI::Document->new(\$code2);
# $expr1        = _get_ppi_part($doc);
# say "Code: $code2";
# my $n_id      = $expr_o->parse_expr_to_tree($expr1);
# _dump_expr_vals($code2, $expr1, $expr_o, $n_id);
# Pl::PExpr::SET_DEBUG(0);
# exit 0;


my $code      = 'foobar(5 + 3, "duh", 10);';
$doc          = PPI::Document->new(\$code);
my $expr2     = _get_ppi_part($doc);

ok( $expr_o->is_word($expr2->[0]),    "is_word");
ok(!$expr_o->is_word($expr2->[1]),    "is_word 2");

# say "Code: $code"; say $expr2->[0]->content(); say dump $expr2; # exit 0;

# - - - Make a tree out of trivial exprs:
# Pl::PExpr::SET_DEBUG(1 + 2);
$code         = 'foo(10, 5)';
# $code         = '$foo->(10, 5)';
$doc          = PPI::Document->new(\$code);
my $expr      = _get_ppi_part($doc);
my $node_id   = $expr_o->parse_expr_to_tree($expr);

my($node)     = $expr_o->get_nodes($node_id);
my $kid_ids   = $expr_o->get_node_children($node_id);
my(@kid_nodes)= $expr_o->get_nodes(@$kid_ids);

is(ref $node, "PPIreference", "List object for: $code");
is($node->{type}, "funcall", "Type is 'funcall' for: $code");

is($kid_nodes[0]->content(), "foo", "Sub name: $code");
is($kid_nodes[1]->content(), "10", "1st param for: $code");
is($kid_nodes[2]->content(),  "5", "2nd param for: $code");
# Pl::PExpr::SET_DEBUG(0);

_dump_expr_vals($code, $expr, $expr_o, $node_id)
    if 0;

# - - - Zero parameters sub call:
# $Pl::PExprDEBUG_VAL = 1;
$code         = 'barf()';
$doc          = PPI::Document->new(\$code);
$expr         = _get_ppi_part($doc);
$expr_o       = Pl::PExpr->new(e => $expr);
$node_id      = $expr_o->parse_expr_to_tree($expr);

($node)       = $expr_o->get_nodes($node_id);
$kid_ids      = $expr_o->get_node_children($node_id);
@kid_nodes    = $expr_o->get_nodes(@$kid_ids);

is(ref $node, "PPIreference", "List object for: $code");
is($node->{type}, "funcall", "Type is 'funcall' for: $code");

is($kid_nodes[0]->content(), "barf", "Sub name: $code");

_dump_expr_vals($code, $expr, $expr_o, $node_id)
    if 0;

# - - - Zero parameters sub call, no parentheses:
$code         = 'foobar';
$doc          = PPI::Document->new(\$code);
$expr         = _get_ppi_part($doc);
$expr_o       = Pl::PExpr->new(e => $expr);
$node_id      = $expr_o->parse_expr_to_tree($expr);

($node)       = $expr_o->get_nodes($node_id);
$kid_ids      = $expr_o->get_node_children($node_id);
@kid_nodes    = $expr_o->get_nodes(@$kid_ids);

is(ref $node, "PPIreference", "List object for: $code");
is($node->{type}, "funcall", "Type is 'funcall' for: $code");

is($kid_nodes[0]->content(), "foobar", "Sub name: $code");

_dump_expr_vals($code, $expr, $expr_o, $node_id)
    if 0;

# - - - Commas without fun calls:
# Pl::PExpr::SET_DEBUG(1 + 8);
$code         = '5, "foo" => 10';
$doc          = PPI::Document->new(\$code);
$expr         = _get_ppi_part($doc);
$expr_o       = Pl::PExpr->new(e => $expr);
$node_id      = $expr_o->parse_expr_to_tree($expr);

($node)       = $expr_o->get_nodes($node_id);
$kid_ids      = $expr_o->get_node_children($node_id);
@kid_nodes    = $expr_o->get_nodes(@$kid_ids);

is(ref $node, "PPIreference", "List object for: $code");
is($kid_nodes[0]->content(), "5",     "1st param for: $code");
is($kid_nodes[1]->content(), '"foo"', "2nd param for: $code");
is($kid_nodes[2]->content(), "10",    "3nd param for: $code");
Pl::PExpr::SET_DEBUG(0);

_dump_expr_vals($code, $expr, $expr_o, $node_id)
    if 0;


# - - - Expr:
$code         = '$xxx * ($yyyy + 5)';
diag "-------- $code";
$doc          = PPI::Document->new(\$code);
$expr         = _get_ppi_part($doc);
$expr_o       = Pl::PExpr->new(e => $expr);

$node_id      = $expr_o->parse_expr_to_tree($expr);

($node)       = $expr_o->get_nodes($node_id);
$kid_ids      = $expr_o->get_node_children($node_id);
@kid_nodes    = $expr_o->get_nodes(@$kid_ids);
is($node->content, "*", "top op is '*'");
is($kid_nodes[0]->content, '$xxx', "1st param is \$xxx");
is(ref($kid_nodes[1]), 'PPIreference', "2nd param to '*' is tree");
# Inside the parenthese:
my $par_subf  = $expr_o->get_node_children($kid_ids->[1]);
my @par_nodes = $expr_o->get_nodes(@$par_subf);
is($par_nodes[0]->content, '+', "2nd param to '*' is '+'");

# Check the parameters for the '+':
$kid_ids      = $expr_o->get_node_children($par_subf->[0]);
@kid_nodes    = $expr_o->get_nodes(@$kid_ids);
is($kid_nodes[0]->content, '$yyyy', "first param for the '+' is \$yyyy");
is($kid_nodes[1]->content, '5', "2nd param for the '+' is 5");

_dump_expr_vals($code, $expr, $expr_o, $node_id)
    if 0;


# - - - Parse order of l-associative:
# Pl::PExpr::SET_DEBUG(1 + 8);

# l-associative:
#        +
#     +      5
#   3   4
#
$code         = '3 + 4+5';
diag "-------- $code";
$doc          = PPI::Document->new(\$code);
$expr         = _get_ppi_part($doc);
$expr_o       = Pl::PExpr->new(e => $expr);
$node_id      = $expr_o->parse_expr_to_tree($expr);

($node)       = $expr_o->get_nodes($node_id);
$kid_ids      = $expr_o->get_node_children($node_id);
@kid_nodes    = $expr_o->get_nodes(@$kid_ids);

is($node->content, "+", "top op is '+'");
is($kid_nodes[0]->content(), "+", "1st parameter for top op is '+'");
is($kid_nodes[1]->content(), '5', "2nd is '5'");

$kid_ids      = $expr_o->get_node_children($kid_ids->[0]);
@kid_nodes    = $expr_o->get_nodes(@$kid_ids);
is($kid_nodes[0]->content, '3', "1st param for the lower '+' is '3'");
is($kid_nodes[1]->content, '4', "2nd param for the lower '+' is '4'");

Pl::PExpr::SET_DEBUG(0);

_dump_expr_vals($code, $expr, $expr_o, $node_id)
    if 0;

# - - - Check parentheses:
$code         = '3 + (4+5)';
diag "-------- Check parentheses: $code";
$doc          = PPI::Document->new(\$code);
$expr         = _get_ppi_part($doc);
$expr_o       = Pl::PExpr->new(e => $expr);
$node_id      = $expr_o->parse_expr_to_tree($expr);

($node)       = $expr_o->get_nodes($node_id);
$kid_ids      = $expr_o->get_node_children($node_id);
@kid_nodes    = $expr_o->get_nodes(@$kid_ids);

is($node->content, "+", "top op is '+'");
is($kid_nodes[0]->content(), "3", "1st parameter is '3'");
is(ref($kid_nodes[1]), 'PPIreference', "2nd param to '+' is tree");
$par_subf     = $expr_o->get_node_children($kid_ids->[1]);
@par_nodes    = $expr_o->get_nodes(@$par_subf);
# is($par_nodes[0]->content, '+', "2nd param to '*' is '+': $code");


is($par_nodes[0]->content(), '+', "2nd param is '+'");

$kid_ids      = $expr_o->get_node_children($par_subf->[0]);
@kid_nodes    = $expr_o->get_nodes(@$kid_ids);
is($kid_nodes[0]->content, '4', "1st param for the 2nd '+' is '4'");
is($kid_nodes[1]->content, '5', "2nd param for the 2nd '+' is '5'");


# - - - Check parentheses II:
$code         = '3 * (4+5)';
diag "-------- Check parentheses II: $code";
$doc          = PPI::Document->new(\$code);
$expr         = _get_ppi_part($doc);
$expr_o       = Pl::PExpr->new(e => $expr);
$node_id      = $expr_o->parse_expr_to_tree($expr);

($node)       = $expr_o->get_nodes($node_id);
$kid_ids      = $expr_o->get_node_children($node_id);
@kid_nodes    = $expr_o->get_nodes(@$kid_ids);

is($node->content, "*", "top op is '*'");
is($kid_nodes[0]->content(), "3", "1st parameter is '3'");
is(ref($kid_nodes[1]), 'PPIreference', "2nd param to '*' is tree");
# Inside the parenthese:
$par_subf     = $expr_o->get_node_children($kid_ids->[1]);
@par_nodes    = $expr_o->get_nodes(@$par_subf);
is($par_nodes[0]->content, '+', "2nd param to '*' is '+'");

$kid_ids      = $expr_o->get_node_children($par_subf->[0]);
@kid_nodes    = $expr_o->get_nodes(@$kid_ids);
is($kid_nodes[0]->content, '4', "1st param for the 2nd '+' is '4'");
is($kid_nodes[1]->content, '5', "2nd param for the 2nd '+' is '5'");


test_expr('$foo . "bar" . $duh',
          ['.',
           ['.', '$foo', '"bar"'],
           '$duh',]
       );

test_expr('$foo ** 3 ** 4', ['**', '$foo', ['**', 3, 4]]); # right associative
test_expr('$foo - 3 - 4', ['-', ['-', '$foo', 3],  4]);    # left  associative


exit 0;
# $code         = '$q= 1 + $w=$z # Bad, need parentheses';
# $code         = '$q= 1 + ($w=$z)';


# $code         = '10 - 1 - 2';
# $doc          = PPI::Document->new(\$code);
# $expr         = _get_ppi_part($doc);
# $expr_o       = Pl::PExpr->new(e => $expr);
# $node_id      = $expr_o->parse_expr_to_tree($expr);

# _dump_expr_vals($code, $expr, $expr_o, $node_id)
#     if 1;
# exit 0;


# ----------------------------------------------------------------------
# Utils:

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



# Help util, get parts of statement X in some PPI Document:
sub _get_ppi_part {
  my $doc     = shift;
  my $stmt_ix = shift // 0;

  my @stmts;
  my @parts;
  if (ref($doc) eq 'PPI::Document') {
    @stmts    = $doc->children(); # Just get first...
    @parts    = $stmts[$stmt_ix]->children();
  } elsif (ref($doc) eq 'PPI::Statement') {
    @parts    = $doc->children();
  } else {
    die "Code is not PPI::Document?? Is: " . ref($doc);
  }

  return \@parts;
}



sub test_expr {
  my $code    = shift;
  my $tst_spec= shift;
  my $debuglvl= shift // 0;

  diag "Parsing: $code";
  my $doc     = PPI::Document->new(\$code);
  my $expr    = _get_ppi_part($doc);
  my $expr_o  = Pl::PExpr->new(e => $expr);

  Pl::PExpr::SET_DEBUG($debuglvl);
  my $node_id = $expr_o->parse_expr_to_tree($expr);
  _test_expr_2($node_id, $expr_o, $tst_spec, $code);
  Pl::PExpr::SET_DEBUG(0);
}

sub _test_expr_2 {
  my $node_id = shift;
  my $expr_o  = shift;
  my $tst_spec= shift;
  my $code    = shift;
  my $level   = shift // 0;

  return
      if ! defined $tst_spec || scalar(@$tst_spec) == 0;

  my $spaces  = " " x ($level * 2);

  my $n_spec  = $tst_spec->[0];
  (my $node)  = $expr_o->get_nodes($node_id);
  my $content;
  my $msg;
  if ($expr_o->is_internal_node_type($node)) {
    $content  = $node->{type};
    $msg      = "Type $content";
  } else {
    $content  = $node->content();
    $msg      = "Expect $content";
  }
  is($content, $n_spec, "${spaces}Code ${code}: $msg");

  # XXXX Check all parameters recursively.
  my $kid_ids = $expr_o->get_node_children($node_id);

  for(my $i=1; $i < scalar(@$tst_spec); $i++) {
    my $t_part= $tst_spec->[$i];
    next
        if ! defined $t_part;     # Didn't specify a test for this.
    my $kid_id= $kid_ids->[$i-1]; # Off by one for kids.
    if (ref($t_part) eq 'ARRAY') {
      _test_expr_2($kid_id, $expr_o, $t_part, $code, $level + 1);
      next;
    }
    (my $child_node)  = $expr_o->get_nodes($kid_id);
    my $content;
    my $msg;
    if ($expr_o->is_internal_node_type($child_node)) {
      $content  = $child_node->{type};
      $msg      = "Type $content";
    } else {
      $content  = $child_node->content();
      $msg      = "Expect $content";
    }
    is($content, $t_part, "  ${spaces}${code}: $msg");
  }
}



# Suggestion:
# _test_expr($code, $expr_o, $node_id, [ list of test descriptions]);
# It should return so it is easy to call it recursively for lower
# parts in the tree.

# Is this a win??
# Instead just send in code and tests??
# Specify the test values as ... ???

# Not good enough.

sub _test_expr {
  my $code    = shift;
  my $expr_o  = shift;
  my $node_id = shift;
  my $tests   = shift;

  (my $node)  = $expr_o->get_nodes($node_id);
  my $kid_ids = $expr_o->get_node_children($node_id);
  my @kid_ns  = $expr_o->get_nodes(@$kid_ids);

  for my $tst (@$tests) {
    my($type, $val_spec, $expected, $msg) = @$tst;

    if ($type eq 'is') {
      # XXXX Do eval to get value to compare??
    } elsif ($type eq 'ok') {

    }
  }
}

