#          -*-Mode: CPerl -*-

# Test the expression parser:

use v5.32;
use strict;
use warnings;

use lib ".";

use Data::Dump qw/dump/;

use PPI;
use PPI::Dumper;


use Test::More tests => 1;
BEGIN { use_ok('Pl::PExpr') };

my $code;
my $doc;
my $expr_o    = Pl::PExpr->new();
my $expr;
my $node_id;
my $node;
my $kid_ids;
my @kid_nodes;


my $n_id;
$code         = '$x < $y < $z and $q';
Pl::PExpr::SET_DEBUG(0);
$doc          = PPI::Document->new(\$code);
$expr         = _get_ppi_part($doc);
say "Code: $code";
$expr_o       = Pl::PExpr->new();
$n_id         = $expr_o->parse_expr_to_tree($expr);
# say dump $expr_o; exit 0;
_dump_expr_vals($code, $expr, $expr_o, $n_id);
Pl::PExpr::SET_DEBUG(0);


exit 0;
$code         = '-f "foo" . $bar';
# $code         = '!$foo';
# $code         = 'funny($foo)->barf(5)';
Pl::PExpr::SET_DEBUG(0);
$doc          = PPI::Document->new(\$code);
$expr         = _get_ppi_part($doc);
say "Code: $code";
$expr_o       = Pl::PExpr->new();
$n_id         = $expr_o->parse_expr_to_tree($expr);
# say dump $expr_o; exit 0;
_dump_expr_vals($code, $expr, $expr_o, $n_id);
Pl::PExpr::SET_DEBUG(0);


# ----------------------------------------------------------------------
# Utils:

# XXXX Need a simple way to write texts for test message, like:
#    '#Get object to call method for', ['h_ref_acc', '$foo', '$bar'],
#    '#Name of method', 'foobar',
#    ...


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

