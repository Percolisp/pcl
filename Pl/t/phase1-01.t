#          -*-Mode: CPerl -*-

# Test Phase 1 new features:
# - Defined-or operator (//)
# - Range operator (..)
# - File test operators
# - Implicit $_ handling

use v5.32;
use strict;
use warnings;

use lib ".";

use Data::Dump qw/dump/;

use PPI;
use PPI::Dumper;

use Test::More;
BEGIN { use_ok('Pl::PExpr') };

my $code;
my $doc;
my $expr_o;
my $expr;
my $node_id;
my $node;
my $kid_ids;
my @kid_nodes;

# ----------------------------------------------------------------------
# Helper functions

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

  # Check all parameters recursively
  my $kid_ids = $expr_o->get_node_children($node_id);

  for(my $i=1; $i < scalar(@$tst_spec); $i++) {
    my $t_part= $tst_spec->[$i];
    next
        if ! defined $t_part;
    my $kid_id= $kid_ids->[$i-1];
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

# ----------------------------------------------------------------------
# Tests

diag "";
diag "-------- Defined-Or Operator (//):";

# Basic defined-or
test_expr('$x // $y', ['//', '$x', '$y']);

# With undef
test_expr('$x // 0', ['//', '$x', 0]);

# Chained (left-associative)
test_expr('$a // $b // $c', ['//', ['//', '$a', '$b'], '$c']);

# In expression
test_expr('$x + ($y // 5)', ['+', '$x', ['tree_val', ['//', '$y', 5]]]);

# With hash access
test_expr('$hash{key} // "default"',
          ['//',
           ['h_acc', '$hash', '"key"'],
           '"default"']);


diag "";
diag "-------- Range Operator (..):";

# Basic range
test_expr('1..10', ['..', 1, 10]);

# With variables
test_expr('$start..$end', ['..', '$start', '$end']);

# In array slice (range used as list)
test_expr('@array[0..5]',
          ['slice_a_acc', '@array', ['..', 0, 5]]);

# Range in expression
test_expr('($a..$b)', ['..', '$a', '$b']);

# Range precedence: || has higher precedence than ..
# $x || 1..10 should parse as ($x || 1) .. 10
test_expr('$x || 1..10', ['..', ['||', '$x', 1], 10]);

# Range precedence: // has higher precedence than ..
test_expr('$x // 1..10', ['..', ['//', '$x', 1], 10]);

# Range precedence: && has higher precedence than ..
test_expr('$x && 1..10', ['..', ['&&', '$x', 1], 10]);

# Range precedence: = has lower precedence than ..
# $a = 1..10 should parse as $a = (1..10)
test_expr('$a = 1..10', ['=', '$a', ['..', 1, 10]]);


diag "";
diag "-------- File Test Operators:";

# Basic file tests
test_expr('-f $file', ['prefix_op', '-f', '$file']);
test_expr('-r $file', ['prefix_op', '-r', '$file']);
test_expr('-w $file', ['prefix_op', '-w', '$file']);
test_expr('-e $file', ['prefix_op', '-e', '$file']);
test_expr('-d $dir',  ['prefix_op', '-d', '$dir']);
test_expr('-s $file', ['prefix_op', '-s', '$file']);
test_expr('-z $file', ['prefix_op', '-z', '$file']);

# With string literal
test_expr('-f "foo.txt"', ['prefix_op', '-f', '"foo.txt"']);

# In conditional expression
test_expr('-e $file && -r $file',
          ['&&',
           ['prefix_op', '-e', '$file'],
           ['prefix_op', '-r', '$file']]);

# Symbolic link test
test_expr('-l $path', ['prefix_op', '-l', '$path']);

# Directory test with path
test_expr('-d "/tmp"', ['prefix_op', '-d', '"/tmp"']);


diag "";
diag "-------- Implicit \$_ Handling:";

# Functions that default to $_
test_expr('chomp()',
          ['funcall', 'chomp', '$_']);

test_expr('lc()',
          ['funcall', 'lc', '$_']);

test_expr('uc()',
          ['funcall', 'uc', '$_']);

test_expr('length()',
          ['funcall', 'length', '$_']);

test_expr('chr()',
          ['funcall', 'chr', '$_']);

# With explicit argument (should NOT add $_)
test_expr('lc("HELLO")',
          ['funcall', 'lc', '"HELLO"']);

test_expr('chomp($x)',
          ['funcall', 'chomp', '$x']);

# In expression
test_expr('length() + 5',
          ['+',
           ['funcall', 'length', '$_'],
           5]);

# Chained
test_expr('lc(uc())',
          ['funcall',
           'lc',
           ['funcall', 'uc', '$_']]);


diag "";
diag "-------- Combined Features:";

# Defined-or with function call
test_expr('lc() // "default"',
          ['//',
           ['funcall', 'lc', '$_'],
           '"default"']);

# File test with defined-or
test_expr('-f $file // 0',
          ['//',
           ['prefix_op', '-f', '$file'],
           0]);

# Range with file test
test_expr('-s $file > 0 ? 1..10 : 20..30',
          ['ternary',
           ['>',
            ['prefix_op', '-s', '$file'],
            0],
           ['..', 1, 10],
           ['..', 20, 30]]);


done_testing();
