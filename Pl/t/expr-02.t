#          -*-Mode: CPerl -*-

# Test the expression parser:

use v5.32;
use strict;
use warnings;

use lib ".";

use Data::Dump qw/dump/;

use PPI;
use PPI::Dumper;


use Test::More tests => 360;
BEGIN { use_ok('Pl::PExpr') };

my $code;
my $doc;
my $expr_o    = Pl::PExpr->new();
my $expr;
my $node_id;
my $node;
my $kid_ids;
my @kid_nodes;

# - - - Parse order of r-associative:
# Pl::PExpr::SET_DEBUG(1 + 8);

# r-associative: $q = $w = $z
#         =
#     $q      =
#          $w   $z

# But when add more, will need parentheses

Pl::PExpr::SET_DEBUG(0);


if (0) {
  # $code       = '$x = {foo => "bar", "heja",  42}->{heja}';
  # $code       = '$x = $qq->{heja}'; # XXXX Fail, this is a funcall!!

  # XXXX Need to look at when doing prototypes etc. Can specify code block
  #      in them too??
  # $code       = 'grep { $_ > 0  } @foo';

  # XXXXX This fail when first value is a number. WTF? :-(
  #       is_inline_arr() etc works. Not parse_list().

  # $code       = '$x = {foo => "bar", "heja",  [42, "duh"]}->{heja}';
  # $code       = '$x = [foo => "bar"]'; # , "heja",  42]';

  say "Code: $code";
  # test_expr($code, ['hash_init', , 2], 1024);

  $doc        = PPI::Document->new(\$code);
  $expr       = _get_ppi_part($doc);
# say dump $expr; exit 0;
  $expr_o     = Pl::PExpr->new(e => $expr);
Pl::PExpr::SET_DEBUG(4);
  $node_id    = $expr_o->parse_expr_to_tree($expr);
  # Pl::PExpr::SET_DEBUG(0);
  say "/////////////////  Dump of $code parsed:";
  _dump_expr_vals($code, $expr, $expr_o, $node_id);
  exit 0;
}

Pl::PExpr::SET_DEBUG(0);
my $q = "'";


# XXXX FAIL!!
# $code         = '@array[0..5]';
# $doc          = PPI::Document->new(\$code);
# $expr         = _get_ppi_part($doc);
# say "Code: $code";
# $expr_o       = Pl::PExpr->new();
# Pl::PExpr::SET_DEBUG(1);
# $node_id      = $expr_o->parse_expr_to_tree($expr);
# _dump_expr_vals($code, $expr, $expr_o, $node_id);
# Pl::PExpr::SET_DEBUG(0);
# exit 0;


# -------------
diag "";
diag "-------- Diverse:";
test_expr('"foo" . $qq . "duh"', ['.', ['.', '"foo"', '$qq'], '"duh"']);
test_expr('foo("hello") * 3',
          ['*',
           ['funcall',
            'foo',
            '"hello"'],
           3,]
      );


# (This was confusing first. Perl gives an error without () here, since
#  '=' is right assoc and '+' has higher precedence than '+'.)
test_expr('$q= 1 + ($w=$z)', ['=', '$q',
                              ['+', '1', ['tree_val', ['=', '$w', '$z']]]]);


# Inline hashes and arrays:
test_expr('$x = [ foo => "bar", "heja",  42]',
          ['=', '$x',
           ['arr_init', '"foo"', '"bar"', '"heja"', 42]]);

test_expr('$x = {foo => "bar", "heja",  42}',
          ['=', '$x',
           ['hash_init', '"foo"', '"bar"', '"heja"', 42]]);


test_expr('$foo{ duh  } * 5', ['*', ['h_acc', '$foo', '"duh"'], 5]);

test_expr('$x = {foo => "bar", "heja",  42}->{heja}',
          ['=', '$x',
           ['h_ref_acc',
            ['hash_init', '"foo"', '"bar"', '"heja"', 42],
            '"heja"']
        ]);


test_expr('$x = {foo => "bar", "heja",  [42, "duh"]}->{heja}',
          ['=', '$x',
           ['h_ref_acc',
            ['hash_init', '"foo"', '"bar"',
	     '"heja"', ['arr_init', 42, '"duh"']],
            '"heja"']
        ]);


test_expr('$foo = $bar // $frotz',
	  ['=', '$foo', ['//', '$bar', '$frotz']]);

test_expr('$x // $y // $z', ['//', ['//', '$x', '$y'], '$z']);

# $code         = '$foo = $bar // $frotz';
# Pl::PExpr::SET_DEBUG(0);
# $doc          = PPI::Document->new(\$code);
# $expr         = _get_ppi_part($doc);
# say "Code: $code";
# $expr_o       = Pl::PExpr->new();
# my $top_n_id  = $expr_o->parse_expr_to_tree($expr);
# # say dump $expr_o; exit 0;
# _dump_expr_vals($code, $expr, $expr_o, $top_n_id);
# Pl::PExpr::SET_DEBUG(0);
# exit 0;


# -------------
diag "";
diag "-------- Method calls:";
test_expr('$foo->(1 => 42)', ['ref_funcall', '$foo', 1, 42]);
test_expr('$foo->[1]->[2]', ['a_acc', ['a_ref_acc', '$foo', 1], 2]);
test_expr('$foo->{$bar}->[5]',
          ['a_acc', ['h_ref_acc', '$foo', '$bar'], 5]);
test_expr('$foo[3][4]->[5]',
          ['a_acc', ['a_acc', ['a_acc', '$foo', 3], 4], 5]);


test_expr('$foo->$barf(5)',
          ['methodcall',
           '$foo',              # Get object.
           '$barf',             # Name of method.
           5,]                  # Parameter
      );

test_expr('funny($foo)->barf(5)',
          ['methodcall',
           ['funcall', 'funny', '$foo'], # Get object.
           'barf',                       # Name of method.
           5],                             # Parameter
      );


# methodcall
# . h_ref_acc
# . . id 14, Class PPI::Token::Symbol, value: $obj
# . . id 16, Class PPI::Token::Symbol, value: $bar
# . id 13, Class PPI::Token::Word, value: foobar
# . id 9, Class PPI::Token::Number, value: 22
# . id 12, Class PPI::Token::Operator, value: +
# . . id 10, Class PPI::Token::Number, value: 42
# . . id 11, Class PPI::Token::Number, value: 12

test_expr('$obj->{$bar}->foobar(22, 42 + 12)',
          ['methodcall',
           ['h_ref_acc', '$obj', '$bar'], # Get object to call.
           'foobar',                      # Name of method.
           22,                            # 1st param to method
           ['+', 42, 12]]                 # 2nd param
      );



# methodcall
# . id 17, Class PPI::Token::Symbol, value: $foo
# . id 18, Class PPI::Token::Symbol, value: $foobar
# . id 15, Class PPI::Token::Operator, value: *
# . . ref_funcall
# . . . a_ref_acc
# . . . . id 10, Class PPI::Token::Symbol, value: $q
# . . . . id 12, Class PPI::Token::Number, value: 1
# . . . id 8, Class PPI::Token::Quote::Double, value: "hello"
# . . id 14, Class PPI::Token::Number, value: 3

test_expr('$foo->$foobar($q->[1]->("hello") * 3)',
          ['methodcall',
           '$foo',              # Object to call
           '$foobar',           # Method name in variable
           # Parameters:
           ['*',
            ['ref_funcall', ['a_ref_acc', '$q', 1], '"hello"'],
            3,]]
       );


# ------------- Diverse:
diag "";
diag "-------- Diverse functions etc:";
test_expr('$bar= $foo x $z', ['=', '$bar', ['x', '$foo', '$z']]);
test_expr('$bar =~ /foo/i', ['=~', '$bar', '/foo/i']);
test_expr('/foo/i', ['=~', '$_', '/foo/i']);
test_expr('"foo" . $bar', ['.', '"foo"', '$bar']);
test_expr('"foo" . $qq . "duh"', ['.', ['.', '"foo"', '$qq'], '"duh"']);


# ------------- One parameter operands:
diag "";
diag "-------- Single parameter ops:";
test_expr('!$foo', ['prefix_op', '!', '$foo']);
test_expr('!$foo + 5', ['+', ['prefix_op', '!', '$foo'], 5]);
test_expr('-f "foo" . $bar', ['prefix_op', '-f', ['.', '"foo"', '$bar']]);
test_expr('++$bar', ['prefix_op', '++', '$bar']);
test_expr('5 + ++$bar', ['+', 5, ['prefix_op', '++', '$bar']]);
test_expr('++$bar + 5', ['+', ['prefix_op', '++', '$bar'], 5]);
test_expr('$bar++', ['postfix_op', '$bar', '++']);
test_expr('5 + $bar++', ['+', 5, ['postfix_op', '$bar', '++']]);
test_expr('$bar++ + 5', ['+', ['postfix_op', '$bar', '++'], 5]);
test_expr('($bar=5)++', ['postfix_op', ['tree_val', ['=', '$bar', 5]], '++']);


# ------------- Slices:
diag "";
diag "-------- Slices:";
test_expr('@w=@q[3,4,5,6]',
          ['=', '@w', ['slice_a_acc', '@q', 3,4,5,6]]);
test_expr('@w=@q{3,4,5,6}',
          ['=', '@w', ['slice_h_acc', '@q', 3,4,5,6]]);

# ------------- Chained compare:
diag "";
diag "-------- Chained:";
test_expr('$x < $y < $z and $q',
          ['and',
           ['postfix_op', '$x', '<', '$y', '<', '$z'],
           '$q']);


# -------------
# Basic ternary
diag "";
diag "-------- Ternary:";
test_expr('$x ? $y : $z', 
          ['ternary', '$x', '$y', '$z']);

diag "Ternary With operators in condition";
test_expr('$x > 0 ? "pos" : "neg"',
          ['ternary',
           ['>', '$x', 0],
           '"pos"',
           '"neg"']);


diag "Ternary right-associative nesting:";
test_expr('$a ? $b : $c ? $d : $e',
          ['ternary',
           '$a',
           '$b',
           ['ternary', '$c', '$d', '$e']]);


diag "Ternary left nesting (with parens)";
test_expr('($a ? $b : $c) ? $d : $e',
          ['ternary',
           ['tree_val', ['ternary', '$a', '$b', '$c']],
           '$d',
           '$e']);


diag "Ternary in complex expressions in each part:";
test_expr('$x + $y > 10 ? $a * 2 : $b / 2',
          ['ternary',
           ['>', ['+', '$x', '$y'], 10],
           ['*', '$a', 2],
           ['/', '$b', 2]]);


diag "Ternary nested in function call";
test_expr('foo($x ? $y : $z)',
          ['funcall',
           'foo',
           ['ternary', '$x', '$y', '$z']]);


diag "Multiple ternaries:";
test_expr('$a ? $b : $c ? $d : $e ? $f : $g',
          ['ternary',
           '$a',
           '$b',
           ['ternary',
            '$c',
            '$d',
            ['ternary', '$e', '$f', '$g']]]);


diag "Ternary with assignment (assignment has lower precedence):";
test_expr('$x = $a ? $b : $c',
          ['=', '$x',
           ['ternary', '$a', '$b', '$c']]);

test_expr('$x = $a ? $b : $c ? $d : $e',
          ['=', '$x',
           ['ternary', '$a', '$b',
            ['ternary', '$c', '$d', '$e']]]);

test_expr('$x += $a ? 1 : 0',
          ['+=', '$x',
           ['ternary', '$a', 1, 0]]);


diag "Ternary with 'and'/'or' (even lower precedence than assignment):";
test_expr('$x = $a ? $b : $c or $default',
          ['or',
           ['=', '$x', ['ternary', '$a', '$b', '$c']],
           '$default']);

test_expr('$ok and $x ? $y : $z',
          ['and', '$ok',
           ['ternary', '$x', '$y', '$z']]);

test_expr('$a ? $b : $c and $d ? $e : $f',
          ['and',
           ['ternary', '$a', '$b', '$c'],
           ['ternary', '$d', '$e', '$f']]);


diag "Ternary with high-precedence operators in condition/branches:";
test_expr('$x + $y ? $a : $b',
          ['ternary', ['+', '$x', '$y'], '$a', '$b']);

test_expr('$x > 0 && $y ? $a : $b',
          ['ternary', ['&&', ['>', '$x', 0], '$y'], '$a', '$b']);

test_expr('$cond ? $a + $b : $c * $d',
          ['ternary', '$cond',
           ['+', '$a', '$b'],
           ['*', '$c', '$d']]);

test_expr('$x || $y ? $a : $b',
          ['ternary', ['||', '$x', '$y'], '$a', '$b']);


diag "Ternary with mixed precedence:";
test_expr('$r = $x > 0 ? $a + 1 : $b - 1',
          ['=', '$r',
           ['ternary', ['>', '$x', 0],
            ['+', '$a', 1],
            ['-', '$b', 1]]]);

test_expr('$x = $a && $b ? $c : $d or die',
          ['or',
           ['=', '$x',
            ['ternary', ['&&', '$a', '$b'], '$c', '$d']],
           ['funcall', 'die']]);

test_expr('$x .= $flag ? "yes" : "no"',
          ['.=', '$x',
           ['ternary', '$flag', '"yes"', '"no"']]);


exit 0;




# -------------
my $n_id;
$code         = 'bar("foo" => 3,5)';
# THis fails now:
# $code         = '$q={foo => 5, bar => 10}';
Pl::PExpr::SET_DEBUG(0);
$doc          = PPI::Document->new(\$code);
$expr         = _get_ppi_part($doc);
say "Code: $code";
say dump $doc; exit 0;

$expr_o       = Pl::PExpr->new();
$n_id         = $expr_o->parse_expr_to_tree($expr);
# say dump $expr_o; exit 0;
_dump_expr_vals($code, $expr, $expr_o, $n_id);
Pl::PExpr::SET_DEBUG(0);


$code         = '$q={foo => 5, bar => 10}';
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

# XXXX Add value other than content for a node, like 'ref:PPI::Token::...'

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
  $expr_o->debug_dump_tree($node_id)
      if $debuglvl == 1024;
  _test_expr_2($node_id, $expr_o, $tst_spec, $code);
  Pl::PExpr::SET_DEBUG(0)
      if $debuglvl;
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

