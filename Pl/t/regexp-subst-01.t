#          -*-Mode: CPerl -*-

# Test s/// substitution and tr/// transliteration

use v5.32;
use strict;
use warnings;

use lib ".";

use PPI;
use Test::More tests => 22;

BEGIN { use_ok('Pl::PExpr') };
BEGIN { use_ok('Pl::ExprToCL') };


# Helper: convert Perl to CL
sub perl_to_cl {
  my $code    = shift;

  my $doc     = PPI::Document->new(\$code);
  my @stmts   = $doc->children();
  my @exprs   = $stmts[0]->children();

  my $expr_o  = Pl::PExpr->new(e => \@exprs, full_PPI => $doc);
  my $node_id = $expr_o->parse_expr_to_tree(\@exprs);
  $expr_o->annotate_contexts($node_id);

  my $gen     = Pl::ExprToCL->new(expr_o => $expr_o);
  return $gen->generate($node_id);
}

sub test_codegen {
  my $perl_code = shift;
  my $expected  = shift;
  my $desc      = shift // "Perl: $perl_code";

  my $result = perl_to_cl($perl_code);
  is($result, $expected, $desc);
}


# ============================================================
diag "";
diag "-------- Basic s/// substitution:";

test_codegen('s/foo/bar/',
             '(pl-subst "foo" "bar")',
             's/foo/bar/ basic');

test_codegen('s/hello/world/g',
             '(pl-subst "hello" "world" :g)',
             's///g global');

test_codegen('s/UPPER/lower/i',
             '(pl-subst "UPPER" "lower" :i)',
             's///i case insensitive');

test_codegen('s/pattern/replace/gi',
             '(pl-subst "pattern" "replace" :g :i)',
             's///gi multiple modifiers');


# ============================================================
diag "";
diag "-------- s/// with binding operator:";

test_codegen('$str =~ s/old/new/',
             '(pcl:pl-=~ $str (pl-subst "old" "new"))',
             '$str =~ s///');

test_codegen('$str =~ s/a/b/g',
             '(pcl:pl-=~ $str (pl-subst "a" "b" :g))',
             '$str =~ s///g');

test_codegen('$str !~ s/x/y/',
             '(pcl:pl-!~ $str (pl-subst "x" "y"))',
             '$str !~ s///');


# ============================================================
diag "";
diag "-------- s/// with more modifiers:";

test_codegen('s/pat/rep/s',
             '(pl-subst "pat" "rep" :s)',
             's///s single-line');

test_codegen('s/pat/rep/m',
             '(pl-subst "pat" "rep" :m)',
             's///m multi-line');

test_codegen('s/pat/rep/x',
             '(pl-subst "pat" "rep" :x)',
             's///x extended');

test_codegen('s/pat/rep/gimsxe',
             '(pl-subst "pat" "rep" :e :g :i :m :s :x)',
             's/// all common modifiers');


# ============================================================
diag "";
diag "-------- Basic tr/// transliteration:";

test_codegen('tr/a-z/A-Z/',
             '(pl-tr "a-z" "A-Z")',
             'tr/a-z/A-Z/ uppercase');

test_codegen('tr/A-Z/a-z/',
             '(pl-tr "A-Z" "a-z")',
             'tr/A-Z/a-z/ lowercase');

test_codegen('y/abc/xyz/',
             '(pl-tr "abc" "xyz")',
             'y/// synonym for tr///');


# ============================================================
diag "";
diag "-------- tr/// with modifiers:";

test_codegen('tr/aeiou//d',
             '(pl-tr "aeiou" "" :d)',
             'tr///d delete');

test_codegen('tr/a-z//c',
             '(pl-tr "a-z" "" :c)',
             'tr///c complement');

test_codegen('tr/a-z//s',
             '(pl-tr "a-z" "" :s)',
             'tr///s squash');

test_codegen('tr/a-z/A-Z/cds',
             '(pl-tr "a-z" "A-Z" :c :d :s)',
             'tr/// multiple modifiers');


# ============================================================
diag "";
diag "-------- tr/// with binding:";

test_codegen('$str =~ tr/a-z/A-Z/',
             '(pcl:pl-=~ $str (pl-tr "a-z" "A-Z"))',
             '$str =~ tr///');

test_codegen('$count = ($str =~ tr/x//)',
             '(pcl:pl-setf $count (pcl:pl-=~ $str (pl-tr "x" "")))',
             'Count chars with tr///');


diag "";
diag "All s/// and tr/// tests completed!";

done_testing();
