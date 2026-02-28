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
diag "-------- Basic s/// substitution (standalone - implicit \$_):";

# Standalone s/// gets wrapped with '$_ =~' for implicit target
test_codegen('s/foo/bar/',
             '(pl-=~ $_ (pl-subst "foo" "bar"))',
             's/foo/bar/ basic (implicit $_)');

test_codegen('s/hello/world/g',
             '(pl-=~ $_ (pl-subst "hello" "world" :g))',
             's///g global (implicit $_)');

test_codegen('s/UPPER/lower/i',
             '(pl-=~ $_ (pl-subst "UPPER" "lower" :i))',
             's///i case insensitive (implicit $_)');

test_codegen('s/pattern/replace/gi',
             '(pl-=~ $_ (pl-subst "pattern" "replace" :g :i))',
             's///gi multiple modifiers (implicit $_)');


# ============================================================
diag "";
diag "-------- s/// with binding operator:";

test_codegen('$str =~ s/old/new/',
             '(pl-=~ $str (pl-subst "old" "new"))',
             '$str =~ s///');

test_codegen('$str =~ s/a/b/g',
             '(pl-=~ $str (pl-subst "a" "b" :g))',
             '$str =~ s///g');

test_codegen('$str !~ s/x/y/',
             '(pl-!~ $str (pl-subst "x" "y"))',
             '$str !~ s///');


# ============================================================
diag "";
diag "-------- s/// with more modifiers (standalone - implicit \$_):";

test_codegen('s/pat/rep/s',
             '(pl-=~ $_ (pl-subst "pat" "rep" :s))',
             's///s single-line (implicit $_)');

test_codegen('s/pat/rep/m',
             '(pl-=~ $_ (pl-subst "pat" "rep" :m))',
             's///m multi-line (implicit $_)');

test_codegen('s/pat/rep/x',
             '(pl-=~ $_ (pl-subst "pat" "rep" :x))',
             's///x extended (implicit $_)');

test_codegen('s/pat/rep/gimsxe',
             '(pl-=~ $_ (pl-subst "pat" "rep" :e :g :i :m :s :x))',
             's/// all common modifiers (implicit $_)');


# ============================================================
diag "";
diag "-------- Basic tr/// transliteration (standalone - implicit \$_):";

test_codegen('tr/a-z/A-Z/',
             '(pl-=~ $_ (pl-tr "a-z" "A-Z"))',
             'tr/a-z/A-Z/ uppercase (implicit $_)');

test_codegen('tr/A-Z/a-z/',
             '(pl-=~ $_ (pl-tr "A-Z" "a-z"))',
             'tr/A-Z/a-z/ lowercase (implicit $_)');

test_codegen('y/abc/xyz/',
             '(pl-=~ $_ (pl-tr "abc" "xyz"))',
             'y/// synonym for tr/// (implicit $_)');


# ============================================================
diag "";
diag "-------- tr/// with modifiers (standalone - implicit \$_):";

test_codegen('tr/aeiou//d',
             '(pl-=~ $_ (pl-tr "aeiou" "" :d))',
             'tr///d delete (implicit $_)');

test_codegen('tr/a-z//c',
             '(pl-=~ $_ (pl-tr "a-z" "" :c))',
             'tr///c complement (implicit $_)');

test_codegen('tr/a-z//s',
             '(pl-=~ $_ (pl-tr "a-z" "" :s))',
             'tr///s squash (implicit $_)');

test_codegen('tr/a-z/A-Z/cds',
             '(pl-=~ $_ (pl-tr "a-z" "A-Z" :c :d :s))',
             'tr/// multiple modifiers (implicit $_)');


# ============================================================
diag "";
diag "-------- tr/// with binding:";

test_codegen('$str =~ tr/a-z/A-Z/',
             '(pl-=~ $str (pl-tr "a-z" "A-Z"))',
             '$str =~ tr///');

test_codegen('$count = ($str =~ tr/x//)',
             '(pl-scalar-= $count (pl-=~ $str (pl-tr "x" "")))',
             'Count chars with tr///');


diag "";
diag "All s/// and tr/// tests completed!";

done_testing();
