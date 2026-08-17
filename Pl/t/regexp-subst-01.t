#          -*-Mode: CPerl -*-
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Test s/// substitution and tr/// transliteration

use v5.32;
use strict;
use warnings;

use lib ".";

use PPI;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use Test::More;

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
             '(p-=~ $_ (p-subst "foo" "bar"))',
             's/foo/bar/ basic (implicit $_)');

test_codegen('s/hello/world/g',
             '(p-=~ $_ (p-subst "hello" "world" :g))',
             's///g global (implicit $_)');

test_codegen('s/UPPER/lower/i',
             '(p-=~ $_ (p-subst "UPPER" "lower" :i))',
             's///i case insensitive (implicit $_)');

test_codegen('s/pattern/replace/gi',
             '(p-=~ $_ (p-subst "pattern" "replace" :g :i))',
             's///gi multiple modifiers (implicit $_)');


# ============================================================
diag "";
diag "-------- s/// with binding operator:";

test_codegen('$str =~ s/old/new/',
             '(p-=~ $str (p-subst "old" "new"))',
             '$str =~ s///');

test_codegen('$str =~ s/a/b/g',
             '(p-=~ $str (p-subst "a" "b" :g))',
             '$str =~ s///g');

test_codegen('$str !~ s/x/y/',
             '(p-!~ $str (p-subst "x" "y"))',
             '$str !~ s///');


# ============================================================
diag "";
diag "-------- s/// with more modifiers (standalone - implicit \$_):";

test_codegen('s/pat/rep/s',
             '(p-=~ $_ (p-subst "pat" "rep" :s))',
             's///s single-line (implicit $_)');

test_codegen('s/pat/rep/m',
             '(p-=~ $_ (p-subst "pat" "rep" :m))',
             's///m multi-line (implicit $_)');

test_codegen('s/pat/rep/x',
             '(p-=~ $_ (p-subst "pat" "rep" :x))',
             's///x extended (implicit $_)');

test_codegen('s/pat/rep/gimsxe',
             '(p-=~ $_ (p-subst "pat" (lambda () (let ((*wantarray* nil)) (pl-rep))) :e :g :i :m :s :x))',
             's/// all common modifiers (implicit $_)');


# ============================================================
diag "";
diag "-------- Basic tr/// transliteration (standalone - implicit \$_):";

test_codegen('tr/a-z/A-Z/',
             '(p-=~ $_ (p-tr "a-z" "A-Z"))',
             'tr/a-z/A-Z/ uppercase (implicit $_)');

test_codegen('tr/A-Z/a-z/',
             '(p-=~ $_ (p-tr "A-Z" "a-z"))',
             'tr/A-Z/a-z/ lowercase (implicit $_)');

test_codegen('y/abc/xyz/',
             '(p-=~ $_ (p-tr "abc" "xyz"))',
             'y/// synonym for tr/// (implicit $_)');


# ============================================================
diag "";
diag "-------- tr/// with modifiers (standalone - implicit \$_):";

test_codegen('tr/aeiou//d',
             '(p-=~ $_ (p-tr "aeiou" "" :d))',
             'tr///d delete (implicit $_)');

test_codegen('tr/a-z//c',
             '(p-=~ $_ (p-tr "a-z" "" :c))',
             'tr///c complement (implicit $_)');

test_codegen('tr/a-z//s',
             '(p-=~ $_ (p-tr "a-z" "" :s))',
             'tr///s squash (implicit $_)');

test_codegen('tr/a-z/A-Z/cds',
             '(p-=~ $_ (p-tr "a-z" "A-Z" :c :d :s))',
             'tr/// multiple modifiers (implicit $_)');


# ============================================================
diag "";
diag "-------- tr/// with binding:";

test_codegen('$str =~ tr/a-z/A-Z/',
             '(p-=~ $str (p-tr "a-z" "A-Z"))',
             '$str =~ tr///');

test_codegen('$count = ($str =~ tr/x//)',
             '(p-scalar-= $count (p-=~ $str (p-tr "x" "")))',
             'Count chars with tr///');


diag "";
diag "-------- Named captures %+:";

# $+{name} in code should access the %+ hash
test_codegen('$+{year}',
             '(p-gethash %+ "year")',
             '$+{name} generates hash access on %+');

# $+{name} in string interpolation — wrapped in p-string-concat so the captured
# value is stringified (interpolation always stringifies its single part).
test_codegen('"$+{year}"',
             '(p-string-concat (p-gethash %+ "year"))',
             '"$+{name}" interpolates via p-gethash');

diag "";
diag "All s/// and tr/// tests completed!";

diag "";
diag "-------- heredoc inside \${\\ …} inside an s///e replacement (task #342):";

# The construct t/base/lex.t is built around: a heredoc OPENED inside an
# interpolation block, with the body inside the s/// delimiters.  PPI lexes
# `"${\<<END}"` as one Quote::Double token and never sees the opener, so the
# body and terminator were left as loose code the expression parser refused.
# perl is the oracle: the same source through both.
{
    my $root = "$RealBin/../..";
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh <<'PL';
my $test = 42;
my $v = "V";
$_ = "";
s|(?:)|"${\<<END}"
ok $test - heredoc in "" in multiline s///e outside eval
END
|e;
print $_ || "not ok $test\n";
$_ = "cYd";
s|Y|"${\<<'RAW'}"
raw $v
RAW
|e;
print $_;
PL
    close $fh;
    my $perl_out = `perl $file 2>&1`;
    my $pcl_out  = `$root/runpcl $file 2>&1`;
    is($pcl_out, $perl_out,
       '#342: heredoc in ${\ …} in an s///e replacement — same as perl')
      or diag("perl: [$perl_out]\nPCL:  [$pcl_out]");
    like($perl_out, qr/heredoc in "" in multiline/,
         '#342: …and the oracle really did substitute the heredoc');
}

diag "";
diag "-------- s///e replacement that cannot be compiled must DIE (task #342):";

# RULE 12: `s/…/EXPR/e` replaces matched text with EXPR's VALUE, so a
# replacement the compiler cannot build must not become nil — that substituted
# the empty string, exit 0, with only a warning nobody reads.  The trigger is
# VALID Perl that PCL genuinely cannot compile (smartmatch — a documented
# feature absence, Track A of Option B phase 2): perl prints 1 here.
{
    my $root = "$RealBin/../..";
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh <<'PL';
no warnings;
$_ = "z";
s|z|my $q = 1; $q ~~ [1]|e;
print $_, "\n";
PL
    close $fh;
    my $err = "$file.err";
    my $out = `$root/pl2cl $file 2>$err`;
    my $rc  = $? >> 8;
    my $msg = do { open my $e, '<', $err or die; local $/; <$e> };
    unlink $err;
    isnt($rc, 0, 's///e with an uncompilable replacement exits nonzero (rule 12)');
    like($msg, qr/cannot compile the s\/\/\/e replacement/,
         '... and says which replacement it could not compile');
    unlike($out, qr/lambda \(\) nil/,
           '... and emits no nil replacement thunk');
}

done_testing();
