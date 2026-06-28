#!/usr/bin/env perl
# Transpile tests part 3: variable scoping

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

# Path to pl2cl and runtime
my $project_root = "$RealBin/../..";
my $pl2cl = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";

# Check dependencies
plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

# Run a Perl snippet and return output
sub run_perl {
    my ($code) = @_;
    my $output = `perl -e '$code' 2>&1`;
    return $output;
}

# Transpile and run CL, return output
sub run_cl {
    my ($code) = @_;

    # Write Perl code to temp file
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    # Transpile
    my $cl_code = `$pl2cl $pl_file 2>&1`;

    # Write CL to temp file
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    # Run with sbcl
    my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;

    # Filter out warnings and "PCL Runtime loaded"
    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^-->.*\n//gm;
    $output =~ s/^==>.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;

    return $output;
}

# Test helper: compare Perl and CL output
sub test_transpile {
    my ($name, $code) = @_;
    my $perl_out = run_perl($code);
    my $cl_out = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL: $cl_out");
}

# ============ SCOPING TESTS ============

# if condition scoping - variable visible inside if/else but not after
test_transpile("scope: my in if condition - visible in then block", '
my $result = "";
if (my $x = 5) { $result = "x=$x"; }
print "$result\n";
');

test_transpile("scope: my in if condition - visible in else block", '
my $result = "";
if (my $x = 0) { $result = "then"; } else { $result = "else:x=$x"; }
print "$result\n";
');

test_transpile("scope: my in if condition - not visible after if", '
my $x = "outer";
if (my $x = 5) { }
print "after:$x\n";
');

test_transpile("scope: nested if with my in condition", '
my $r = "";
if (my $a = 1) {
    if (my $b = 2) {
        $r = "a=$a,b=$b";
    }
}
print "$r\n";
');

test_transpile("scope: my in elsif condition", '
my $r = "";
my $test = 0;
if ($test) { $r = "if"; }
elsif (my $x = 10) { $r = "elsif:x=$x"; }
print "$r\n";
');

# Inline declaration in expression
test_transpile("scope: inline my in expression", '
my $z = 2;
my $result = (my $y = 5) * $z;
print "result=$result,y=$y\n";
');

# Multiple declarations in same condition
test_transpile("scope: assignment chain with my", '
my $r;
if (my $a = my $b = 3) { $r = "a=$a,b=$b"; }
print "$r\n";
');

test_transpile("scope: chained my in if - not visible after", '
my $x = "outer_x";
my $y = "outer_y";
if (my $x = my $y = 3) { }
print "after:$x,$y\n";
');

# while loop scoping
test_transpile("scope: my in while condition - visible inside", '
my $r = "";
my $n = 3;
while (my $i = $n--) { $r .= "$i,"; }
print "$r\n";
');

test_transpile("scope: my in while - not visible after", '
my $i = "outer";
my $n = 1;
while (my $i = $n--) { }
print "after:$i\n";
');

test_transpile("scope: chained my in while - not visible after", '
my $x = "outer_x";
my $y = "outer_y";
my $n = 1;
while (my $x = my $y = $n--) { }
print "after:$x,$y\n";
');

# for loop scoping
test_transpile("scope: my in for init - visible inside loop", '
my $r = "";
for (my $i = 0; $i < 3; $i++) { $r .= "$i,"; }
print "$r\n";
');

test_transpile("scope: my in for init - not visible after loop", '
my $i = "outer";
for (my $i = 0; $i < 2; $i++) { }
print "after:$i\n";
');

# foreach loop scoping - use push since @arr = (1,2,3) has a bug
test_transpile("scope: foreach loop variable", '
my $r = "";
my @arr; push @arr, 1; push @arr, 2; push @arr, 3;
foreach my $x (@arr) { $r .= "$x,"; }
print "$r\n";
');

# Multiple variable declarations
test_transpile("scope: multiple vars in for init", '
my $r = "";
for (my $i = 0, my $j = 10; $i < 3; $i++, $j++) { $r .= "$i:$j,"; }
print "$r\n";
');

test_transpile("scope: multiple vars in for - not visible after", '
my $i = "outer_i";
my $j = "outer_j";
for (my $i = 0, my $j = 0; $i < 2; $i++) { }
print "after:$i,$j\n";
');

# List declaration scoping - my ($x, $y) in if
test_transpile("scope: list declaration in if", '
my $p = "outer_p";
my $q = "outer_q";
if (my ($p, $q) = (5, 10) and 1) { }
print "after:$p,$q\n";
');

# ============ LABELED BLOCK EXIT (last LABEL) ============

test_transpile("last LABEL exits bare block", '
my $x = 0;
SKIP: { last SKIP; $x = 1; }
print "$x\n";
');

test_transpile("last LABEL with other label name", '
my $x = 0;
OUTER: { last OUTER; $x = 1; }
print "$x\n";
');

test_transpile("SKIP block: code after last LABEL does not run", '
my @ran;
SKIP: {
  last SKIP;
  push @ran, "inner";
}
push @ran, "after";
print join(",", @ran), "\n";
');

# ============ CONTROL CHARACTER ESCAPE \cX ============

test_transpile("control char \\c@ is chr(0)", '
my $c = "\c@";
print ord($c), "\n";
');

test_transpile("control char \\c? is chr(127)", '
my $c = "\c?";
print ord($c), "\n";
');

test_transpile("control char \\cA-\\cZ range", '
print ord("\cA"), " ", ord("\cZ"), "\n";
');

test_transpile("control char \\c@ in string concat", '
my $s = "a\c@b";
print length($s), "\n";
');

# \&funcname - references to named subs
test_transpile("funcref: \\&foo stored and called",
    'sub foo { print "hello\n"; } my $r = \&foo; $r->();');

test_transpile("funcref: \\&foo called with args",
    'sub add { my ($a, $b) = @_; print $a + $b, "\n"; } my $r = \&add; $r->(3, 4);');

test_transpile("funcref: \\&foo ref()",
    'sub foo {} my $r = \&foo; print ref($r), "\n";');

# &funcname(args) - explicit call with & sigil
test_transpile("amp call: &foo(args)",
    'sub double { my ($n) = @_; print $n * 2, "\n"; } &double(7);');

# ============ BARE BLOCK PACKAGE SCOPING ============

# Bare blocks with package declarations must not leak *package* after the block.
# Before fix: (in-package :Foo) inside block leaked, causing subsequent
# top-level forms to read symbols in wrong package.

test_transpile("bare block package: __PACKAGE__ reverts after block", '
{ package Foo; }
print __PACKAGE__, "\n";
');

test_transpile("bare block package: my var visible outside block, in correct pkg", '
{ package Foo; my $x = 1; }
our $y = 2;
print $y, "\n";
');

test_transpile("bare block package: multiple packages in one block", '
{ package P1; package P2; }
print __PACKAGE__, "\n";
');

test_transpile("bare block package: outer package preserved after inner block", '
package Bar;
{ package Foo; }
print __PACKAGE__, "\n";
');

# ============ exists &sub / defined &sub ============

# Forward declaration only — exists is true, defined is false
test_transpile("exists &sub — forward declared only", '
sub t1;
my $e = exists &t1 ? "yes" : "no";
print $e, "\n";
');

test_transpile("defined &sub — forward declared only (not defined)", '
sub t1;
my $d = defined &t1 ? "yes" : "no";
print $d, "\n";
');

# Full definition — exists and defined both true
test_transpile("exists &sub — with body", '
sub t5 { 1; }
my $e = exists &t5 ? "yes" : "no";
print $e, "\n";
');

test_transpile("defined &sub — with body", '
sub t5 { 1; }
my $d = defined &t5 ? "yes" : "no";
print $d, "\n";
');

# ============ PACKAGE IN BARE BLOCK (task #79) ============

# Sub defined inside bare block with inline package change
# must end up in the correct CL package, not :main
test_transpile("bare block pkg: sub defined in inner package exists", '
{ package P1; sub tmc { 1; } }
my $e = exists &P1::tmc ? "yes" : "no";
print $e, "\n";
');

test_transpile("bare block pkg: sub defined in inner package is callable", '
{ package P1; sub tmc { 42; } }
print P1::tmc(), "\n";
');

test_transpile("bare block pkg: multiple packages in block", '
{ package P1; sub p1_sub { 1; } package P2; sub p2_sub { 2; } }
my $e1 = exists &P1::p1_sub ? "y" : "n";
my $e2 = exists &P2::p2_sub ? "y" : "n";
print "$e1 $e2\n";
');

# Regression: print UPPERCASE->chain() — uppercase/single-letter class name
# was mistakenly consumed as a print filehandle, leaving -> at position 0.
test_transpile("chained method: uppercase class name not treated as filehandle", '
package B; sub new { bless {v => $_[1]}, $_[0] } sub val { $_[0]->{v} }
package main;
print B->new("ok")->val(), "\n";
', "ok\n");

test_transpile("chained method: ALLCAPS class name not treated as filehandle", '
package DB; sub new { bless {}, $_[0] } sub ping { "pong" }
package main;
print DB->new()->ping(), "\n";
', "pong\n");

test_transpile("chained method: real filehandle still works", '
print STDOUT "yes\n";
', "yes\n");

# Regression: bless sub { ... }, "Class" — anonymous sub in arg position
# splice bug in PExpr.pm was eating the comma after sub { }, causing parse error.
test_transpile("bless: anon sub blessed into class", '
my $e1 = bless sub { 42 }, "E";
print ref($e1), "\n";
');

test_transpile("bless: anon sub called after bless", '
my $e1 = bless sub { return "hello" }, "E";
print $e1->(), "\n";
');

# Regression: package NAME; inside a sub body must not open a new CL section.
# Previously, package _foo; inside sub broke the section/bucket structure and
# caused all subsequent top-level code to be emitted inside the sub body.
test_transpile("package inside sub: inline switch does not leak", '
sub make_foo {
    package Foo;
    return bless {}, "Foo";
}
my $f = make_foo();
print ref($f), "\n";
print "ok\n";
');

# Regression: ref() on blessed array/scalar refs passed through function args
# %pl-flatten-list was stripping the class by extracting the inner value.
test_transpile("bless: ref() on blessed array survives function call", '
my $b = bless [], "MyArr";
sub check_ref { my ($x) = @_; print ref($x), "\n"; }
check_ref($b);
');

test_transpile("bless: ref() on blessed scalar survives function call", '
my $tmp = "hello";
my $s = \$tmp;
my $b = bless $s, "MyScl";
sub check_ref2 { my ($x) = @_; print ref($x), "\n"; }
check_ref2($b);
');

# sprintf %g must not strip significant integer-part trailing zeros: 100000
# (which %g prints in fixed notation, exp 5 < precision 6) was becoming "1".
test_transpile("sprintf %g keeps integer trailing zeros", '
printf "%g %g %g %g\n", 100000, 200000, 500000, 120000;
');
test_transpile("sprintf %g fixed vs exponential boundary", '
printf "%g %g %g\n", 999999, 1000000, 0.00001;
');

# Postfix -- on undef returns undef (NOT 0, unlike ++); then sets to -1.
test_transpile("postdec of undef returns undef", '
my $a; my $y = $a--;
printf "y=%s a=%s\n", (defined $y ? $y : "undef"), $a;
my @x; my $z = $x[0]--;
printf "z=%s x0=%s\n", (defined $z ? $z : "undef"), $x[0];
');

# Nested compound-assignment lvalue chains: each compound op returns its LHS as
# an lvalue, so the place form is shared.  The runtime must evaluate the place
# exactly once per op — otherwise the inner assignment re-runs and the result
# grows exponentially.  (opbasic/concat.t test 242.)
test_transpile("nested .= lvalue chain evaluates place once", '
my $a = "a";
(($a .= $a) .= $a) .= $a;
print "$a\n";
');
test_transpile("nested += lvalue chain evaluates place once", '
my $n = 1;
(($n += $n) += $n) += $n;
print "$n\n";
');

# CORE::<declarator> in expression context (CORE::my / CORE::state) names the
# bare declarator (PCL has no overridable builtins).  Without normalization it
# parsed as a function call and crashed.  (opbasic/concat.t.)
test_transpile("CORE::my declarator in expression context", '
my $h = { a => 1 };
print ref(CORE::my $x = $h), "\n";
');
test_transpile("CORE::state declarator in expression context", '
use feature "state";
sub f { my $r = (CORE::state $y = 7); $y }
print f(), f(), "\n";
');

# $^T (BASETIME) is the program start time in Unix seconds — must be a sane
# positive epoch value, not an unbound variable.  (op/lex_assign.t.)
test_transpile("\$^T is a positive epoch value", '
print(($^T > 1000000000 ? "yes" : "no"), "\n");
print((localtime($^T))[5] + 1900 >= 2020 ? "ok\n" : "bad\n");
');

# `undef` placeholder in a my-list LHS occupies a position; with a single declared
# var the (vector $x) shortcut used to DROP a leading/middle undef and misalign
# the assignment (my (undef, $b) = (10,20) wrongly gave $b=10).  Found while
# writing Pl/t/socket-01.t (getpeername unpack).
test_transpile("my-list leading undef placeholder", '
my (undef, $b) = (10, 20);
my (undef, undef, $c) = (1, 2, 3);
my @a = (5, 6, 7); my (undef, $x) = @a;
print "$b $c $x\n";
');
test_transpile("my-list interleaved undef placeholders and array slurp", '
my ($a, undef, $c, undef, $e) = (1 .. 5);
my (undef, @rest) = (9, 8, 7);
print "$a$c$e | @rest\n";
');

# A user sub with an old-style scalar prototype ($) imposes SCALAR context on
# the corresponding argument: an array yields its element count, a hash its
# bucket-ratio-ish scalar, and a wantarray-sensitive builtin (each/keys) its
# scalar value — rather than flattening into the arg list.  perl's t/test.pl
# `sub is ($$@)` relies on this so `is(@a, 3)` / `is(each @h, 0)` work.
test_transpile("scalar prototype (\$) imposes scalar context on array arg", '
sub takes ($) { return $_[0]; }
my @a = (10, 20, 30);
print takes(@a), "\n";
');
test_transpile("scalar prototype (\$\$) imposes scalar ctx on each slot", '
sub two ($$) { return "$_[0]|$_[1]"; }
my @a = (10, 20, 30);
print two(@a, 99), "\n";
');
test_transpile("scalar prototype (\$) collapses keys() to its count", '
sub one ($) { return $_[0]; }
my %h = (a => 1, b => 2, c => 3);
print one(keys %h), "\n";
');
# Guard: when a call passes FEWER args than the prototype mandates, an array
# argument is flattening to fill the slots — it must NOT be scalarized.  This
# is perl test.pl`s `sub like ($$@) { like_yn(0, @_) }` / `like_yn ($$$@)`
# pattern; collapsing @_ to its count breaks every test.pl-based file.
test_transpile("scalar prototype does not collapse a flattening \@_", '
sub f ($$@) { return g(0, @_); }
sub g ($$$@) { my ($flip, undef, $expected, $name, @mess) = @_;
               return "$expected|$name|@mess"; }
print f("got", "EXP", "nm", "x", "y"), "\n";
');

done_testing();
