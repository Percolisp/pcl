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

done_testing();
