#!/usr/bin/env perl
# Transpile tests: compare Perl output with transpiled CL output

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

# ============ TESTS ============

# Basic arithmetic
test_transpile("arithmetic: add", 'print 1 + 2, "\n";');
test_transpile("arithmetic: subtract", 'print 10 - 3, "\n";');
test_transpile("arithmetic: multiply", 'print 4 * 5, "\n";');
test_transpile("arithmetic: modulo", 'print 10 % 3, "\n";');
test_transpile("arithmetic: power", 'print 2 ** 8, "\n";');

# String operations
test_transpile("string: concat", 'print "hello" . " " . "world", "\n";');
test_transpile("string: length", 'print length("hello"), "\n";');
test_transpile("string: uc", 'print uc("hello"), "\n";');
test_transpile("string: lc", 'print lc("HELLO"), "\n";');
test_transpile("string: substr", 'print substr("hello", 1, 3), "\n";');

# Variables
test_transpile("var: scalar assign", 'my $x = 42; print $x, "\n";');
test_transpile("var: scalar modify", 'my $x = 10; $x = $x + 5; print $x, "\n";');
test_transpile("var: increment", 'my $n = 5; $n++; print $n, "\n";');
test_transpile("var: decrement", 'my $n = 5; $n--; print $n, "\n";');

# Comparisons
test_transpile("cmp: numeric ==", 'print 5 == 5 ? "yes" : "no", "\n";');
test_transpile("cmp: numeric !=", 'print 5 != 3 ? "yes" : "no", "\n";');
test_transpile("cmp: numeric <", 'print 3 < 5 ? "yes" : "no", "\n";');
test_transpile("cmp: numeric >", 'print 5 > 3 ? "yes" : "no", "\n";');
test_transpile("cmp: string eq", 'print "a" eq "a" ? "yes" : "no", "\n";');
test_transpile("cmp: string ne", 'print "a" ne "b" ? "yes" : "no", "\n";');

# Logical
test_transpile("logic: and true", 'print 1 && 2, "\n";');
test_transpile("logic: and false", 'print 0 && 2, "\n";');
test_transpile("logic: or", 'print 0 || 5, "\n";');
test_transpile("logic: not", 'print !0, "\n";');

# Control flow
test_transpile("if: true branch", 'my $x = 10; if ($x > 5) { print "big\n"; }');
test_transpile("if: else branch", 'my $x = 3; if ($x > 5) { print "big\n"; } else { print "small\n"; }');

test_transpile("while: loop", 'my $i = 0; while ($i < 3) { print $i, "\n"; $i++; }');

test_transpile("for: c-style", 'for (my $i = 0; $i < 3; $i++) { print $i, "\n"; }');

# Subroutines
test_transpile("sub: basic call", 'sub add { my $a = shift; my $b = shift; return $a + $b; } print add(3, 4), "\n";');
test_transpile("sub: no args", 'sub hello { return "hi"; } print hello(), "\n";');
test_transpile("sub: with signature", 'use feature "signatures"; no warnings "experimental::signatures"; sub add($a, $b) { return $a + $b; } print add(3, 4), "\n";');

# map/grep/sort with blocks
test_transpile("map: simple block", 'my @a = (1, 2, 3); my @b = map { $_ * 2 } @a; print join(",", @b), "\n";');
test_transpile("map: multi-statement block", 'my @a = (1, 2, 3); my @b = map { my $x = $_; $x * 2 } @a; print join(",", @b), "\n";');
test_transpile("grep: simple block", 'my @a = (1, 2, 3, 4, 5); my @b = grep { $_ > 2 } @a; print join(",", @b), "\n";');
test_transpile("grep: multi-statement block", 'my @a = (1, 2, 3, 4, 5); my @b = grep { my $x = $_; $x > 2 } @a; print join(",", @b), "\n";');
test_transpile("sort: numeric block", 'my @a = (3, 1, 4, 1, 5); my @b = sort { $a <=> $b } @a; print join(",", @b), "\n";');

# List flattening - Perl flattens arrays when building lists
test_transpile("list flatten: arrays", 'my @a = (1, 2); my @b = (3, 4); my @c = (@a, @b); print join(",", @c), "\n";');
test_transpile("list flatten: mixed", 'my @a = (1, 2); my @c = (@a, "foo", 42); print join(",", @c), "\n";');
test_transpile("list flatten: nested", 'my @a = (1); my @b = (2, 3); my @c = (@a, @b, 4); print join(",", @c), "\n";');
test_transpile("list flatten: empty", 'my @a = (); my @b = (1, 2); my @c = (@a, @b); print join(",", @c), "\n";');

# L-value assignment tests (box-set returns box for modification)
# These test that assignment returns something modifiable

# Basic pre/post increment on assignment result
test_transpile("lvalue: pre-increment on assign", 'my $x; print ++($x = 5), "\n"; print $x, "\n";');
test_transpile("lvalue: post-increment on assign", 'my $x; print (($x = 5)++), "\n"; print $x, "\n";');
test_transpile("lvalue: pre-decrement on assign", 'my $x; print --($x = 5), "\n"; print $x, "\n";');
test_transpile("lvalue: post-decrement on assign", 'my $x; print (($x = 5)--), "\n"; print $x, "\n";');

# Increment/decrement through zero and negative
test_transpile("lvalue: pre-decrement to negative", 'my $x; print --($x = 0), "\n";');
test_transpile("lvalue: post-decrement to negative", 'my $x; print (($x = 1)--), "\n"; print $x, "\n";');

# Multiple increments chained
test_transpile("lvalue: double pre-increment", 'my $x; ++($x = 5); print ++$x, "\n";');
test_transpile("lvalue: pre-increment then post", 'my $x; ++($x = 5); print $x++, "\n"; print $x, "\n";');

# Nested assignment as l-value
test_transpile("lvalue: nested assign pre-inc", 'my ($x, $y); print ++($x = ($y = 5)), "\n"; print "$x $y\n";');
test_transpile("lvalue: nested assign post-inc", 'my ($x, $y); print (($x = ($y = 10))++), "\n"; print "$x $y\n";');

# Assignment in larger expression
test_transpile("lvalue: inc assign in expr", 'my $x; my $r = 10 + ++($x = 5); print "$r $x\n";');
test_transpile("lvalue: post-inc assign in expr", 'my $x; my $r = 10 + (($x = 5)++); print "$r $x\n";');

# Multiple variables modified
test_transpile("lvalue: multiple assigns inc", 'my ($a, $b); ++($a = 1); ++($b = 2); print "$a $b\n";');

# String increment (Perl magical ++)
test_transpile("lvalue: string pre-increment", 'my $x; print ++($x = "aa"), "\n";');
test_transpile("lvalue: string post-increment", 'my $x; print (($x = "zz")++), "\n"; print $x, "\n";');

# chop/chomp on assignment result
test_transpile("lvalue: chop on assign", 'my $x; chop($x = "hello"); print $x, "\n";');
test_transpile("lvalue: chomp on assign", 'my $x; chomp($x = "line\n"); print $x, "\n";');
test_transpile("lvalue: chop return value", 'my $x; print chop($x = "hi"), "\n"; print $x, "\n";');
test_transpile("lvalue: chomp return value", 'my $x; print chomp($x = "end\n"), "\n"; print $x, "\n";');

# Edge case: assign 0 (falsy value)
test_transpile("lvalue: inc on zero assign", 'my $x; print ++($x = 0), "\n";');
test_transpile("lvalue: dec on zero assign", 'my $x; print --($x = 0), "\n";');

# Edge case: assign empty string
test_transpile("lvalue: chop empty assign", 'my $x; chop($x = "a"); print length($x), "\n";');

# Compound: assignment result used immediately and modified
test_transpile("lvalue: use and modify", 'my $x; my $y = ($x = 5); ++$x; print "$x $y\n";');

# L-value tests for ARRAY elements
test_transpile("lvalue array: post-increment element", 'my @a = (1, 2, 3); $a[1]++; print $a[1], "\n";');
test_transpile("lvalue array: pre-increment element", 'my @a = (10, 20, 30); my $v = ++$a[0]; print $v, "\n";');
test_transpile("lvalue array: post-decrement element", 'my @a = (5, 6, 7); print $a[2]--, "\n"; print $a[2], "\n";');
test_transpile("lvalue array: pre-decrement element", 'my @a = (100); my $v = --$a[0]; print $v, "\n";');
test_transpile("lvalue array: chop element", 'my @a = ("hello", "world"); chop($a[0]); print $a[0], "\n";');
test_transpile("lvalue array: chomp element", 'my @a = ("line\n"); chomp($a[0]); print $a[0], "X\n";');
test_transpile("lvalue array: chop on assign to element", 'my @a; chop($a[0] = "test"); print $a[0], "\n";');
test_transpile("lvalue array: inc then read", 'my @a = (0); $a[0]++; $a[0]++; print $a[0], "\n";');
test_transpile("lvalue array: negative index inc", 'my @a = (1, 2, 3); $a[-1]++; print $a[2], "\n";');

# L-value tests for HASH elements
test_transpile("lvalue hash: post-increment element", 'my %h = (x => 5); $h{x}++; print $h{x}, "\n";');
test_transpile("lvalue hash: pre-increment element", 'my %h = (n => 10); my $v = ++$h{n}; print $v, "\n";');
test_transpile("lvalue hash: post-decrement element", 'my %h = (v => 100); print $h{v}--, "\n"; print $h{v}, "\n";');
test_transpile("lvalue hash: pre-decrement element", 'my %h = (k => 50); my $v = --$h{k}; print $v, "\n";');
test_transpile("lvalue hash: chop element", 'my %h = (s => "abcd"); chop($h{s}); print $h{s}, "\n";');
test_transpile("lvalue hash: chomp element", 'my %h = (line => "text\n"); chomp($h{line}); print $h{line}, "X\n";');
test_transpile("lvalue hash: chop on assign to element", 'my %h; chop($h{new} = "foo"); print $h{new}, "\n";');
test_transpile("lvalue hash: inc then read", 'my %h = (c => 0); $h{c}++; $h{c}++; $h{c}++; print $h{c}, "\n";');
test_transpile("lvalue hash: string key inc", 'my %h; $h{"key"}++; print $h{key}, "\n";');

# Mixed array/hash l-value operations
test_transpile("lvalue mixed: array and hash inc", 'my @a = (1); my %h = (x => 2); $a[0]++; $h{x}++; print $a[0], " ", $h{x}, "\n";');
test_transpile("lvalue mixed: chop both", 'my @a = ("ab"); my %h = (k => "cd"); chop($a[0]); chop($h{k}); print $a[0], $h{k}, "\n";');

# L-value corner cases
test_transpile("lvalue corner: inc on undef array elem", 'my @a; $a[0]++; print defined($a[0]) ? $a[0] : "undef", "\n";');
test_transpile("lvalue corner: inc on undef hash elem", 'my %h; $h{x}++; print defined($h{x}) ? $h{x} : "undef", "\n";');
test_transpile("lvalue corner: chop empty string in array", 'my @a = (""); chop($a[0]); print length($a[0]), "\n";');
test_transpile("lvalue corner: multiple elem inc", 'my @a = (1, 2, 3); $a[0]++; $a[1]++; $a[2]++; print join(",", @a), "\n";');

# L-value: pre-increment on assignment to array/hash element
test_transpile("lvalue: pre-inc on array elem assign", 'my @a; my $r = ++($a[0] = 5); print "$r $a[0]\n";');
test_transpile("lvalue: post-inc on array elem assign", 'my @a; my $r = ($a[0] = 5)++; print "$r $a[0]\n";');
test_transpile("lvalue: pre-inc on hash elem assign", 'my %h; my $r = ++($h{k} = 10); print "$r $h{k}\n";');
test_transpile("lvalue: post-inc on hash elem assign", 'my %h; my $r = ($h{k} = 10)++; print "$r $h{k}\n";');
test_transpile("lvalue: chop on array elem assign", 'my @a; chop($a[0] = "hello"); print "$a[0]\n";');
test_transpile("lvalue: chop on hash elem assign", 'my %h; chop($h{k} = "world"); print "$h{k}\n";');

done_testing();
