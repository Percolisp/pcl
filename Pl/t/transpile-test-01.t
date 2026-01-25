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

# TODO: File I/O tests - requires filehandle symbol quoting in code generator
# Currently filehandle symbols (FH, $fh) aren't quoted, causing CL eval errors
# test_transpile("file: write", '
#   open(my $fh, ">", "/tmp/pl2cl-test.txt") or die "Cannot open";
#   print $fh "Hello";
#   close($fh);
#   print "wrote file\n";
# ');

done_testing();
