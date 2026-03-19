#!/usr/bin/env perl
# Transpile tests part 5: foreach-over-arrayrefs and sprintf-style arrayref iteration

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_transpile {
    my ($name, $code, $expected) = @_;
    my $got = run_cl($code);
    is($got, $expected, $name);
}

# ── Issue 1: for over literal list of arrayrefs ──────────────────────────────
# Old bug: gen_progn emitted (progn aref1 aref2) which returns only the last
# element; p-flatten-for-list then spread its contents.  Result: iterated over
# the *contents* of [2,"b"] instead of over both arrayrefs.

test_transpile("for literal arrayrefs: iteration count", '
my $n = 0;
for ([1,"a"], [2,"b"]) { $n++ }
print "$n\n";
', "2\n");

test_transpile("for literal arrayrefs: each \$_ is a ref", '
for ([1,"a"], [2,"b"]) { print ref($_), "\n" }
', "ARRAY\nARRAY\n");

test_transpile("for literal arrayrefs: access elements", '
for ([10,"x"], [20,"y"]) { print $_->[0], " ", $_->[1], "\n" }
', "10 x\n20 y\n");

# ── Issue 2: sprintf-style arrayref iteration (sprintf.t regression) ─────────
# Session 78 broke sprintf.t (0/566 -> 0/473).  The test builds @tests as an
# array of arrayrefs [fmt, [args...], expected], then iterates and unpacks.
# Regression test: this pattern must work end-to-end.

test_transpile("sprintf-style: push arrayrefs then iterate", '
my @tests;
push @tests, ["%d",   [42],      "42"];
push @tests, ["%s",   ["hello"], "hello"];
push @tests, ["%.2f", [3.14159], "3.14"];
my ($pass, $fail) = (0, 0);
for (@tests) {
    my ($fmt, $args, $exp) = @$_;
    my $x = sprintf($fmt, @$args);
    if ($x eq $exp) { $pass++ } else { $fail++ }
}
print "pass=$pass fail=$fail\n";
', "pass=3 fail=0\n");

test_transpile("sprintf-style: iteration count is not spread", '
my @tests;
push @tests, ["a", [1], "x"];
push @tests, ["b", [2], "y"];
push @tests, ["c", [3], "z"];
my $n = 0;
for (@tests) { $n++ }
print "$n\n";
', "3\n");

test_transpile("sprintf-style: unpack nested arrayref field", '
my @tests;
push @tests, ["%d %d", [10, 20], "10 20"];
for (@tests) {
    my ($fmt, $args, $exp) = @$_;
    my $x = sprintf($fmt, @$args);
    print $x eq $exp ? "ok" : "fail:$x", "\n";
}
', "ok\n");

done_testing;
