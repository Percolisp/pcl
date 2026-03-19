#!/usr/bin/env perl
# misc-fixes-01.t - Small targeted fixes: auto-increment, splice scalar

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

plan tests => 12;

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

sub test_cl {
    my ($name, $code, $expected) = @_;
    my $got = run_cl($code);
    is($got, $expected, $name);
}

# ── auto.t test 32: ++("99a") → numeric 100 ──────────────────────────────
# Perl string increment only applies to strings matching /^[a-zA-Z]*[0-9]*$/
# "99a" (digits then letter) does NOT match → numeric increment → 100

test_cl('++("99a") gives 100 (numeric, not string)',
    'my $foo; my $r = ++($foo = "99a"); print $r, "\n";',
    "100\n");

test_cl('++("99\\0a") gives 100 (NUL breaks string increment)',
    'my $foo; my $r = ++($foo = "99\0a"); print $r, "\n";',
    "100\n");

# Regression: pure-alpha strings still do string increment
test_cl('++("az") stays string: "ba"',
    'my $foo; my $r = ++($foo = "az"); print $r, "\n";',
    "ba\n");

test_cl('++("zz") stays string: "aaa"',
    'my $foo; my $r = ++($foo = "zz"); print $r, "\n";',
    "aaa\n");

test_cl('++("A99") stays string: "B00"',
    'my $foo; my $r = ++($foo = "A99"); print $r, "\n";',
    "B00\n");

test_cl('++("99") numeric: 100',
    'my $foo; my $r = ++($foo = "99"); print $r, "\n";',
    "100\n");

# ── splice.t test 21: splice in scalar context returns last removed element

test_cl('splice scalar: returns last removed element',
    'my @a = qw(red green blue);
     my $foo = splice @a, 1, 2;
     print $foo, "\n";',
    "blue\n");

test_cl('splice scalar: returns single removed element',
    'my @a = qw(a b c d);
     my $x = splice @a, 1, 1;
     print $x, "\n";',
    "b\n");

test_cl('splice scalar: array modified correctly',
    'my @a = qw(red green blue);
     splice @a, 1, 2;
     print join(",", @a), "\n";',
    "red\n");

test_cl('splice list context: returns all removed elements',
    'my @a = qw(red green blue);
     my @r = splice @a, 1, 2;
     print join(",", @r), "\n";',
    "green,blue\n");

test_cl('splice scalar: remove and replace',
    'my @a = (1,2,3,4,5);
     my $r = splice @a, 1, 2, 10, 20;
     print "$r ", join(",", @a), "\n";',
    "3 1,10,20,4,5\n");

test_cl('splice list: returns removed with replacement',
    'my @a = (1,2,3,4,5);
     my @r = splice @a, 1, 2, 10, 20;
     print join(",", @r), " ", join(",", @a), "\n";',
    "2,3 1,10,20,4,5\n");
