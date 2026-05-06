#!/usr/bin/env perl
# bless-lvalue-01.t - Bless preservation through all lvalue-setting paths
#
# Bugs found: blessed objects lose their class when stored via p-hash-= (both
# hash-table and vector paths), p-push-impl, p-unshift, and p-splice-impl.

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

plan tests => 26;

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

# ---- hash-based blessed ({}) - class stored in :__class__, already worked ----

test_cl('hash-bless: array-= preserves bless',
    'my $o = bless {}, "Foo";
     my @arr = ($o);
     print ref($arr[0]), "\n";',
    "Foo\n");

test_cl('hash-bless: array-= method call',
    'package Foo; sub name { "foo" }
     package main;
     my $o = bless {}, "Foo";
     my @arr = ($o);
     print $arr[0]->name(), "\n";',
    "foo\n");

test_cl('hash-bless: hash-= hash path',
    'my $o = bless {}, "Foo";
     my %src = (obj => $o);
     my %dst;
     %dst = %src;
     print ref($dst{obj}), "\n";',
    "Foo\n");

test_cl('hash-bless: hash-= vector path',
    'my $o = bless {}, "Foo";
     my %dst = (obj => $o);
     print ref($dst{obj}), "\n";',
    "Foo\n");

test_cl('hash-bless: push',
    'my $o = bless {}, "Foo";
     my @arr;
     push @arr, $o;
     print ref($arr[0]), "\n";',
    "Foo\n");

test_cl('hash-bless: unshift',
    'my $o = bless {}, "Foo";
     my @arr = (99);
     unshift @arr, $o;
     print ref($arr[0]), "\n";',
    "Foo\n");

test_cl('hash-bless: splice replacement',
    'my $o = bless {}, "Foo";
     my @arr = (1, 2);
     splice(@arr, 0, 1, $o);
     print ref($arr[0]), "\n";',
    "Foo\n");

test_cl('hash-bless: splice removed',
    'my $o = bless {}, "Foo";
     my @arr = ($o);
     my @r = splice(@arr, 0, 1);
     print ref($r[0]), "\n";',
    "Foo\n");

# ---- array-ref blessed ([]) - class ONLY on box, these were the real bugs ----

test_cl('array-bless: array-= preserves bless',
    'my $o = bless [], "Foo";
     my @arr = ($o);
     print ref($arr[0]), "\n";',
    "Foo\n");

test_cl('array-bless: hash-= vector path (my %h = (k=>$o))',
    'my $o = bless [], "Foo";
     my %h = (obj => $o);
     print ref($h{obj}), "\n";',
    "Foo\n");

test_cl('array-bless: hash-= hash path (%dst = %src)',
    'my $o = bless [], "Foo";
     my %src = (obj => $o);
     my %dst;
     %dst = %src;
     print ref($dst{obj}), "\n";',
    "Foo\n");

test_cl('array-bless: push preserves bless',
    'my $o = bless [], "Foo";
     my @arr;
     push @arr, $o;
     print ref($arr[0]), "\n";',
    "Foo\n");

test_cl('array-bless: push method call',
    'package Bar; sub id { "bar" }
     package main;
     my $o = bless [], "Bar";
     my @arr;
     push @arr, $o;
     print $arr[0]->id(), "\n";',
    "bar\n");

test_cl('array-bless: push multiple mixed',
    'my $o = bless [], "Foo";
     my @arr;
     push @arr, "plain", $o, 42;
     print ref($arr[1]), "\n";
     print $arr[0], "\n";',
    "Foo\nplain\n");

test_cl('array-bless: unshift preserves bless',
    'my $o = bless [], "Foo";
     my @arr = (1, 2);
     unshift @arr, $o;
     print ref($arr[0]), "\n";',
    "Foo\n");

test_cl('array-bless: unshift method call',
    'package Qux; sub val { "qux" }
     package main;
     my $o = bless [], "Qux";
     my @arr = (99);
     unshift @arr, $o;
     print $arr[0]->val(), "\n";',
    "qux\n");

test_cl('array-bless: splice replacement',
    'my $o = bless [], "Foo";
     my @arr = (1, 2, 3);
     splice(@arr, 1, 1, $o);
     print ref($arr[1]), "\n";',
    "Foo\n");

test_cl('array-bless: splice removed',
    'my $o = bless [], "Foo";
     my @arr = ($o, 2, 3);
     my @removed = splice(@arr, 0, 1);
     print ref($removed[0]), "\n";',
    "Foo\n");

test_cl('array-bless: push then hash-= compound',
    'my $o = bless [], "Foo";
     my @arr;
     push @arr, $o;
     my %src = (x => $arr[0]);
     my %dst;
     %dst = %src;
     print ref($dst{x}), "\n";',
    "Foo\n");

# ---- verify non-bless paths still work (regressions) ----

test_cl('regression: plain scalar through hash',
    'my %h = (k => 42);
     print $h{k}, "\n";',
    "42\n");

test_cl('regression: array-ref through hash',
    'my @arr = (1, 2, 3);
     my %h = (ref => \@arr);
     print ref($h{ref}), "\n";
     print $h{ref}[1], "\n";',
    "ARRAY\n2\n");

test_cl('regression: hash-ref through hash',
    'my %inner = (x => 42);
     my %h = (ref => \%inner);
     print ref($h{ref}), "\n";
     print $h{ref}{x}, "\n";',
    "HASH\n42\n");

test_cl('regression: push plain values',
    'my @arr;
     push @arr, 1, "hello", 3;
     print scalar @arr, "\n";
     print $arr[1], "\n";',
    "3\nhello\n");

test_cl('regression: unshift plain values',
    'my @arr = (3, 4);
     unshift @arr, 1, 2;
     print join(",", @arr), "\n";',
    "1,2,3,4\n");

test_cl('regression: splice plain insert',
    'my @arr = (1, 2, 3);
     my @r = splice(@arr, 1, 1, 10, 20);
     print join(",", @arr), "\n";
     print $r[0], "\n";',
    "1,10,20,3\n2\n");

test_cl('regression: hash-= with plain values',
    'my %src = (a => 1, b => 2);
     my %dst;
     %dst = %src;
     print $dst{a}, "\n";',
    "1\n");

