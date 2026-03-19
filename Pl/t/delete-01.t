#!/usr/bin/env perl
# delete-01.t - Tests for delete/exists on array elements (delete.t failures)

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

plan tests => 8;

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

# ── delete.t test 28: delete $arr[N] → exists should return false ────────

test_cl('delete $foo[2] → exists is false',
    'my @foo = (1,2,3,4,5);
     delete $foo[2];
     print exists($foo[2]) ? "exists" : "absent", "\n";',
    "absent\n");

test_cl('delete $foo[2] → access returns undef',
    'my @foo = (1,2,3,4,5);
     delete $foo[2];
     print defined($foo[2]) ? "defined" : "undef", "\n";',
    "undef\n");

test_cl('delete returns old value',
    'my @foo = (1,2,3,4,5);
     my $r = delete $foo[2];
     print $r, "\n";',
    "3\n");

test_cl('non-deleted elements still exist',
    'my @foo = (1,2,3,4,5);
     delete $foo[2];
     print exists($foo[0]) ? "1" : "0",
           exists($foo[1]) ? "1" : "0",
           exists($foo[2]) ? "1" : "0",
           exists($foo[3]) ? "1" : "0", "\n";',
    "1101\n");

test_cl('$a[i] = undef: still exists',
    'my @foo = (1,2,3);
     $foo[1] = undef;
     print exists($foo[1]) ? "exists" : "absent", "\n";',
    "exists\n");

# ── delete.t test 36-37: delete multiple elements ────────────────────────

test_cl('delete multiple: $foo[4] and $foo[5] absent',
    'my @foo = (0,1,2,3,4,5,6,7);
     delete $foo[4];
     delete $foo[5];
     print exists($foo[4]) ? "4exists" : "4absent",
           exists($foo[5]) ? "5exists" : "5absent", "\n";',
    "4absent5absent\n");

# ── Regression: basic array access still works after delete ──────────────

test_cl('array still usable after delete',
    'my @foo = (1,2,3,4,5);
     delete $foo[2];
     print join(",", map { defined $_ ? $_ : "U" } @foo), "\n";',
    "1,2,U,4,5\n");

test_cl('delete hashref chain: keys after delete',
    'my %refhash;
     $refhash{"top"}{"foo"} = "FOO";
     $refhash{"top"}{"bar"} = "BAR";
     delete $refhash{"top"}{"bar"};
     my @list = sort keys %{$refhash{"top"}};
     print "@list\n";',
    "foo\n");
