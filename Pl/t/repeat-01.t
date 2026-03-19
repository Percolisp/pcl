#!/usr/bin/env perl
# repeat-01.t - Tests for list repetition (x operator) failures from perl-tests/repeat.t

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

plan tests => 10;

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

# ── repeat.t test 29: (@x,1) x Y ─────────────────────────────────────────
# (@x,1) is a list with array elements + extra literal; x N repeats the list

test_cl("(\@x,1) x 4: list with array + literal repeated",
    'my @x = (1,2,3);
     my @r = (@x,1) x 4;
     print join(":", @r), "\n";',
    "1:2:3:1:1:2:3:1:1:2:3:1:1:2:3:1\n");

# ── repeat.t test 32: (X,X) x Y ──────────────────────────────────────────
# ($x,$x) is a 2-element list; x 4 should give 8 elements

test_cl("(\$x,\$x) x 4: two-variable list repeated",
    'my $x = 9;
     my @r = ($x,$x) x 4;
     print join(":", @r), "\n";',
    "9:9:9:9:9:9:9:9\n");

# ── repeat.t test 33: split and x ────────────────────────────────────────
# split result used with x

test_cl("(split) x 2: parenthesized split result repeated",
    'my @r = (split(/,/, "1,2,3")) x 2;
     print join(":", @r), "\n";',
    "1:2:3:1:2:3\n");

# ── Baseline: already-passing cases (regression guard) ───────────────────

test_cl("string x N: basic string repetition",
    'my $s = "ab" x 3;
     print $s, "\n";',
    "ababab\n");

test_cl("\@arr x N: array in scalar context gives string repetition",
    'my @x = (1,2,3);
     my @r = @x x 2;
     print join(":", @r), "\n";',
    "33\n");  # @x scalar = 3, "3" x 2 = "33", @r gets one element "33"

test_cl("(\@arr) x N: parenthesized array list repetition",
    'my @x = (1,2,3);
     my @r = (@x) x 2;
     print join(":", @r), "\n";',
    "1:2:3:1:2:3\n");

test_cl("(1,2,3) x N: literal list repetition",
    'my @r = (1,2,3) x 3;
     print join(":", @r), "\n";',
    "1:2:3:1:2:3:1:2:3\n");

test_cl("() x N: empty list repetition",
    'my @r = () x 5;
     print scalar @r, "\n";',
    "0\n");

# ── repeat.t test 43: (...)x... in void context in list ──────────────────

test_cl("list repeat in scalar context gives element count",
    'my @x = (1,2);
     my $n = () = (@x,3) x 4;
     print $n, "\n";',
    "12\n");

# ── (@x, extra) x N where @x is empty ────────────────────────────────────

test_cl("(empty_array, literal) x N",
    'my @x;
     my @r = (@x, 7) x 3;
     print join(":", @r), "\n";',
    "7:7:7\n");
