#!/usr/bin/env perl
# index-01.t - Tests for index/rindex runtime behavior
#
# Regression for p-rindex with empty substr and negative position:
#   rindex("abc", "", -1) was returning -1; should return 0.
#   Root cause: p-rindex checked (< start-num 0) → -1 BEFORE checking
#   empty substr. Empty substr with any negative pos should clamp to 0.
#
# Fix: moved empty-substr check before the negative-position check in
# p-rindex (cl/pcl-runtime.lisp:1197).

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

plan tests => 18;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code; close $fh;
    my $cl = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl; close $cl_fh;
    my $out = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    is(run_cl($code), $expected, $name);
}

# ── rindex empty string + negative position (regression test 27 fix) ─────────

test_cl('rindex empty string pos=-1 returns 0',
    'print rindex("abc", "", -1), "\n";', "0\n");

test_cl('rindex empty string pos=-100 returns 0',
    'print rindex("abc", "", -100), "\n";', "0\n");

# ── rindex empty string normal cases ─────────────────────────────────────────

test_cl('rindex empty string no-pos returns length',
    'print rindex("abc", ""), "\n";', "3\n");

test_cl('rindex empty string pos=0 returns 0',
    'print rindex("abc", "", 0), "\n";', "0\n");

test_cl('rindex empty string pos=1 returns 1',
    'print rindex("abc", "", 1), "\n";', "1\n");

test_cl('rindex empty string pos=2 returns 2',
    'print rindex("abc", "", 2), "\n";', "2\n");

test_cl('rindex empty string pos=3 returns 3',
    'print rindex("abc", "", 3), "\n";', "3\n");

test_cl('rindex empty string pos=4 clamps to length=3',
    'print rindex("abc", "", 4), "\n";', "3\n");

# ── rindex non-empty string + negative position still returns -1 ──────────────

test_cl('rindex non-empty string pos=-1 returns -1',
    'print rindex("abc", "a", -1), "\n";', "-1\n");

test_cl('rindex non-empty string pos=-5 returns -1',
    'print rindex("abc", "a", -5), "\n";', "-1\n");

# ── index normal cases ────────────────────────────────────────────────────────

test_cl('index basic',
    'print index("abcdef", "cd"), "\n";', "2\n");

test_cl('index not found returns -1',
    'print index("abcdef", "xyz"), "\n";', "-1\n");

test_cl('index with position',
    'print index("ababa", "a", 1), "\n";', "2\n");

test_cl('index empty string pos=0 returns 0',
    'print index("abc", "", 0), "\n";', "0\n");

test_cl('index empty string pos=2 returns 2',
    'print index("abc", "", 2), "\n";', "2\n");

test_cl('index empty string past end clamps to length',
    'print index("abc", "", 4), "\n";', "3\n");

# ── rindex normal search cases ────────────────────────────────────────────────

test_cl('rindex finds last occurrence',
    'print rindex("ababa", "a"), "\n";', "4\n");

test_cl('rindex with position limits search',
    'print rindex("ababa", "a", 3), "\n";', "2\n");
