#!/usr/bin/env perl
# range-01.t - Tests for range operator edge cases from range.t failures

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

# ── range.t test 34: non-alphanumeric start → just return start ───────────
# "*x" contains "*" so magical increment doesn't apply → return ("*x")

test_cl('"*x".."az" → just "*x" (non-magical start)',
    'print join(":", "*x".."az"), "\n";',
    "*x\n");

# ── Tests 44-48: undef/empty strings with "B" ────────────────────────────
# Perl: undef treated as "" for magical (non-numeric) ranges

test_cl('"" .. "B" → ("") — empty start in string range',
    'my @r = map { "[$_]" } ("".."B");
     print join(":", @r), "\n";',
    "[]\n");

test_cl('undef .. "B" → ("") — undef as "" for string range',
    'my @r = map { "[$_]" } (undef.."B");
     print join(":", @r), "\n";',
    "[]\n");

test_cl('"B" .. "" → () — empty end stops range',
    'my @r = map { "[$_]" } ("B".."");
     print scalar(@r), "\n";',
    "0\n");

test_cl('"B" .. undef → () — undef end stops range',
    'my @r = map { "[$_]" } ("B"..undef);
     print scalar(@r), "\n";',
    "0\n");

test_cl('undef .. undef → ("") — both undef',
    'my @r = map { "[$_]" } (undef..undef);
     print join(":", @r), "\n";',
    "[]\n");

# ── Tests 53-57: same but with foreach loops ────────────────────────────

test_cl('foreach undef.."B" → ("")',
    'my @foo; push @foo, $_ for undef.."B";
     my @r = map { "[$_]" } @foo;
     print join(":", @r), "\n";',
    "[]\n");

test_cl('foreach "".."B" → ("")',
    'my @foo; push @foo, $_ for "".."B";
     my @r = map { "[$_]" } @foo;
     print join(":", @r), "\n";',
    "[]\n");

test_cl('foreach "B"..undef → ()',
    'my @foo; push @foo, $_ for "B"..undef;
     print scalar(@foo), "\n";',
    "0\n");

test_cl('foreach "B".."" → ()',
    'my @foo; push @foo, $_ for "B".."";
     print scalar(@foo), "\n";',
    "0\n");

test_cl('foreach undef..undef → ("")',
    'my @foo; push @foo, $_ for undef..undef;
     my @r = map { "[$_]" } @foo;
     print join(":", @r), "\n";',
    "[]\n");

# ── Regression: normal string ranges still work ─────────────────────────

test_cl('"ax".."az" still works',
    'print join(":", "ax".."az"), "\n";',
    "ax:ay:az\n");
