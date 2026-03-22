#!/usr/bin/env perl
# sort-01.t - Tests for sort operator code generation and runtime behavior
#
# Documents failures identified from perl-tests/sort.t:
#   1. sort NAME LIST not generating function-reference form (generates wrong call)
#   2. $a/$b undefined in named sort comparator subs (no defvar emitted)
#   3. p-sort not binding $a/$b dynamically before calling comparator
#
# See docs/v1-implementation-plan.md B5 for context.

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

plan tests => 16;

# ── Helpers ─────────────────────────────────────────────────────────────────

sub transpile {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return scalar `$pl2cl $pl_file 2>/dev/null`;
}

sub run_cl {
    my ($code) = @_;
    my $cl_code = transpile($code);
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

# ── Transpilation tests ─────────────────────────────────────────────────────

# Test 1: sort { BLOCK } LIST → inline lambda with $a/$b params
{
    my $cl = transpile('sort { $a cmp $b } @arr;');
    like($cl, qr/p-sort.*lambda.*\$a.*\$b/s,
         'sort { BLOCK } LIST generates inline lambda with $a/$b');
}

# Test 2: sort NAME LIST → wrapped in lambda, not a bare call
{
    my $cl = transpile('sort compare @arr;');
    unlike($cl, qr/\(p-sort \(pl-compare/,
           'sort NAME LIST: comparator not called as bare function');
    like($cl, qr/p-sort.*pl-compare/s,
         'sort NAME LIST: comparator name appears in p-sort call');
}

# Test 3: $a and $b are declared (defvar) so named comparator subs can use them
{
    my $cl = transpile('sub backwards { $b cmp $a } my @s = sort backwards qw/c a b/;');
    like($cl, qr/defvar.*\$[ab]|\$[ab].*defvar/s,
         '$a and $b get defvar declarations for named comparator subs');
}

# Test 4: default sort (no comparator) emits p-sort without function arg
{
    my $cl = transpile('my @s = sort @arr;');
    like($cl, qr/p-sort/,
         'sort without comparator emits p-sort');
    unlike($cl, qr/p-sort.*lambda/s,
           'sort without comparator has no lambda');
}

# ── Runtime tests ────────────────────────────────────────────────────────────

# Test 5: basic numeric sort with block comparator
test_cl('sort { $a <=> $b } produces ascending order',
    'my @s = sort { $a <=> $b } (3, 1, 4, 1, 5, 9, 2);
     print join(" ", @s), "\n";',
    "1 1 2 3 4 5 9\n");

# Test 6: reverse numeric sort with block
test_cl('sort { $b <=> $a } produces descending order',
    'my @s = sort { $b <=> $a } (3, 1, 2);
     print join(" ", @s), "\n";',
    "3 2 1\n");

# Test 7: string sort with block comparator
test_cl('sort { $a cmp $b } produces lexical order',
    'my @s = sort { $a cmp $b } qw/banana apple cherry/;
     print join(" ", @s), "\n";',
    "apple banana cherry\n");

# Test 8: default sort (no comparator) is lexical
test_cl('default sort is lexical',
    'my @s = sort qw/banana apple cherry/;
     print join(" ", @s), "\n";',
    "apple banana cherry\n");

# Test 9: sort returns new list (original unchanged)
test_cl('sort does not modify original array',
    'my @orig = (3, 1, 2);
     my @s = sort { $a <=> $b } @orig;
     print join(" ", @orig), "\n";
     print join(" ", @s), "\n";',
    "3 1 2\n1 2 3\n");

# Test 10: sort with named comparator sub
test_cl('sort with named comparator sub',
    'sub num_cmp { $a <=> $b }
     my @s = sort num_cmp (3, 1, 2);
     print join(" ", @s), "\n";',
    "1 2 3\n");

# Test 11: sort with named reverse comparator
test_cl('sort with named reverse comparator',
    'sub rev_cmp { $b cmp $a }
     my @s = sort rev_cmp qw/banana apple cherry/;
     print join(" ", @s), "\n";',
    "cherry banana apple\n");

# Test 12: Schwartzian transform (map/sort/map)
# Use words with distinct lengths to avoid stability dependence
test_cl('Schwartzian transform',
    'my @words = qw/hi hello bye/;
     my @sorted = map { $_->[0] }
                  sort { $a->[1] <=> $b->[1] }
                  map { [$_, length($_)] } @words;
     print join(" ", @sorted), "\n";',
    "hi bye hello\n");

# Test 13: sort empty list
test_cl('sort empty list returns empty list',
    'my @s = sort ();
     print scalar(@s), "\n";',
    "0\n");

# Test 14: sort single element
test_cl('sort single element',
    'my @s = sort { $a <=> $b } (42);
     print join(" ", @s), "\n";',
    "42\n");
