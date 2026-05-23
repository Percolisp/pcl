#!/usr/bin/env perl
# aassign-01.t — AASSIGN_COMMON self-assignment regression tests
# Covers the p-array-= fix that snapshots the RHS before clearing the LHS.
# Without the fix, @a = @a clears @a and then reads from the (now empty) array.

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

# ── Self-assignment: @a = @a ──────────────────────────────────────────────────
# Without the AASSIGN_COMMON fix, p-array-= clears fill-pointer before reading,
# so @a = @a emptied the array.

test_cl('@a = @a preserves elements',
    'our @a = (1, 2, 3); @a = @a; print "@a\n";',
    "1 2 3\n");

test_cl('@a = @a with strings',
    'our @a = ("foo", "bar", "burbl", "blah"); @a = @a; print "@a\n";',
    "foo bar burbl blah\n");

# ── Embedding self: @a = (x, @a, y) ─────────────────────────────────────────
# The nested @a inside (vector x @a y) was not snapshotted before clearing.

test_cl('@a = (100, @a, 200) embeds self correctly',
    'our @a = ("bar", "burbl", "blah"); @a = (100, @a, 200); print "@a\n";',
    "100 bar burbl blah 200\n");

test_cl('@a = ("X", @a, "Y") with string context',
    'our @a = ("bar", "burbl", "blah"); @a = ("XXX", @a, "YYY"); print "@a\n";',
    "XXX bar burbl blah YYY\n");

# ── List assignment with self-reference: (undef, @a) = @a ───────────────────
# p-list-= uses %p-flatten-list which already snapshots; ensure it still works.

test_cl('(undef, @a) = @a shifts first element',
    'our @a = ("foo", "bar", "burbl", "blah"); (undef, @a) = @a; print "@a\n";',
    "bar burbl blah\n");

# ── Chained: @a = @a; then embed ─────────────────────────────────────────────

test_cl('sequential self-assigns stay consistent',
    'our @a = (1,2,3); @a = @a; @a = (0, @a); print "@a\n";',
    "0 1 2 3\n");

# ── nil (deleted element) preservation through p-array-= ─────────────────────
# The snapshot must not drop nil slots (which mark deleted elements).

test_cl('delete then @a = @a preserves deleted slot',
    'my @a = (1,2,3,4); delete $a[1]; @a = @a;
     print exists($a[1]) ? "exists" : "deleted", "\n";',
    "deleted\n");

test_cl('delete then @a = reverse @a preserves nil slot',
    'my @a = (1,2,3,4); @a = reverse @a;
     delete $a[1];
     @a = reverse @a;
     print exists($a[2]) ? "exists" : "deleted", "\n";',
    "deleted\n");
