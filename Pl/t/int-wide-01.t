#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# int-wide-01.t — int() of a double, on every range (task #3528).
#
# p-int's double-float arm is RANGED since #3528: inside +-4.5e18 the
# truncation is open-coded (SBCL 2.5.2 -- the supported floor -- could not
# compile the named-call form `int($pos / $n)' takes inside a loop when the
# quotient arrives boxed: "sb-vm::move-double-float-arg is not the name of a
# defined VOP", which killed every `pack' call on 2.5.2 because the pack
# artifact's template sizer has that shape).  Outside the range a finite
# double goes through %p-int-wide, NaN and the infinities come back as
# themselves.  Row 1 pins the VALUES on all three paths, perl 5.40.3 as the
# oracle.  Row 2 is the compile shape itself: under SBCL 2.5.2 on 6cf75804 it
# dies with the VOP error (the named callee is
# sb-bignum:unary-truncate-double-float-to-bignum); tools/t/sbcl-floor.t runs
# this file under the floor.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt      = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

my $workdir = tempdir(CLEANUP => 1);

sub both {
    my ($tag, $body) = @_;
    my $src = "$workdir/$tag.pl";
    open(my $s, '>', $src) or die; print {$s} $body; close $s;
    my $perl = `$^X $src 2>&1`;
    my $cl_code = `$pl2cl $src 2>/dev/null`;
    my $cl_file = "$workdir/$tag.lisp";
    open(my $c, '>', $cl_file) or die; print {$c} $cl_code; close $c;
    my $pcl = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $pcl =~ s/^;.*\n//gm;
    $pcl =~ s/^PCL Runtime loaded\n//gm;
    $pcl =~ s/^\s*\n//gm;
    return ($perl, $pcl);
}

my ($perl, $pcl) = both('values', <<'PL');
my @x = (7/2, -7/2, 0.5, -0.5, 1e18, -1e18, 4.4e18, -4.4e18, 4.6e18, -4.6e18,
         1e19, 1.8e19, 9**9**9, -9**9**9, 2**62, -2**62, 2**63);
# (-1e19 and 1.5e300 are left out: perl answers an NV beyond the IV/UV range
#  and prints -1e+19 / 1.5e+300, PCL the exact integer -- before AND after
#  #3528, task #3700.)
print join(" ", map { int($_) } @x), "\n";
my $nan = 9**9**9 / 9**9**9;
print int($nan) == int($nan) ? "nan-eq\n" : "nan-ne\n";
my $pos = 0;
for my $n (3, 4, 7) { $pos = int(($pos + 10) / $n) * $n }
print "$pos\n";
PL
is($pcl, $perl, 'int() of a double: in range, beyond +-4.5e18, the infinities, NaN, inside a loop');

($perl, $pcl) = both('sizer', <<'PL');
sub sz {
    my ($t, $nrep) = @_;
    my ($pos, $i) = (0, 0);
    while (1) {
        last if $i >= length $t;
        my $ch = substr($t, $i, 1); $i++;
        if ($ch eq 'X') { my $n = $nrep; $pos = int($pos / $n) * $n; next }
        if ($ch eq 'B') { $pos += int(($nrep + 7) / 8); next }
        $pos += $nrep;
    }
    return $pos;
}
print sz("aaXaB", 2), " ", sz("XBXB", 3), "\n";
PL
is($pcl, $perl, 'the pack template sizer\'s int(/) shapes inside a while loop (#3528)');

done_testing;
