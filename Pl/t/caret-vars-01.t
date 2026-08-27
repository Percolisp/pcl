#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# caret-vars-01.t — the caret / punctuation MAGIC SCALARS, read and written.
# Every expectation is the live `perl` answer.
#
#   #565  $^R was `(defvar |$^R| nil)` — a RAW value, not a p-box.  box-set
#         silently returns when its place is not a p-box (that is how it lets
#         a write to *p-undef* be a no-op), so `$^R = 7` STORED NOTHING and the
#         read that followed answered undef.  The spelling was never the
#         problem: the assignment and the read both name |$^R|.  A magic scalar
#         a program may ASSIGN to has to be a box, like $^P/$^D/$^F/$^I/$^M.
#
# NOT covered here, and deliberately: `local` on a caret variable is a silent
# no-op (task #600) — the `local` let binds the BARE symbol `$^P` while every
# read emits `|$^P|`, so the two are different symbols under :invert.  That is
# the local-target storage-name authority, not this.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 2;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

# Transpile (PCLCore::transpile FAILS the row on a dropped statement) and run.
sub run_cl {
    my ($code) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub both_agree {
    my ($code, $desc) = @_;
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- #565: $^R round-trips ------------------------------------------------

both_agree(<<'PL', '$^R: undef before any (?{}) block, then read/write round-trip');
print "pre-def=", (defined($^R) ? 1 : 0), " pre=[$^R]\n";
$^R = 7;
print "num=[$^R] plus=", $^R + 1, "\n";
$^R = "str";
print "str=[$^R]\n";
PL

both_agree(<<'PL', '$^R is a real container: a reference to it writes through');
$^R = 1;
my $r = \$^R;
$$r = 99;
print "via-ref=[$^R]\n";
PL
