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
#   #571  $^E and $^C were absent from ExprToCL's %SPECIAL_VARS, so they fell
#         through to the ordinary global path, emitted as a bare `$^E` token,
#         and — because generated code loads under :invert, where an all-upper
#         token reads DOWN-cased — aborted the whole FILE with "The variable
#         $^e is unbound".  On POSIX perl's $^E *is* $!, probed identical in
#         both directions, so it maps onto the same ['p-errno-string'] the `$!`
#         entry uses rather than onto an inert cell of its own (a cell would
#         read "" after a failed syscall — silent wrong).  $^C is 0 at run time
#         in perl and PCL has no -c, so a box holding 0 is exact.
#
#   #573  $: (FORMAT_LINE_BREAK_CHARACTERS) read " n-" instead of " \n-".  A
#         CL string literal has no \n escape — the reader consumes a backslash
#         as "the next character literally" — so `(make-p-box " \n-")` built a
#         three-character string with a LETTER n in the middle.  It was the one
#         default-value mismatch among the 45 punctuation/caret names, and a
#         scan of every CL string literal in cl/*.lisp for a backslash before
#         an alphanumeric finds no second instance.
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

plan tests => 6;

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

# ---- #571: $^E and $^C exist ----------------------------------------------

# $^E on POSIX is $! — the SAME variable, in both directions.  The row proves
# the alias, not one particular errno string: it sets $! and reads $^E, then
# sets $^E and reads $!.
both_agree(<<'PL', '$^E is $! on POSIX: read, write and numeric context');
$! = 2;
printf "a=[%s] anum=[%d]\n", $^E, $^E + 0;
print "same=", (("$!" eq "$^E") ? 1 : 0), "\n";
$^E = 13;
printf "b=[%s] bnum=[%d]\n", $!, $! + 0;
PL

both_agree(<<'PL', '$^E after a FAILED syscall is the errno string, not ""');
open(my $fh, '<', '/no/such/file/xyzzy-pcl') or 1;
print "E=[$^E]\n";
print "nonempty=", (length("$^E") ? 1 : 0), "\n";
PL

both_agree(<<'PL', '$^C is a defined 0 at run time, and writable');
print "C=[$^C] def=", (defined($^C) ? 1 : 0), "\n";
$^C = 1;
print "C2=[$^C]\n";
PL

# ---- #573: $: default value ------------------------------------------------

# The characters, not the rendering: printing " \n-" straight would compare a
# literal newline and hide a one-character slip.  ord() names each one.
both_agree(<<'PL', '$: defaults to the three chars space, NEWLINE, hyphen');
print "len=", length($:), " ords=", join(",", map { ord } split //, $:), "\n";
$: = "xy";
print "after=[$:]\n";
PL
