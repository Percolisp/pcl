#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# die-exit-status-01.t — THE EXIT STATUS OF AN UNCAUGHT die (task #1247 (b)).
#
# perlfunc: "the exit value is $!, or $? >> 8 if $! is 0, else 255".  PCL let
# the condition reach SBCL's toplevel, which exits 1 for every one of them and
# prints a fifteen-frame BACKTRACE where perl prints one line — so a program
# that runs another and checks $? read a status PCL never produced, and the
# ir-conform corpus carried four rows for it (120-io, 288-loops-exits and,
# through the same handler, 022-refs).
#
# The hook is armed IN p-die and not at load, because SBCL processes
# `--non-interactive` while parsing its command line — after *init-hooks* and
# after a saved core has restored whatever the runtime set (measured: the hook
# was installed and SBCL's DEBUGGER-DISABLED-HOOK still ran).
#
# ONLY A PERL DIE IS RESHAPED.  An internal CL condition is a PCL BUG and its
# backtrace is the diagnosis the sweep's crash triage reads; rows 10-11 are
# that boundary, asserted as PCL's answer with the divergence named (#1427),
# because a guard that asserted perl's number there would be asserting a fix
# this file did not make.
#
# perl 5.40.3 IS the oracle: every row runs the same program both ways.

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

plan tests => 13;

my $workdir = tempdir(CLEANUP => 1);

# The status of the SAME program under perl and under PCL.
sub status_of {
    my ($tag, $body) = @_;
    my $src = "$workdir/$tag.pl";
    open(my $s, '>', $src) or die; print {$s} $body; close $s;
    system("$^X $src >/dev/null 2>&1");
    my $perl = $? >> 8;
    my $cl_code = `$pl2cl $src 2>/dev/null`;
    my $cl_file = "$workdir/$tag.lisp";
    open(my $c, '>', $cl_file) or die; print {$c} $cl_code; close $c;
    system("sbcl @sbcl_rt --load $cl_file >/dev/null 2>&1");
    my $pcl = $? >> 8;
    return ($perl, $pcl);
}

my @cases = (
  ['01-errno-2',   'open(my $f, "<", "/nonexistent-pcl-xyz"); die "boom\n";'],
  ['02-errno-0',   '$! = 0; die "boom\n";'],
  ['03-child-3',   '$! = 0; system("/bin/sh", "-c", "exit 3"); die "boom\n";'],
  ['04-both-0',    '$! = 0; $? = 0; die "boom\n";'],
  ['05-errno-13',  '$! = 13; die "boom\n";'],
  ['06-errno-wins','$! = 5; system("/bin/sh","-c","exit 7"); die "boom\n";'],
  ['07-require',   'print "before\n"; require "./no-such-pcl-file.pl";'],
  ['08-exit-0',    'print "x\n"; exit 0;'],
  ['09-exit-7',    'exit 7;'],
);

for my $c (@cases) {
    my ($perl, $pcl) = status_of(@$c);
    is($pcl, $perl, "uncaught-die exit status $c->[0] is perl's ($perl)");
}

# THE BOUNDARY, asserted as it IS.  A perl-level error PCL raises with a bare
# CL `error` (not p-die) still reaches SBCL's hook: backtrace, exit 1.  That is
# task #1427 — the census of those sites and the decision — and the row exists
# so that closing it FAILS here instead of passing unnoticed.
{
    my ($perl, $pcl) = status_of('10-cl-error',
        '$! = 0; $? = 0; my $x; $x->method_that_is_not_there();');
    is($perl, 255, 'perl exits 255 for its own runtime error (oracle)');
    is($pcl, 1, 'PCL still exits 1 for a runtime error raised as a CL condition (#1427)');
}

# `$? = N` does not stick in PCL, so the `$? >> 8` branch is reachable only
# from a real child's status (row 03 above).  Task #1426.
{
    my ($perl, $pcl) = status_of('11-set-child',
        '$! = 0; $? = 256; die "boom\n";');
    is($perl, 1, 'perl uses $? >> 8 when $! is 0 (oracle)');
    is($pcl, 255, 'PCL falls to 255 because an explicit `$? = N` is not stored (#1426)');
}
