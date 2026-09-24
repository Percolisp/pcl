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
# backtrace is the diagnosis the sweep's crash triage reads.  Rows 10-11 were
# that boundary for a method call on undef until s495f made that one a p-die
# (#2051); they now assert perl's number for it (#1427 stays open for the
# other bare-`error` sites).
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

plan tests => 23;

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

# THE BOUNDARY this row guarded CLOSED FOR ITS SHAPE (s495f, task #2051): a
# method call on undef used to be a bare CL `error` -- backtrace, exit 1 -- and
# p-method-call's three "Can't call method" errors are p-die now, so the
# uncaught one takes perl's one line and perl's 255.  The rest of #1427 (the
# census of the OTHER bare-`error` sites) stays open; this row now asserts
# perl's answer for the shape that was fixed.
{
    my ($perl, $pcl) = status_of('10-cl-error',
        '$! = 0; $? = 0; my $x; $x->method_that_is_not_there();');
    is($perl, 255, 'perl exits 255 for its own runtime error (oracle)');
    is($pcl, $perl, 'a method call on undef is a perl die: 255, like perl (#2051, s495f)');
}

# `$? = N` STICKS since s495f ($? is a magic box, tasks #1426/#2031/#2009), so
# the `$? >> 8` branch of the uncaught-die status is reachable from the
# program's own assignment, not only from a real child's status.
{
    my ($perl, $pcl) = status_of('11-set-child',
        '$! = 0; $? = 256; die "boom\n";');
    is($perl, 1, 'perl uses $? >> 8 when $! is 0 (oracle)');
    is($pcl, $perl, 'an explicit `$? = N` is stored, so die exits $? >> 8 like perl (#1426)');
}

# THE END PHASE AND $? (task #2009, #2031; s495f).  perl: END blocks see in $?
# the status the program is leaving with (exit N / an uncaught die's / 0 at the
# natural end, even after a failed system), an END may ASSIGN $? and the next
# (LIFO) block sees it, and the process then exits with $? & 255.  `$? = N`
# sticks everywhere (compound, list assignment, through a sub, through \$?) and
# stores an INTEGER.  Each row compares STDOUT and exit status with perl's.
# INVERSE GUARDS: exit in END ends only that block, POSIX::_exit skips END,
# and a natural end after `system("false")` still reads 0 in END.
sub out_and_status {
    my ($tag, $body) = @_;
    my $src = "$workdir/$tag.pl";
    open(my $s, '>', $src) or die; print {$s} $body; close $s;
    my $perl_out = `$^X $src 2>/dev/null`;
    my $perl = $? >> 8;
    my $cl_code = `$pl2cl $src 2>/dev/null`;
    my $cl_file = "$workdir/$tag.lisp";
    open(my $c, '>', $cl_file) or die; print {$c} $cl_code; close $c;
    my $pcl_out = `sbcl @sbcl_rt --load $cl_file 2>/dev/null`;
    my $pcl = $? >> 8;
    return ("$perl_out|rc=$perl", "$pcl_out|rc=$pcl");
}
my @end_cases = (
  ['e1-exit3',    'END { print "end=$?\n" } exit 3;'],
  ['e2-endset',   'END { $? = 5 } exit 3;'],
  ['e3-mask',     'END { $? = 300 } exit 0;'],
  ['e4-natural',  'system("false"); print "sys=$?\n"; END { print "end=$?\n" }'],
  ['e5-die',      '$! = 2; END { print "end=$?\n" } die "boom\n";'],
  ['e6-lifo',     'END { print "A=$?\n" } END { print "B=$?\n"; $? = 4 } exit 2;'],
  ['e7-exit-end', 'END { print "second=$?\n" } END { exit 7 } print "x\n";'],
  ['e8-assign',   '$? = 5; $? += 1; $? |= 8; print "a=$?\n"; ($?) = (4); print "b=$?\n"; sub s9 { $? = 9 } s9(); print "c=$?\n"; $? = "7abc"; print "d=$?\n"; $? = 3.7; print "e=$?\n"; my $r = \$?; $$r = 2; print "f=$?\n";'],
  ['e9-local',    '$? = 14; { local $? = 3; print "in=$?\n"; system("false"); print "sys=$?\n" } print "out=$?\n";'],
  ['e10-posix',   'use POSIX (); END { print "end\n" } POSIX::_exit(6);'],
);
for my $c (@end_cases) {
    my ($perl, $pcl) = out_and_status(@$c);
    is($pcl, $perl, "END phase / \$? $c->[0] matches perl");
}
