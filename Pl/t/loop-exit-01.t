#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# loop-exit-01.t — s470bi, task #1022 half (a): AN UNLABELLED last/next/redo
# WHOSE LOOP IS NOT LEXICALLY HERE IS LOUD.
#
# perl's unlabelled `last`/`next`/`redo` exits the innermost DYNAMICALLY
# enclosing loop, so a bare `last` in a sub called from a loop exits the
# CALLER's loop.  PCL lowers it to a LEXICAL CL exit, which is not that, and
# it failed in two ways — one of them silent:
#
#   sub do_last { last }  for my $i (1..3) { $n++; do_last(); $n += 100 }
#     perl n=1    PCL n=303   — the `last` did NOTHING and the loop ran on
#   sub do_next { next }  while ($g++ < 3) { $n++; do_next(); $n += 100 }
#     perl n=3    PCL: SBCL "attempt to GO to nonexistent tag: :next"
#
# Until the dynamic half (#1022 (b)) ships, such a statement DIES: at run
# time, at its own site, trappably, naming the sub — the Fable ruling of s470,
# which also settled that the s329 announce-and-continue boundary does not
# cover it (an untaken loop exit changes the CALLER's control flow).
#
# The INVERSE rows are the point of this file as much as the loud ones: every
# ordinary spelling of a loop exit must be untouched, and the loop shapes are
# PPI's own compound types — missing `for` (the C-style one) was a real bug in
# the first version, caught by corpus-diff on perl-tests/my.t and time.t.

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

plan tests => 8;

# Transpile (a DROPPED statement fails the row, via PCLCore) and run; stderr
# is kept, because the whole point of half (a) is what lands on it.
sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = PCLCore::transpile("$pl2cl $pl_file");
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# ---- 1. the SILENT half: repro A ------------------------------------------
like(run_cl(<<'PERL'), qr/PCL: unsupported: "last" exiting subroutine do_last/,
sub do_last { last }
my $n = 0;
for my $i (1..3) { $n++; do_last(); $n += 100; }
print "n=$n\n";
PERL
     'a bare `last` in a called sub is LOUD (it silently ran the loop out: n=303 vs perl 1)');

unlike(run_cl(<<'PERL'), qr/n=303/,
sub do_last { last }
my $n = 0;
for my $i (1..3) { $n++; do_last(); $n += 100; }
print "n=$n\n";
PERL
       '... and the wrong answer is not printed any more');

# ---- 2. the CRASHING half: repro B ----------------------------------------
like(run_cl(<<'PERL'), qr/PCL: unsupported: "next" exiting subroutine do_next/,
sub do_next { next }
my $n = 0; my $g = 0;
while ($g++ < 3) { $n++; do_next(); $n += 100; }
print "n=$n\n";
PERL
     'a bare `next` in a called sub names itself instead of "GO to nonexistent tag"');

# ---- 3. TRAPPABLE, and perl-shaped where perl has its own text -------------
like(run_cl(<<'PERL'), qr/^caught: PCL: unsupported: "last" exiting subroutine f\b/m,
sub f { last }
eval { for my $i (1..2) { f() } };
print "caught: $@";
PERL
     '... and it is a trappable run-time die, not a load-time death');

like(run_cl(<<'PERL'), qr/Can't "last" outside a loop block/,
last;
print "after\n";
PERL
     'a bare `last` with no loop ANYWHERE keeps perl\'s own text');

# ---- 4. INVERSES: every ordinary loop exit is untouched --------------------
is(run_cl(<<'PERL'), "sum=3\nc=1\nlabelled=1\n",
my $sum = 0;
for my $i (1..5) { last if $i > 2; $sum += $i }     # foreach
print "sum=$sum\n";
my $c = 0;
for (my $j = 0; $j < 9; $j++) { last if $j > 1; $c = $j }   # C-style for
print "c=$c\n";
my $ok = 0;
sub exit_outer { last OUTER }
OUTER: { $ok = 1; exit_outer(); $ok = 0 }
print "labelled=$ok\n";
PERL
   'inverse: foreach, C-style for and a LABELLED exit from a sub all still work');

is(run_cl(<<'PERL'), "bare=7 8\nwhile=3\n",
sub g { my @x = (7,8); { last if $x[0] > 99; @x } }
print "bare=@{[ g() ]}\n";
my $n = 0;
sub h { my $i = 0; while (1) { $i++; last if $i >= 3 } $i }
print "while=", h(), "\n";
PERL
   'inverse: a BARE BLOCK and a `while` inside the sub are loops, so no die');

is(run_cl(<<'PERL'), "n=3\n",
my $n = 0;
for my $i (1..3) { $n++; next; $n += 100 }
print "n=$n\n";
PERL
   'inverse: a plain `next` in its own loop is unchanged');
