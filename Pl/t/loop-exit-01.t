#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# loop-exit-01.t — task #1022: THE DYNAMIC LOOP EXIT.
#
# perl's unlabelled `last`/`next`/`redo` acts on the innermost DYNAMICALLY
# enclosing loop, so a bare `last` in a sub called from a loop exits the
# CALLER's loop.  PCL lowered it to a LEXICAL CL exit, which is not that, and
# it failed in two ways — one of them silent:
#
#   sub do_last { last }  for my $i (1..3) { $n++; do_last(); $n += 100 }
#     perl n=1    PCL n=303   — the `last` did NOTHING and the loop ran on
#   sub do_next { next }  while ($g++ < 3) { $n++; do_next(); $n += 100 }
#     perl n=3    PCL: SBCL "attempt to GO to nonexistent tag: :next"
#
# Half (a) (s470bi) made it a trappable die; half (b) (s470bl) PERFORMS the
# exit: a loop whose body can reach user code establishes ONE catch of
# `p-loop-dyn` per loop ENTRY (Kind-A gate `dyn-loop-exit`, `:dyn t`,
# %p-loop-driver / p-dyn-once) and the exit site throws to it.  Every
# expectation below is perl 5.40.3's, probed.
#
# The INVERSE rows are the point of this file as much as the positive ones:
# every ordinary spelling of a loop exit must be untouched, a loop with no
# call in its body must gain NO frame (that byte-identity is what keeps a
# counting loop free), and the two shapes PCL still refuses must stay LOUD.

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

plan tests => 22;

# Transpile (a DROPPED statement fails the row, via PCLCore) and run; stderr
# is kept, because some of these rows are about what lands on it.  $OPT is an
# optional PCL_OPT value for the transpile (the Kind-A gate rows).
sub transpile {
    my ($code, $opt) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $pre = defined $opt ? "PCL_OPT=$opt " : '';
    return PCLCore::transpile("$pre$pl2cl $pl_file");
}

sub run_cl {
    my ($code, $opt) = @_;
    my $cl_code = transpile($code, $opt);
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# ---- 1. the two original repros, now ANSWERED ----------------------------
is(run_cl(<<'PERL'), "n=1\n",
sub do_last { last }
my $n = 0;
for my $i (1..3) { $n++; do_last(); $n += 100; }
print "n=$n\n";
PERL
   'repro A: a bare `last` in a called sub exits the caller\'s foreach (was n=303)');

is(run_cl(<<'PERL'), "n=3 g=4\n",
sub do_next { next }
my $n = 0; my $g = 0;
while ($g++ < 3) { $n++; do_next(); $n += 100; }
print "n=$n g=$g\n";
PERL
   'repro B: a bare `next` re-tests the while condition (was "GO to nonexistent tag")');

# ---- 2. every loop shape, every keyword ----------------------------------
is(run_cl(<<'PERL'), "n=1\n",
sub f { last }
my $n = 0;
for (my $i = 0; $i < 3; $i++) { $n++; f(); $n += 100 }
print "n=$n\n";
PERL
   'C-style for: `last` from a called sub leaves it');

is(run_cl(<<'PERL'), "n=3\n",
sub f { next }
my $n = 0;
for (my $i = 0; $i < 3; $i++) { $n++; f(); $n += 100 }
print "n=$n\n";
PERL
   'C-style for: a caught `next` runs the STEP, so the loop still terminates');

is(run_cl(<<'PERL'), "n=305 c=5\n",
my $c = 0;
sub f { redo if $c++ < 2 }
my $n = 0;
for my $i (1..3) { $n++; f(); $n += 100 }
print "n=$n c=$c\n";
PERL
   'foreach: a caught `redo` re-runs the body without advancing (perl n=305)');

is(run_cl(<<'PERL'), "1 1 1 2 3\n",
my $c = 0;
sub f { redo if $c++ < 2 }
my @seen;
for my $i (1..3) { push @seen, $i; f() }
print "@seen\n";
PERL
   '... on the SAME element: the index is backed up, not left advanced');

is(run_cl(<<'PERL'), "n=1\n",
sub f { last }
my $n = 0;
until ($n > 9) { $n++; f(); $n += 100 }
print "n=$n\n";
PERL
   'until is a while: same frame');

# ---- 3. the BARE BLOCK — loopctl.t\'s own row and rt119311.t\'s shape ------
is(run_cl(<<'PERL'), "ok=1\n",
sub test_last { last }
my $ok = 0;
TEST41: { $ok = 1; test_last(); $ok = 0 }
print "ok=$ok\n";
PERL
   'perl-tests/loopctl.t "dynamically scoped": a labelled BARE BLOCK is a loop-once');

is(run_cl(<<'PERL'), "n=1\n",
sub foo { my ($b) = @_; $b->() }
my $n = 0;
{ $n++; foo(sub { last }); $n += 100 }
print "n=$n\n";
PERL
   't/op/rt119311.t\'s shape: `{ foo(sub { … last }) }` leaves the bare block');

is(run_cl(<<'PERL'), "n=103 c=3\n",
sub foo { my ($b) = @_; $b->() }
my $c = 0; my $n = 0;
{ $n++; foo(sub { redo if $c++ < 2 }); $n += 100 }
print "n=$n c=$c\n";
PERL
   '... and `redo` RESTARTS the loop-once (a bare block carries no state)');

# ---- 4. which loop it is ---------------------------------------------------
is(run_cl(<<'PERL'), "n=2\n",
sub g { last }
sub f { my $m = 0; for my $j (1..3) { $m++; g(); $m += 100 } return $m }
my $n = 0;
for my $i (1..2) { $n += f() }
print "n=$n\n";
PERL
   'the INNERMOST dynamically enclosing loop wins (f\'s, not the caller\'s)');

is(run_cl(<<'PERL'), "n=0\n",
sub g { last }
sub f { my $t = 0; for my $j (1..2) { $t += $j } g(); return $t }
my $n = 0;
for my $i (1..3) { $n += f(); $n += 100 }
print "n=$n\n";
PERL
   '... and a loop that has ALREADY FINISHED does not catch it');

is(run_cl(<<'PERL'), "n=1\n",
sub f { last }
my $n = 0;
for my $i (1..3) { $n++; my @y = map { f(); $_ } (1,2); $n += 100 }
print "n=$n\n";
PERL
   'a map BLOCK is transparent: the exit reaches the enclosing loop');

is(run_cl(<<'PERL'), "n=1 err=\n",
my $n = 0;
for my $i (1..3) { $n++; eval q{last}; $n += 100 }
print "n=$n err=$@\n";
PERL
   'a `last` in a STRING EVAL exits the loop the eval is in (was n=303, silently)');

# ---- 5. what is NOT a loop — LOUD, with perl\'s own text -------------------
like(run_cl(<<'PERL'), qr/Can't "last" outside a loop block/,
last;
print "after\n";
PERL
     'no loop ANYWHERE: perl\'s own text');

like(run_cl(<<'PERL'), qr/^caught: Can't "last" outside a loop block/m,
sub f { last }
eval { f() };
print "caught: $@";
PERL
     '... trappable, at the exit\'s own site');

like(run_cl(<<'PERL'), qr/^caught: Can't "last" outside a loop block/m,
sub f { last }
my $g = 0;
eval { do { f() } while ($g++ < 2) };
print "caught: $@";
PERL
     '`do BLOCK while` is NOT a loop (perl agrees, and says so)');

# ---- 6. INVERSES: nothing ordinary moved ----------------------------------
is(run_cl(<<'PERL'), "sum=3\nc=1\nlabelled=1\nn=3\n",
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
my $n = 0;
for my $i (1..3) { $n++; next; $n += 100 }
print "n=$n\n";
PERL
   'inverse: lexical exits, a LABELLED exit from a sub, a plain `next`');

is(run_cl(<<'PERL'), "1 after1 2 after2 3 after3\n",
my @s;
for my $i (1..3) { L: { push @s, $i; last; push @s, "X" } push @s, "after$i" }
print "@s\n";
PERL
   'task #1160: a bare `last` in a LABELLED bare block leaves THAT block (it used to exit the enclosing for loop, silently — and died at load with no loop around it)');

# ---- 7. the Kind-A gate, and the byte-identity it protects -----------------
like(run_cl(<<'PERL', '-dyn-loop-exit'), qr/PCL: unsupported: "last" exiting subroutine do_last/,
sub do_last { last }
for my $i (1..3) { do_last() }
PERL
     'PCL_OPT=-dyn-loop-exit: no frame is emitted, so the site keeps half (a)\'s die');

{
    # ONE source file, transpiled twice: the emitted preamble embeds the input
    # path, so two tempfiles could never compare byte-identical.
    my ($fh, $pl) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh 'my $s = 0; for my $i (1..3) { $s += $i } print "$s\n";';
    close $fh;
    my $a = PCLCore::transpile("$pl2cl $pl");
    my $b = PCLCore::transpile("PCL_OPT=-dyn-loop-exit $pl2cl $pl");
    ok($a eq $b, 'a call-free loop is emitted BYTE-IDENTICALLY with the gate on or off');
    like(transpile('sub f { 1 } my $s = 0; for my $i (1..3) { $s += f() } print "$s\n";'),
         qr/:dyn\s+t/, '... and a loop that calls user code carries :dyn t');
}
