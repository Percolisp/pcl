#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# stdio-buffering-01.t — task #542, perl's stdio buffering POLICY for the
# standard handles.
#
# perl block-buffers a non-tty STDOUT and leaves STDERR unbuffered; PCL
# line-buffered both, which is invisible until a second handle reaches the same
# descriptor.  Then `print $dup "a"; print "b"` comes out in the OTHER ORDER,
# because the dup's own buffer flushes at its close while STDOUT's sits there
# until exit.  The policy is one function, cl/pcl-runtime.lisp's
# %p-output-buffering (+ %p-std-buffering for STDERR's exception), asked by
# boot, by every standard-handle rebuild and by every dup.
#
# EVERY ROW HERE RUNS THROUGH A PIPE, which is the point: a pipe is where perl
# block-buffers, so a row that agrees with the perl oracle here is a row that
# would have disagreed before the fix.  (The pty leg of the measurement — where
# perl line-buffers and the same programs come out in program order — is not a
# gate row: it needs a controlling terminal.  It is in the session record.)
#
# The die row is the OTHER half of the change and the one with the sweep-sized
# consequence: block buffering turns a mid-file abort into invisible row loss
# unless the exit path flushes, so `print; die` must still show the print.  It
# does because SBCL's disabled-debugger quit runs sb-ext:*exit-hooks* (measured
# 2.6.0) and PCL's flush lives there.  If a future SBCL makes that an abort,
# THIS ROW is what says so.

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

plan tests => 9;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    binmode($fh, ':raw');
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub cl_file {
    my ($code) = @_;
    my $cl = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($fh, $file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    binmode($fh, ':raw');
    print $fh $cl;
    close $fh;
    return $file;
}

# SBCL's own chatter all goes to stderr; strip exactly that and nothing else,
# so the ORDER of the program's own lines is compared byte for byte.
sub strip_noise {
    my ($o) = @_;
    $o =~ s/^;.*\n//gm;
    $o =~ s/^PCL (?:Runtime|Test)[^\n]*\n//gm;
    return $o;
}

# `scalar` is load-bearing: backticks in LIST context hand strip_noise a list of
# LINES, and its `my ($o) = @_` would keep only the first one — which reads
# exactly like a lost buffer, the very bug these rows are about.
sub cl_merged  { my $f = cl_file($_[0]); return strip_noise(scalar `sbcl @sbcl_rt --load $f 2>&1`) }
sub cl_stdout  { my $f = cl_file($_[0]); return scalar `sbcl @sbcl_rt --load $f 2>/dev/null` }
sub pl_merged  { my $f = write_pl($_[0]); return scalar `perl $f 2>&1` }
sub pl_stdout  { my $f = write_pl($_[0]); return scalar `perl $f 2>/dev/null` }

# ── 1. The task's two reproducers ───────────────────────────────────────────
# perl: via-dup then via-stdout — the dup's buffer goes at `close $dup`, the
# block-buffered STDOUT's at exit.  PCL used to print them the other way round.
{
    my $prog = <<'PL';
open(my $dup, ">&", \*STDOUT) or die;
print $dup "via-dup\n";
print "via-stdout\n";
close $dup;
PL
    my $got = cl_merged($prog);
    is($got, pl_merged($prog), '#542: dup then source — perl oracle');
    is($got, "via-dup\nvia-stdout\n", '#542: … and the order is the dup first');
}

# The second reproducer separates "agrees by coincidence" from "agrees": a
# per-dup line-buffering fix would answer the first one right and this one
# wrong (the task's own NOT-a-fix note).
{
    my $prog = <<'PL';
open(my $dup, ">&", \*STDOUT) or die;
print $dup "one\n"; print "two\n"; print $dup "three\n"; close $dup;
PL
    my $got = cl_merged($prog);
    is($got, pl_merged($prog), '#542: interleaved dup/source writes — perl oracle');
    is($got, "one\nthree\ntwo\n", '#542: … block-buffered STDOUT comes last');
}

# ── 2. $| still overrides the policy, per handle ────────────────────────────
{
    my $prog = <<'PL';
$| = 1;
open(my $dup, ">&", \*STDOUT) or die;
print $dup "one\n"; print "two\n"; print $dup "three\n"; close $dup;
PL
    is(cl_merged($prog), pl_merged($prog),
       '#542: $|=1 on STDOUT still flushes at every write (perl oracle)');
}

# ── 3. STDERR is unbuffered, STDOUT is not ─────────────────────────────────
# Merged into one pipe, perl shows both stderr lines before either stdout line.
{
    my $prog = <<'PL';
print STDOUT "out1\n";
print STDERR "err1\n";
print STDOUT "out2\n";
print STDERR "err2\n";
PL
    is(cl_merged($prog), pl_merged($prog),
       '#542: unbuffered STDERR overtakes block-buffered STDOUT (perl oracle)');
}

# ── 4. A dup of a FILE handle: both writes land ────────────────────────────
# The scratch path is built from $$ rather than File::Temp, and STAYS that way.
# #711 (the OPEN => 0 spelling dying on a template that does end in ten X) is
# CLOSED — it was a compiler bug, not a shim one — but the DEFAULT `tempfile()`
# still needs a handle, and `sysopen` is not implemented, so the call dies for a
# second, unrelated reason (task #730).  A guard row must fail for its own
# reason or not at all.
{
    my $prog = <<'PL';
my $f = "/tmp/pcl-stdio-buffering-01-$$.tmp";
unlink $f;
open(my $fh, ">", $f) or die "open: $!";
open(my $dup, ">&", $fh) or die "dup: $!";
print $fh "A\n";
print $dup "B\n";
close $dup; close $fh;
open(my $r, "<", $f) or die "reopen: $!";
my @l = <$r>; close $r; unlink $f;
chomp @l;
print "lines:", scalar(@l), " [", join("|", sort @l), "]\n";
PL
    is(cl_merged($prog), pl_merged($prog),
       '#542: a dup of a FILE handle keeps its own buffer, both writes land');
}

# ── 5. PERL_FLUSHALL_FOR_CHILD ─────────────────────────────────────────────
# perl flushes EVERY handle before a child runs, so a block-buffered STDOUT
# still prints before the child's output.  With the policy in place and no
# flush, `a` would come out after MARK.
{
    my $prog = <<'PL';
open(my $dup, ">&", \*STDOUT) or die;
print "a\n";
print $dup "d\n";
system("echo MARK");
print "b\n";
close $dup;
PL
    is(cl_merged($prog), pl_merged($prog),
       '#542: system() flushes every handle first (perl oracle)');
}

# ── 6. The exit path: a die must not swallow what was already printed ──────
# stderr is dropped on both sides — perl says "boom", PCL prints a CL backtrace
# (a separate, pre-existing divergence).  What is asserted is that STDOUT's
# buffer still reaches the pipe.
{
    my $prog = <<'PL';
print "row-1\n";
print "row-2\n";
die "boom\n";
PL
    is(cl_stdout($prog), pl_stdout($prog),
       '#542: output printed before an uncaught die is still flushed (perl oracle)');
}
