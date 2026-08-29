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
# EVERY #542 ROW HERE RUNS THROUGH A PIPE, which is the point: a pipe is where
# perl block-buffers, so a row that agrees with the perl oracle here is a row
# that would have disagreed before the fix.
#
# Row 10 (task #710) is the ONE exception and needs to be: the OTHER half of
# the same policy is what happens on a TERMINAL, and only a pty can show it.
# It runs under `script`, and skips when script(1) is absent.
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

plan tests => 13;

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
# The scratch file is File::Temp's DEFAULT `tempfile()` again (restored s452aa):
# #711 (the OPEN => 0 spelling dying on a template that does end in ten X) was a
# compiler bug and closed first; the default spelling needs a HANDLE, i.e.
# `sysopen`, which task #730 implemented.  Until then this block built its path
# from $$, and the comment said so — a guard row must fail for its own reason or
# not at all.  Now the row is BOTH: the dup-buffer assertion, and the one gate
# row that runs `tempfile()` end to end.
{
    my $prog = <<'PL';
use File::Temp qw(tempfile);
my ($fh, $f) = tempfile();
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

# ── 7. task #710: a plain `open` onto a TERMINAL is LINE buffered ──────────
# #542's policy is one function, %p-output-buffering, and it reached the
# standard handles, the dups and the fork-pipe ends but NOT `open FH,'>',path`:
# CL `open` takes no :buffering argument and always builds a :FULL fd-stream.
# So a handle onto /dev/tty block-buffered and its whole buffer arrived at
# `close`, out of order with STDOUT's line-buffered writes.
#
# This row needs a controlling terminal, which the gate does not have — `script`
# provides one.  It is the ONE row here that is not a pipe, and that is the
# point: it is the only place the tty half of the policy is observable.
SKIP: {
    my $script = `which script 2>/dev/null`;
    chomp $script;
    skip "no script(1) — the tty leg needs a controlling terminal", 1
        unless $script && -x $script;
    my $prog = <<'PL';
open(my $t, ">", "/dev/tty") or die "tty: $!";
print $t "a\n";
print STDOUT "S\n";
print $t "b\n";
close $t;
PL
    my $pl = write_pl($prog);
    my $cl = cl_file($prog);
    # `pcl` is not used: the runner must EXEC so the program's STDOUT really is
    # the pty.  sbcl is exec'd by the shell `script` starts, as perl is.
    my $want = scalar `$script -qec 'perl $pl' /dev/null 2>/dev/null`;
    my $got  = scalar `$script -qec 'sbcl @sbcl_rt --load $cl' /dev/null 2>/dev/null`;
    $_ = strip_noise($_ // '') for $want, $got;
    # A pty turns \n into \r\n; compare the LINE ORDER, which is the claim.
    s/\r//g for $want, $got;
    my @wl = grep { /^[abS]$/ } split /\n/, $want;
    my @gl = grep { /^[abS]$/ } split /\n/, $got;
    is(join("|", @gl), join("|", @wl),
       "#710: a handle onto a TERMINAL is line-buffered (perl oracle: "
       . join("|", @wl) . ")");
}

# ── 8. …and the FILE case is untouched ────────────────────────────────────
# The fix rebuilds the stream ONLY when the descriptor is a tty, so a plain
# open onto a file must stay :full on both sides — its buffer arrives at close,
# after STDOUT's line-buffered write on a tty and before it through a pipe.
# Same shape as row 7 with a file instead of the terminal; through a PIPE, so
# it also pins that this row's answer did not move with #710.
{
    my $prog = <<'PL';
my $f = "/tmp/pcl-stdio-buffering-01-file-$$.tmp";
unlink $f;
open(my $o, ">", $f) or die "open: $!";
print $o "A\n";
print STDOUT "S\n";
print $o "B\n";
close $o;
open(my $r, "<", $f) or die "reopen: $!";
my @l = <$r>; close $r; unlink $f;
chomp @l;
print "file:[", join("|", @l), "]\n";
PL
    is(cl_merged($prog), pl_merged($prog),
       '#710 inverse: a plain open onto a FILE is unchanged (perl oracle)');
}

# ── 9. `exit` INSIDE an END block (task #738) ──────────────────────────────
# perl ends that BLOCK only: every remaining END still runs, sees the status as
# $?, and the process finally exits with it.  PCL ran the ENDs from an sb-ext
# exit hook, so the nested sb-ext:exit went straight to the OS and the REST OF
# THE HOOK never ran — the remaining END blocks AND the flush that follows
# them.  Invisible while STDOUT was line-buffered; with #542 it took the whole
# buffer with it (t/op/rt119311.t lost two TAP rows that way).
# Through a PIPE on purpose: that is where the buffer is what carries the text.
{
    my $prog = <<'PL';
print "main\n";
END { print "END-A code=$?\n" }
END { print "END-B exiting\n"; exit 7 }
END { print "END-C\n" }
PL
    is(cl_merged($prog), pl_merged($prog),
       '#738: exit inside an END block ends the BLOCK — the rest still run and flush');
}

# ── 10. …and a top-level exit is still a real exit (the inverse guard) ─────
# The interception is scoped to the END phase; anywhere else `exit` must stop
# the program where it stands, ENDs and flush included.
{
    my $prog = <<'PL';
print "before\n";
END { print "END-ran\n" }
exit 0;
print "after\n";
PL
    is(cl_merged($prog), pl_merged($prog),
       '#738 inverse: a top-level exit still stops the program (perl oracle)');
}
