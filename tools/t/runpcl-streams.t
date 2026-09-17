#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Task #504 — `runpcl` keeps STDOUT and STDERR apart, so a byte-compare against
# perl through this runner measures the program and not the runner.
#
# The bug: `runpcl` used to capture the child with `2>&1` and then strip
# `^;` lines from the merged text.  SBCL writes a compilation NOTE as a BLOCK —
# one EMPTY line, then a run of `;`-prefixed lines — so the strip removed the
# `;` lines and left the empty one wedged into the middle of the program's
# stdout:
#
#     print "1\n"; eval { my $x = 1/0 }; print "2\n";
#
# printed "1\n\n2\n" through runpcl and "1\n2\n" when the emitted CL was run
# directly.  Every byte-compare vs perl over a program whose forms provoke a
# note was silently wrong — the same class as the s386 fix (which had stopped
# a blanket blank-line strip from eating the PROGRAM's blank lines).
#
# The rows below are the two halves of the contract:
#   * stdout is the program's stdout, byte for byte — blank lines and lines
#     beginning with `;` included (SBCL puts NOTHING on stdout; the banner,
#     the notes, the style warnings, a backtrace and the runtime's own
#     "PCL Runtime loaded" all go to *error-output*);
#   * stderr is the program's stderr with SBCL's note BLOCKS removed whole —
#     the opening empty line goes with them, and a `;`-line the PROGRAM wrote
#     survives, because perl prints it.
#
# NOT part of the Pl/t gate: that gate measures the transpiler, this measures a
# runner.  Run it directly:  prove tools/t/runpcl-streams.t
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

my $root   = "$RealBin/../..";
my $runpcl = "$root/runpcl";

plan skip_all => "runpcl not found" unless -x $runpcl;
plan skip_all => "sbcl not found"   unless `which sbcl 2>/dev/null`;

plan tests => 15;

sub write_pl {
    my ($code) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $file;
}

# Run a program under perl and under runpcl, capturing the two streams apart.
sub streams {
    my ($cmd, $file) = @_;
    my (undef, $errfile) = tempfile(SUFFIX => '.err', UNLINK => 1);
    my $out = `$cmd "$file" 2>$errfile`;
    my $err = do { open(my $f, '<', $errfile) or return ($out, ''); local $/; <$f> // '' };
    return ($out, $err);
}

sub agree {
    my ($code, $desc) = @_;
    my $file = write_pl($code);
    my ($pout, $perr) = streams("perl", $file);
    my ($cout, $cerr) = streams($runpcl, $file);
    is($cout, $pout, "$desc — stdout");
    is($cerr, $perr, "$desc — stderr");
}

# ---- the repro: a form SBCL emits a compilation note for -------------------
#
# `1/0` constant-folds and SBCL reports the folding error as a style warning,
# which is what produced the block whose blank line leaked into stdout.

agree(<<'PL', 'a form that provokes an SBCL compilation note');
print "1\n";
eval { my $x = 1/0; };
print "2\n";
PL

# ---- the program's own blank lines and `;` lines, on BOTH streams ----------

agree(<<'PL', "the program's blank and `;`-prefixed lines survive");
print "hello\n";
print STDERR "err-line\n";
print "\n";
print "after-blank\n";
print STDERR "\n";
print STDERR "; semicolon-on-stderr\n";
print "; semicolon-on-stdout\n";
PL

# ---- a program that writes nothing at all ---------------------------------

agree('my $x = 1;', 'a silent program is silent on both streams');

# ---- interleaved writes: the merge must not invent or lose a separator ----

agree(<<'PL', 'many small writes to both streams');
for my $i (1 .. 5) {
    print "out$i\n";
    print STDERR "err$i\n";
}
PL

# ---- $| = 1 reaches the fd WHILE the program runs (task #1850) -------------
#
# The instrument, not just the feature: perl's t/test.pl sets `$| = 1` at its
# line 22 so a test file's TAP is on disk row by row, and every companion file
# runs under PCL's transpilable stub perl-tests/t/test.pl.  While that stub
# lacked the line (and while %tap-out wrote past the autoflush table), a file
# killed by the runner's timeout kept only the rows that happened to have
# flushed — so its C_ok/C_notok were the rows that reached the fd, not the rows
# that ran, and "the last row is N" bounded a hang from BELOW and up to one
# buffer short.  ./runpcl hid the same fact for any program, because it
# captured the child's stdout with backticks and printed it at exit.
#
# A plain run cannot see this: the buffer flushes at exit either way.  The
# discriminator is a RACE the program itself decides — it writes its first
# line, busy-loops, then touches a MARKER file, then writes its last line.  We
# poll both paths: stdout first means the write reached the fd mid-run, marker
# first means it sat in a buffer until exit.  No sleep() in the harness times
# anything, so a slow or a fast box gives the same verdict.

sub flush_race {
    my ($runner, $code, $want, $desc) = @_;
    my (undef, $out)    = tempfile(SUFFIX => '.out',    UNLINK => 1);
    my (undef, $marker) = tempfile(SUFFIX => '.marker', UNLINK => 1);
    unlink $out, $marker;
    my $file = write_pl($code =~ s/__MARKER__/$marker/gr);
    my $pid = fork();
    defined $pid or die "fork: $!";
    if (!$pid) {
        open(STDOUT, '>', $out)     or die;
        open(STDERR, '>', '/dev/null') or die;
        exec($runner, $file);
        die "exec $runner: $!";
    }
    my ($saw, $deadline) = ('timeout', time + 120);
    while (time < $deadline) {
        if (-s $out)   { $saw = 'stdout'; last }
        if (-e $marker) { $saw = 'marker'; last }
        select(undef, undef, undef, 0.05);
    }
    waitpid($pid, 0);
    unlink $out, $marker;
    is($saw, $want, $desc);
}

# TWO busy windows, not one: the marker must be written far enough from the
# program's EXIT that a poll can tell them apart.  With one window the marker
# write, the last print and the exit flush all landed inside one 50 ms tick,
# so the buffered case read 'stdout' too and the row could not fail.
my $BODY = <<'PL';
print "first\n";
my $t = time; 1 while time - $t < 5;
open(my $m, '>', '__MARKER__') or die; print $m "x"; close $m;
my $u = time; 1 while time - $u < 5;
print "last\n";
PL

flush_race($runpcl, "\$| = 1;\n$BODY", 'stdout',
           'runpcl: $| = 1 puts the first line on the fd before the program ends');
flush_race($runpcl, $BODY, 'marker',
           'runpcl: without $| the line waits for exit (the flag is honoured, not ignored)');

my $pclperl = "$root/tools/pclperl-for-tests";
SKIP: {
    skip 'pclperl-for-tests not executable', 2 unless -x $pclperl;
    flush_race($pclperl, "\$| = 1;\n$BODY", 'stdout',
               'pclperl-for-tests: $| = 1 reaches the fd mid-run');
    flush_race($pclperl, $BODY, 'marker',
               'pclperl-for-tests: without $| the line waits for exit');
}

# The TAP writer is the harness's own `print`, so it obeys the same flag: this
# is the half the companion's kept .out depends on (cl/pcl-test.lisp %tap-out).
my $TAP = <<'PL';
use Test::More tests => 2;
ok(1, 'first');
my $t = time; 1 while time - $t < 5;
open(my $m, '>', '__MARKER__') or die; print $m "x"; close $m;
my $u = time; 1 while time - $u < 5;
ok(1, 'last');
PL
flush_race($runpcl, "\$| = 1;\n$TAP", 'stdout',
           'TAP: $| = 1 puts a row on the fd as it is emitted');
flush_race($runpcl, $TAP, 'marker',
           'TAP: without $| the rows wait for exit');

# And the stub carries the line, where perl's harness carries it.
my $stub = do {
    open(my $f, '<', "$root/perl-tests/t/test.pl") or die "stub: $!";
    local $/; <$f>;
};
like($stub, qr/^\$\| = 1;$/m,
     'perl-tests/t/test.pl sets $| = 1, as perl t/test.pl does at its line 22');
