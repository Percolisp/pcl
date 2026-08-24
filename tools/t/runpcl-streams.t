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

plan tests => 8;

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
