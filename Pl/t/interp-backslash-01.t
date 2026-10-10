#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# interp-backslash-01.t -- task #3142: pl2cl SPUN FOREVER on `"ors=$\\"`.
# The interpolation loop (StringInterpolation::parse_interpolated_string)
# matches literal text, a sigil, a case escape or the END; after `$\` took one
# backslash a LONE `\` was left at the end of the string, which the literal
# alternative could not consume (it wanted a character after the backslash),
# the END could not match, and the loop -- with no `else` -- never advanced.
# perl reads a lone trailing backslash as itself.  The loop now DIES on a miss
# (rule 12), and the literal alternative takes `\` + end.
#
# The TIMED row's bound is RELATIVE to perl's own time on the same program
# (8 x perl + 0.5 s, DECIDED ## s513); the transpile runs under `timeout` so a
# regression is a failed row, never a hung gate.

use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use Time::HiRes qw(time);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $root    = "$RealBin/../..";
my $pl2cl   = "$root/pl2cl";
my @sbcl_rt = PCLCore::sbcl_prefix("$root/cl/pcl-runtime.lisp");

my $prog = <<'PROG';
local $\ = "Z";
my $s = "ors=$\\"; print STDOUT $s, "|\n";
my @a = ("$\x", "${\ 1}", "$\\n", "@\\", "a\\", "q\\$\\", "\\$\\");
print STDOUT join("|", @a), "\n";
PROG

my ($pfh, $pl) = tempfile(SUFFIX => '.pl', UNLINK => 1);
print $pfh $prog;
close $pfh;

my $p0   = time;
my $perl = `perl $pl 2>&1`;
my $pdt  = time - $p0;

my $bound = 8 * $pdt + 0.5;
my $t0 = time;
my $cl = `timeout 60 $pl2cl $pl 2>/dev/null`;
my $rc = $? >> 8;
my $dt = time - $t0;
isnt($rc, 124, '#3142 the transpile terminates (no spin on a lone trailing backslash)');
my ($cfh, $clf) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
print $cfh $cl;
close $cfh;
my $t1  = time;
my $pcl = `sbcl @sbcl_rt --load $clf 2>&1`;
$dt += time - $t1;
$pcl =~ s/^(?:;|PCL Runtime loaded).*\n//gm;

my @pl  = split /\n/, $perl, -1;
my @pc  = split /\n/, $pcl, -1;
is($pc[0], $pl[0], "#3142 \"ors=\$\\\\\" is \$\\ then a backslash (perl: $pl[0])");
is($pc[1], $pl[1], "#3142 the \$\\ family of interpolations (perl: $pl[1])");
is($pcl, $perl, '#3142 the whole program, byte for byte');
cmp_ok($dt, '<', $bound,
       sprintf('#3142 timed: transpile + run within 8x perl + 0.5 s = %.2f s (took %.2f s, perl %.2f s)',
               $bound, $dt, $pdt));
done_testing();
