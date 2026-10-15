#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# perf-levers-15.t — perf round 47 (s517c, docs/speed-review-s509.md §2
# "In round 47").  MECHANISM rows (the runtime's own answer) and ANSWER rows:
# programs whose expected output below IS perl's own (probed on 5.40.3;
# nothing in them needs perl >= 5.38).
#
#   #3221 print / say / printf are on Parser2's %TOPIC_RAW_WORD, so a
#         `for (A..B)` topic loop whose body prints binds $_ to the RAW
#         counter (p-foreach-range-raw); the residue -- a TIED handle's
#         PRINT that WRITES the global $_ -- is shown, and PCL_OPT=-raw-topic
#         restores perl's answer.
use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use lib "$RealBin/../..";
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl   = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);
plan skip_all => "pl2cl not found" if !-x $pl2cl;
plan skip_all => "sbcl not found"  if !`which sbcl 2>/dev/null`;

sub src_file {
    my ($src) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $src;
    close $fh;
    return $file;
}

sub transpile { my ($src) = @_; return PCLCore::transpile(qq{$pl2cl } . src_file($src)) }

sub run_pl {
    my ($src) = @_;
    my $cl = transpile($src);
    my ($cfh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cfh $cl;
    close $cfh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^(?:caught |compilation unit|-->|==>|PCL Runtime loaded).*\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

sub answers {
    my ($src, $expected, $what) = @_;
    is(run_pl($src), $expected, $what);
}

# The runtime's own answers to a few forms (the MECHANISM rows).
sub lisp_out {
    my ($forms) = @_;
    my ($lfh, $lfile) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $lfh "(in-package :pcl)\n$forms";
    close $lfh;
    return scalar `sbcl @sbcl_rt --load $lfile 2>&1`;
}

# ─────────────────────────────────────────────────────────────────────────────
# #3221 MECHANISM: a topic loop whose body is print / say / printf binds $_
# raw; a bareword handle (a Word the allowlist does not know) keeps the box.
{
    my $cl = transpile(<<'END_PL');
use feature 'say';
my $buf = '';
open my $fh, '>', \$buf or die;
print {$fh} $_ for 1..3;
say {$fh} $_ for 4..5;
printf {$fh} "%d,", $_ for 6..8;
print STDOUT $_ for 1..2;
END_PL
    like($cl, qr/\(p-foreach-range-raw \(\$_ 1 3\) \(p-print :fh \$fh \$_\)\)/,
         '#3221 print {$fh} $_ for A..B binds $_ raw');
    like($cl, qr/\(p-foreach-range-raw \(\$_ 4 5\) \(p-say :fh \$fh \$_\)\)/,
         '#3221 say {$fh} $_ for A..B binds $_ raw');
    like($cl, qr/\(p-foreach-range-raw \(\$_ 6 8\) \(p-printf :fh \$fh/,
         '#3221 printf {$fh} FMT, $_ for A..B binds $_ raw');
    like($cl, qr/\(p-foreach-range \(\$_ 1 2\)/,
         '#3221 a bareword handle (STDOUT) keeps the boxed loop');
}

# #3221 ANSWERS (perl 5.40.3's output): plain handles, an in-memory handle,
# and an operand with a `""` overload (the handler runs, the value prints).
answers(<<'END_PL', <<'END_OUT', '#3221 print/say/printf in a raw topic loop = perl');
use feature 'say';
my $buf = '';
open my $fh, '>', \$buf or die;
print {$fh} $_ for 1..3;
print $fh "\n";
say {$fh} $_ for 4..5;
printf {$fh} "%d,", $_ for 6..8;
close $fh;
print $buf, "\n";
say $_ for 1..2;
printf "<%s>", $_ for 3..4; print "\n";
package O; use overload '""' => sub { "O(" . $_[0][0] . ")" }; sub new { bless [$_[1]] }
package main;
my @o = map { O->new($_) } 0..2;
print $o[$_], " " for 0..2; print "|\n";
END_PL
123
4
5
6,7,8,
1
2
<3><4>
O(0) O(1) O(2) |
END_OUT

# #3221 a tied handle's PRINT that READS the global $_ sees the current
# element (perl's answer).
answers(<<'END_PL', <<'END_OUT', '#3221 tied PRINT reading the global $_ = perl');
package T; sub TIEHANDLE { bless {} } sub PRINT { shift; print STDOUT "[PRINT sees \$_=$_ arg=@_]"; 1 }
package main;
tie *FH, 'T';
$_ = "out"; my $fh = \*FH;
print {$fh} $_ for 1..3;
print "\n";
END_PL
[PRINT sees $_=1 arg=1][PRINT sees $_=2 arg=2][PRINT sees $_=3 arg=3]
END_OUT

# #3221 THE RESIDUE, made visible (docs/not-supported.md "A raw-topic
# `for (A..B)` loop"): a tied PRINT that WRITES the global $_.  perl prints
# <9><9><9>; under the default PCL's raw loop prints <1><2><3>, and
# PCL_OPT=-raw-topic restores perl's answer.  If the first row ever starts
# printing perl's answer, the residue is gone: update not-supported.md.
{
    my $src = <<'END_PL';
package T; sub TIEHANDLE { bless {} } sub PRINT { shift; $_ = 9; 1 }
package main;
tie *FH, 'T';
my $fh = \*FH;
for (1..3) { print {$fh} $_; print "<$_>"; }
print "\n";
END_PL
    is(run_pl($src), "<1><2><3>\n",
       '#3221 residue: tied PRINT writing $_ is not seen by the raw loop (perl: <9><9><9>)');
    local $ENV{PCL_OPT} = '-raw-topic';
    is(run_pl($src), "<9><9><9>\n",
       '#3221 residue: PCL_OPT=-raw-topic restores perl\'s <9><9><9>');
}

done_testing();
