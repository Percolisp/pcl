#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# while-empty-01.t — s481b: `while ()` is perl's INFINITE LOOP.
#
# perl accepts an empty `while` condition and treats it as constant TRUE, the
# same way an empty COND section of a C-style `for` is:
#
#     my $n = 0;  while () { $n++; last if $n >= 3 }   # perl: $n == 3
#
# PCL passed the empty parts list to `Pl::Parser2::_lower_expr`, which dies
# "Parser2: empty expression" — and that is a HARD transpile error, so the
# WHOLE FILE is lost, not one statement.  Measured while sizing the
# critical-infrastructure milestone (#1607): HTTP::Tiny's `_do_timeout` writer
# loop is spelled exactly this way, so `HTTP/Tiny.pm` did not compile at all
# and the dist scored 1 of its 32 test files (perl: 28 PASS, 455 assertions).
#
# The fix is the C-style-for arm's own rule, read once more: an empty
# condition lowers to constant true.  `p-for`'s cond slot is a `['list', …]`,
# so it spells it `['list','t']`; `p-while`'s cond is a bare form, so it is
# plain `t` here.
#
# The INVERSE rows matter as much: a `while (0)` must still not run, an
# ordinary condition must still be evaluated every iteration, and `for (;;)`
# — the sibling this rule is borrowed from — must be untouched.  Every
# expectation below is perl 5.40.3's, probed.

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

# Transpile (PCLCore::transpile FAILS the row on a dropped statement).
sub transpile {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return PCLCore::transpile("$pl2cl $pl_file");
}

sub run_cl {
    my ($code) = @_;
    my $cl_code = transpile($code);
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# ---------------------------------------------------------------- the rule

is(run_cl(<<'PERL'), "n=3\n",
my $n = 0;
while () { $n++; last if $n >= 3 }
print "n=$n\n";
PERL
   '`while ()` loops until `last` — perl runs the body 3 times');

is(run_cl(<<'PERL'), "j=2\n",
my $j = 0;
while ( ) { $j++; last if $j >= 2 }
print "j=$j\n";
PERL
   'the spaced spelling `while ( )` is the same statement');

is(run_cl(<<'PERL'), "k=4\n",
my $k = 0;
OUTER: while () {
  while () { $k++; last OUTER if $k >= 4 }
}
print "k=$k\n";
PERL
   'a LABELLED `last` still leaves the loop it names, from inside a nested one');

is(run_cl(<<'PERL'), "sum=6 iters=3\n",
sub drain {
  my ($limit) = @_;
  my ($sum, $iters) = (0, 0);
  while () {
    $iters++;
    $sum += $iters;
    last if $iters >= $limit;
  }
  return ($sum, $iters);
}
my ($s, $i) = drain(3);
print "sum=$s iters=$i\n";
PERL
   'the HTTP::Tiny shape: an empty-condition writer loop inside a sub');

is(run_cl(<<'PERL'), "c=5\n",
my $c = 0;
while () { $c++; next if $c < 5; last }
print "c=$c\n";
PERL
   '`next` inside an empty-condition loop re-enters it, it does not exit');

# --------------------------------------------------------------- the shape

like(transpile("my \$n=0; while () { \$n++; last }\n"),
     qr/\(p-while\s+t\b/,
     'the emitted condition is the constant `t`, not a call or a nil');

# ------------------------------------------------------------- the inverses

is(run_cl(<<'PERL'), "never=0\n",
my $n = 0;
while (0) { $n++ }
print "never=$n\n";
PERL
   'inverse: `while (0)` still never runs — an empty condition is not a false one');

is(run_cl(<<'PERL'), "i=3\n",
my $i = 0;
while ($i < 3) { $i++ }
print "i=$i\n";
PERL
   'inverse: an ordinary condition is still evaluated every iteration');

is(run_cl(<<'PERL'), "f=3\n",
my $f = 0;
for (;;) { $f++; last if $f >= 3 }
print "f=$f\n";
PERL
   'inverse: `for (;;)` — the sibling this rule is read from — is untouched');
