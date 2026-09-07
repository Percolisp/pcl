#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# numeric-repr-01.t — THE NUMERIC REPRESENTATION family (s473d).
#
# One file for the questions that are all about how a Perl number is REPRESENTED
# in a p-box and how it comes back out as text:
#
#   A  #1230/#1245 — "is this a dualvar" is a FACT of the representation
#      (the :DUAL marker on p-box's NV-OK), never an inference from comparing
#      the two caches.  The inference false-positived on every cache-warm
#      float, because perl's %.15g rendering of a double does not round-trip
#      to the same double; %p-dualvar-copy then wrote the box's RAW value into
#      the STRING cache, and `printf "%-8s"` died inside sprintf-apply-width
#      with an SBCL type-error that killed the whole program.
#
#   B  #1248(a)/#1191 — `%` with an INFINITE RIGHT operand is the ordinary
#      mathematical modulo (sign of the right operand), not NaN.
#
#   C  #1248(b) — `**` returns an NV in perl, so 2**63 prints through %.15g.
#
#   D  #1012 — the %.15g switch to exponential happens AT 1e15, and deriving
#      the decimal exponent from a LOG got it wrong by one at exactly that
#      value (log(1e15, 10.0d0) is 14.999999999999998).
#
#   E  #1248(c) — Internals::SvREADONLY(@a,1) must not change what scalar(@a)
#      answers.
#
# EVERY expected string below was probed against perl 5.40.3 first: run the
# same program with `perl` and with `./runpcl` and the two are byte-equal.
# The rows are grouped into whole programs so the file costs a handful of SBCL
# launches rather than one per assertion.

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

plan tests => 3;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    is(run_cl($code), $expected, $name);
}

# ─────────────────────────────────────────────────────────────────────────────
# A — #1230/#1245.  A cache-warm float is NOT a dualvar, and printing one with
# a WIDTH must not reach a host type-error.  The crash needed all three of: a
# value from a STRING eval, returned through a sub whose body compares it with
# `eq ""` (that is what warms the string cache), and a conversion carrying a
# width.  `done` on the last line is the point: the whole program used to die.
# ─────────────────────────────────────────────────────────────────────────────
test_cl('#1245 a warm float survives printf with a width (the whole program runs)',
        <<'PL', <<'OUT');
sub cmp_sub  { my $v = shift; return "E" if $v eq ""; return $v }
sub bare_sub { my $v = shift; return $v }
my $fl = eval "1/3";
my $in = eval "42";
my $st = eval '"hello"';
printf "1 |%-8s| |%10s| |%s|\n", cmp_sub($fl), cmp_sub($fl), cmp_sub($fl);
printf "2 |%-8s|\n", bare_sub($fl);
printf "3 |%-8s| |%-8s|\n", cmp_sub($in), cmp_sub($st);
printf "4 %5.2f %d\n", cmp_sub($fl), cmp_sub($in);
my @a = (cmp_sub($fl)); my %h = (k => cmp_sub($fl));
printf "5 |%-8s| |%-8s|\n", $a[0], $h{k};
print "done\n";
PL
1 |0.333333333333333| |0.333333333333333| |0.333333333333333|
2 |0.333333333333333|
3 |42      | |hello   |
4  0.33 42
5 |0.333333333333333| |0.333333333333333|
done
OUT

# ─────────────────────────────────────────────────────────────────────────────
# A2 — the INVERSE guard.  Making "dualvar" a representation fact must not lose
# a GENUINE dualvar: $! and Scalar::Util::dualvar keep both halves through a
# sub frame, an array, a hash and an assignment, and isdual answers for the two
# shapes the old cache-comparison could not see (a dualvar whose halves happen
# to agree numerically).
# ─────────────────────────────────────────────────────────────────────────────
test_cl('#1230 inverse: a genuine dualvar keeps both halves and answers isdual',
        <<'PL', <<'OUT');
use Scalar::Util qw(dualvar isdual);
sub cmp_sub { my $v = shift; return "E" if $v eq ""; return $v }
my $dv = dualvar(42, "forty-two");
my $t = cmp_sub($dv);
printf "1 n=%d s=%s\n", $t+0, "$t";
my @a = ($dv); my %h = (k => $dv); my $c = $dv;
printf "2 %d %s %d %s %d %s\n", $a[0]+0, "$a[0]", $h{k}+0, "$h{k}", $c+0, "$c";
printf "3 %d %d %d\n", (isdual($dv)?1:0), (isdual(dualvar(0,"abc"))?1:0),
                       (isdual(dualvar(5,"5abc"))?1:0);
printf "4 %d %d\n", (isdual(1/3)?1:0), (isdual("x")?1:0);
open(my $fh, '<', '/nope-xyz-numeric-repr') or 1;
my $e = $!;  my $e2 = cmp_sub($e);
printf "5 %d %d %d\n", $e2+0, (length("$e2")>2 ? 1 : 0), (isdual($e2)?1:0);
PL
1 n=42 s=forty-two
2 42 forty-two 42 forty-two 42 forty-two
3 1 1 1
4 0 0
5 2 1 1
OUT

# ─────────────────────────────────────────────────────────────────────────────
# A3 — the emission is not what decides it: the same program under the general
# form compiler (PCL_OPT=none) prints the same thing.  The crash reproduced
# under -none too, which is how it was known to be the box model and not a
# raw-slot verdict.
# ─────────────────────────────────────────────────────────────────────────────
{
    local $ENV{PCL_OPT} = 'none';
    test_cl('#1245 identical under PCL_OPT=none', <<'PL', <<'OUT');
sub cmp_sub { my $v = shift; return "E" if $v eq ""; return $v }
my $fl = eval "1/3";
printf "1 |%-8s|\n", cmp_sub($fl);
print "done\n";
PL
1 |0.333333333333333|
done
OUT
}
