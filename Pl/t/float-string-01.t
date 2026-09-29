#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# float-string-01.t — FLOAT -> TEXT by exact integer arithmetic (task #2510, s500p).
#
# #2510 replaced SBCL's shortest-digits formatter (~F / ~E and the rational
# arithmetic behind %p-decimal-exponent) with PCL's own digit generator:
# %p-scaled-round (round-half-to-even of M * 2^E * 10^K in integers),
# %p-float-digits (PREC significant digits + the ROUNDED decimal exponent) and
# %p-float-g15-string (perl's %.15g text), plus sprintf %f through
# %p-scaled-round.  This file pins every CLASS of double that code has a
# branch or a boundary for.  EVERY expected line is perl 5.40.3's output of
# the program below (run with `perl`), byte for byte; each class also prints
# its negation and its sprintf "%g".
#
# The whole table is ONE program — one SBCL launch — and each class is one row.
# Inverse-verified on 209e7533: the ties15 row FAILS there (SBCL rounded a
# 15th-digit tie half-AWAY: 26.72662353515625 -> 26.7266235351563, perl
# 26.7266235351562); the other rows pass on both and are semantic PINS of the
# new code (carries, powers of ten, the style switch, denormals, the largest
# double, integral doubles, sprintf ties).
#
# Spelling note: subtraction is written `1e15 - 2` with spaces — `1e15-2` hits
# the PPI number-sign drop (#2530).  `1e15 + 1` is NOT here: perl prints an IV
# for an integral arithmetic result (#1369), a different question.

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

my $program = <<'PL';
my %c = (
  ties15   => [636316204659036.5, 87229.89111328125, 3682327.439453125, 985351820871.8125, 0.1640777587890625, 813.9488525390625, 1.016265869140625, 26.72662353515625],
  carry    => [999999999999999.9, 999999999999999.0, 9.999999999999999e-5, 9.99999999999999e-5, 99999999999999.99, 9.9999999999999995, 0.99999999999999995, 1.0000000000000002],
  pow10    => [1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e5, 1e10, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19, 1e20, 1e21, 1e22],
  switch   => [0.00001, 0.0001, 0.000099999, 0.0000123456789012345, 1e14 + 0.5, 1e15 - 1, 1000000000000001.5, 1e15 + 0.5, 123456789012345.6, 1234567890123456, 5e14 + 0.25],
  denormal => [5e-324, 1e-320, 2.2250738585072014e-308, 2.225073858507201e-308, 4.9406564584124654e-324 * 3],
  largest  => [1.7976931348623157e308, 1.7976931348623155e308, 8.98846567431158e307],
  integral => [2**52, 2**53, 2**53 + 2, 2**52 + 0.5, 999999999999999, 1e15 - 2, 4503599627370497, 123456789012.0],
);
for my $k (qw(ties15 carry pow10 switch denormal largest integral)) {
  print "$k: ", join(" ", @{$c{$k}}), "\n";
  print "$k-neg: ", join(" ", map { -$_ } @{$c{$k}}), "\n";
  print "$k-g: ", join(" ", map { sprintf "%g", $_ } @{$c{$k}}), "\n";
}
my @t = (0.5, 1.5, 2.5, -2.5, 3.5, 0.125, 0.375, 1.005, 2.675, 2**-16, 3 * 2**-16, 1/3, 0.1);
print "f0: ",  join(" ", map { sprintf "%.0f", $_ } @t), "\n";
print "f2: ",  join(" ", map { sprintf "%.2f", $_ } @t), "\n";
print "f15: ", join(" ", map { sprintf "%.15f", $_ } @t), "\n";
print "f15-neg: ", join(" ", map { sprintf "%.15f", -$_ } @t), "\n";
PL

# Run once; group the output lines by class (the text before `:`, less a
# -neg / -g suffix; f0 / f2 / f15 lines are the sprintf-ties class).
my $out = run_cl($program);
my %got;
for my $line (split /^/, $out) {
    my ($k) = $line =~ /^(\w+?)(?:-neg|-g)?:/;
    $k = 'UNPARSED' if !defined $k;
    $k = 'sprintf-ties' if $k =~ /^f(?:0|2|15)$/;
    $got{$k} .= $line;
}

for my $row (
    ['ties15', 'a 15th-significant-digit TIE rounds half-EVEN (perl/glibc), not half-away', <<'OUT'],
ties15: 636316204659036 87229.8911132812 3682327.43945312 985351820871.812 0.164077758789062 813.948852539062 1.01626586914062 26.7266235351562
ties15-neg: -636316204659036 -87229.8911132812 -3682327.43945312 -985351820871.812 -0.164077758789062 -813.948852539062 -1.01626586914062 -26.7266235351562
ties15-g: 6.36316e+14 87229.9 3.68233e+06 9.85352e+11 0.164078 813.949 1.01627 26.7266
OUT
    ['carry', 'rounding CARRIES across a power of ten (999999999999999.9 -> 1e+15, 9.999999999999999e-5 -> 0.0001)', <<'OUT'],
carry: 1e+15 999999999999999 0.0001 9.99999999999999e-05 100000000000000 10 1 1
carry-neg: -1e+15 -999999999999999 -0.0001 -9.99999999999999e-05 -100000000000000 -10 -1 -1
carry-g: 1e+15 1e+15 0.0001 0.0001 1e+14 10 1 1
OUT
    ['pow10', 'exact powers of ten 1e-8 .. 1e22 (the log10 first guess is off by one at some)', <<'OUT'],
pow10: 1e-08 1e-07 1e-06 1e-05 0.0001 0.001 0.01 0.1 1 10 100 100000 10000000000 100000000000000 1e+15 1e+16 1e+17 1e+18 1e+19 1e+20 1e+21 1e+22
pow10-neg: -1e-08 -1e-07 -1e-06 -1e-05 -0.0001 -0.001 -0.01 -0.1 -1 -10 -100 -100000 -10000000000 -100000000000000 -1e+15 -1e+16 -1e+17 -1e+18 -1e+19 -1e+20 -1e+21 -1e+22
pow10-g: 1e-08 1e-07 1e-06 1e-05 0.0001 0.001 0.01 0.1 1 10 100 100000 1e+10 1e+14 1e+15 1e+16 1e+17 1e+18 1e+19 1e+20 1e+21 1e+22
OUT
    ['switch', 'the %.15g / %g fixed-vs-exponent switch at 1e-5 / 1e-4 and 1e14 / 1e15', <<'OUT'],
switch: 1e-05 0.0001 9.9999e-05 1.23456789012345e-05 100000000000000 999999999999999 1e+15 1e+15 123456789012346 1234567890123456 500000000000000
switch-neg: -1e-05 -0.0001 -9.9999e-05 -1.23456789012345e-05 -100000000000000 -999999999999999 -1e+15 -1e+15 -123456789012346 -1234567890123456 -500000000000000
switch-g: 1e-05 0.0001 9.9999e-05 1.23457e-05 1e+14 1e+15 1e+15 1e+15 1.23457e+14 1.23457e+15 5e+14
OUT
    ['denormal', 'denormals (5e-324, 1e-320) and the smallest normal 2.2250738585072014e-308', <<'OUT'],
denormal: 4.94065645841247e-324 9.99988867182683e-321 2.2250738585072e-308 2.2250738585072e-308 1.48219693752374e-323
denormal-neg: -4.94065645841247e-324 -9.99988867182683e-321 -2.2250738585072e-308 -2.2250738585072e-308 -1.48219693752374e-323
denormal-g: 4.94066e-324 9.99989e-321 2.22507e-308 2.22507e-308 1.4822e-323
OUT
    ['largest', 'the largest doubles (1.7976931348623157e308)', <<'OUT'],
largest: 1.79769313486232e+308 1.79769313486232e+308 8.98846567431158e+307
largest-neg: -1.79769313486232e+308 -1.79769313486232e+308 -8.98846567431158e+307
largest-g: 1.79769e+308 1.79769e+308 8.98847e+307
OUT
    ['integral', 'integral doubles below and above 1e15 and at 2**52 / 2**53', <<'OUT'],
integral: 4.5035996273705e+15 9.00719925474099e+15 9.00719925474099e+15 4.5035996273705e+15 999999999999999 999999999999998 4503599627370497 123456789012
integral-neg: -4.5035996273705e+15 -9.00719925474099e+15 -9.00719925474099e+15 -4.5035996273705e+15 -999999999999999 -999999999999998 -4503599627370497 -123456789012
integral-g: 4.5036e+15 9.0072e+15 9.0072e+15 4.5036e+15 1e+15 1e+15 4.5036e+15 1.23457e+11
OUT
    ['sprintf-ties', 'sprintf %.0f / %.2f / %.15f round the EXACT value half-even (2**-16 at %.15f is a true tie)', <<'OUT'],
f0: 0 2 2 -2 4 0 0 1 3 0 0 0 0
f2: 0.50 1.50 2.50 -2.50 3.50 0.12 0.38 1.00 2.67 0.00 0.00 0.33 0.10
f15: 0.500000000000000 1.500000000000000 2.500000000000000 -2.500000000000000 3.500000000000000 0.125000000000000 0.375000000000000 1.005000000000000 2.675000000000000 0.000015258789062 0.000045776367188 0.333333333333333 0.100000000000000
f15-neg: -0.500000000000000 -1.500000000000000 -2.500000000000000 2.500000000000000 -3.500000000000000 -0.125000000000000 -0.375000000000000 -1.005000000000000 -2.675000000000000 -0.000015258789062 -0.000045776367188 -0.333333333333333 -0.100000000000000
OUT
) {
    my ($class, $desc, $want) = @$row;
    is($got{$class} // "(no output; whole run:\n$out)", $want, "2510 $class: $desc");
}
