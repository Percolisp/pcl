#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# numify-flag-01.t -- guard for s525u (docs/runtime-review-s525.md R1 + #3760).
#
# n01-n13: a STRING that has been USED AS A NUMBER takes the NUMERIC bitwise
# op, as perl's SvNIOKp says -- the box's nv-ok is that flag (#1040).  s01-s06
# are the negatives: truthiness, eq, length, interpolation, sprintf "%s", a
# fresh store, a numified COPY and an append leave the string op.
# u01-u08: a shift reads its LEFT operand as a UV (#3760): a negative operand
# is its two's complement, >= 2**64 saturates, a left shift keeps 64 bits, a
# count >= 64 is 0, a negative count reverses.  p01: positive operands are
# UNCHANGED.  i01/i02: `use integer' keeps the signed shift, the compound
# `>>=' / `<<=' included (they cannot see the pragma, #3781).
#
# Every expectation is the output of the SAME program under perl 5.40.3; the
# program needs nothing newer than 5.30.  20 of the rows FAIL on main
# ef5df65a (inverse-checked, s525u).  Two SBCL launches, one per emission path.

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

plan tests => 60;

sub run_pcl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    return $out;
}

my $PROG = <<'PERL';
no warnings;
sub h { join('', map { sprintf "%02x", ord } split //, ($_[0] // '')) }
{ my $x = "ab"; my $s = $x + 0; print "n01=", h($x | "  "), "\n"; }
{ my $x = "ab"; my $s = $x == 0; print "n02=", h($x | "  "), "\n"; }
{ my $x = "ab"; my $s = $x <=> 0; print "n03=", h($x | "  "), "\n"; }
{ my $x = "ab"; my $s = abs($x); print "n04=", h($x | "  "), "\n"; }
{ my $x = "ab"; my $s = int($x); print "n05=", h($x | "  "), "\n"; }
{ my $x = "ab"; my @a = (1, 2); my $s = $a[$x]; print "n06=", h($x | "  "), "\n"; }
{ my $x = "ab"; my $s = sprintf "%d", $x; print "n07=", h($x | "  "), "\n"; }
{ my $x = "ab"; my $s = $x + 0; my $y = $x; print "n08=", h($y | "  "), "\n"; }
{ my $x = "ab"; my $s = $x + 0; my $q = \$x; print "n09=", h($$q | "  "), "\n"; }
{ local $_ = "ab"; my $s = $_ + 0; print "n10=", h($_ | "  "), "\n"; }
{ my $x = "ab"; my $s = $x + 0; print "n11=", h($x & "  "), " ", h($x ^ "  "), "\n"; }
{ my $x = "ab"; my $s = $x + 0; print "n12=", ((~$x) =~ /^\d+$/ ? "NUMERIC" : "STRING"), "\n"; }
{ $! = 2; my $e = $!; print "n13=", ($e | "x"), "\n"; }
{ my $x = "ab"; if ($x) {} my $l = length $x; my $t = $x eq "ab"; print "s01=", h($x | "  "), "\n"; }
{ my $x = "ab"; my $s = $x + 0; $x = "ab"; print "s02=", h($x | "  "), "\n"; }
{ my $x = "ab"; my $y = $x; my $s = $y + 0; print "s03=", h($x | "  "), "\n"; }
{ my $x = "ab"; my $s = $x + 0; $x .= ""; print "s04=", h($x | "  "), "\n"; }
{ my $x = "ab"; my $s = "$x" . sprintf("%s", $x); print "s05=", h($x | "  "), "\n"; }
print "s06=", ("abc" | "   "), "\n";
print "u01=", -1 >> 1, " ", -8 >> 1, " ", -1 >> 63, "\n";
print "u02=", -1 << 1, " ", 3 << 63, " ", 18446744073709551615 << 1, "\n";
print "u03=", 1 << 64, " ", -1 >> 64, " ", -1 << 64, " ", 5 >> 70, " ", 5 << -70, "\n";
print "u04=", 2**64 >> 1, " ", 9**30 >> 1, " ", -(2**70) >> 1, "\n";
print "u05=", "-1" >> 1, " ", 1.9 << 1, " ", -1.5 >> 1, "\n";
{ my $x = -4; my $n = -1; print "u06=", $x << $n, " ", $x >> -1, "\n"; }
print "u07=", "inf" >> 1, " ", -"inf" >> 1, " ", 9**9**9 << 1, "\n";
print "u08=", -9223372036854775808 >> 0, " ", -1 << 0, "\n";
print "p01=", 7 >> 1, " ", 1 << 62, " ", 2**62 << 1, " ", 1 << 63, "\n";
{ use integer; print "i01=", -1 >> 1, " ", -1 << 1, " ", -1 >> 64, " ", 1 << 64, " ", -8 >> 1, "\n"; }
{ use integer; my $x = -7; $x >>= 1; my $z = -1; $z <<= 1; print "i02=", $x, " ", $z, "\n"; }
PERL

my %EXPECT = (
    'n01' => '30',
    'n02' => '30',
    'n03' => '30',
    'n04' => '30',
    'n05' => '30',
    'n06' => '30',
    'n07' => '30',
    'n08' => '30',
    'n09' => '30',
    'n10' => '30',
    'n11' => '30 30',
    'n12' => 'NUMERIC',
    'n13' => '2',
    's01' => '6162',
    's02' => '6162',
    's03' => '6162',
    's04' => '6162',
    's05' => '6162',
    's06' => 'abc',
    'u01' => '9223372036854775807 9223372036854775804 1',
    'u02' => '18446744073709551614 9223372036854775808 18446744073709551614',
    'u03' => '0 0 0 0 0',
    'u04' => '9223372036854775807 9223372036854775807 4611686018427387904',
    'u05' => '9223372036854775807 2 9223372036854775807',
    'u06' => '9223372036854775806 18446744073709551608',
    'u07' => '9223372036854775807 4611686018427387904 18446744073709551614',
    'u08' => '9223372036854775808 18446744073709551615',
    'p01' => '3 4611686018427387904 9223372036854775808 9223372036854775808',
    'i01' => '-1 -2 -1 0 -4',
    'i02' => '-4 -2',
);

for my $opt ('default', 'none') {
    if ($opt eq 'none') { $ENV{PCL_OPT} = 'none' } else { delete $ENV{PCL_OPT} }
    my $out = run_pcl($PROG);
    my %got;
    $got{$1} = $2 while $out =~ /^([nsupi]\d\d)=(.*)$/mg;
    for my $k (sort keys %EXPECT) {
        is($got{$k} // "<MISSING; output was:\n$out>", $EXPECT{$k}, "[$opt] row $k");
    }
}
delete $ENV{PCL_OPT};
