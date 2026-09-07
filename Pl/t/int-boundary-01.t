#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# int-boundary-01.t — the DOCUMENTED answers at perl's 64-bit integer boundary.
#
# PCL's numbers are CL integers (unbounded) plus doubles: there is no IV/UV
# distinction and no boundary, so integer arithmetic never promotes to NV,
# `use integer` never wraps, and sprintf "%u"/"%d" never clamp.  That is a
# deliberate divergence, USER-ruled 2026-09-07 to be DOCUMENTED rather than
# implemented — docs/not-supported.md §"Integers are unbounded: PCL has no
# 64-bit boundary", revisit pointer task #1513.
#
# THIS FILE ASSERTS CURRENT BEHAVIOUR, ON PURPOSE.  It is not a bug guard and
# INVERSE VERIFICATION IS N/A (there is no "before" tree where these rows
# fail; they pass at 41ca2496 and at HEAD alike).  Its job is the opposite of
# the usual one: if someone ever implements the boundary, these rows fail and
# the failure is the signal that the documented contract — and the
# not-supported.md section, the op/numconvert.t registration in
# baselines/perl-suite-expected.tsv, and the ir-spec §2.6 numeric-model
# paragraph — all have to move in the same commit.  Every `perl:` comment
# below is perl 5.40.3, probed s473a.
#
# Row 1 additionally pins the section HEADING, because three baselines and
# docs/difftest-fuzzer.md cite it by name.

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

plan tests => 19;

# ── the documented contract ──────────────────────────────────────────────────

{
    my $ns = "$project_root/docs/not-supported.md";
    open my $fh, '<:encoding(UTF-8)', $ns or die "open $ns: $!";
    my $found = 0;
    while (<$fh>) {
        $found = 1, last
            if /^## Integers are unbounded: PCL has no 64-bit boundary\b/;
    }
    close $fh;
    ok($found,
       'docs/not-supported.md still carries the "Integers are unbounded" section '
       . '(baselines/fail-baseline.tsv, perl-suite-expected.tsv, row-shortfall.tsv '
       . 'and docs/difftest-fuzzer.md cite it by name)');
}

# ── one transpile + one SBCL run for the whole fixture ───────────────────────

my $fixture = <<'PL';
my $uv = ~0;                            # 18446744073709551615 on both sides
my $iv = 9223372036854775807;           # IV_MAX
print "D1 ", $uv ** 6, "\n";
print "D2 ", sprintf("%u", $uv ** 6), "\n";
print "D3 ", do { use integer; $uv * 16 }, "\n";
print "D4 ", $uv * 16, "\n";
print "D5 ", $uv + 1, "\n";
print "D6 ", do { use integer; $iv + 1 }, "\n";
print "D7 ", sprintf("%d", $uv), "\n";
print "D8 ", 2 ** 64, "\n";
print "D9 ", do { use integer; 2 ** 63 }, "\n";
print "S1 ", $iv + 1, "\n";
print "S2 ", -3 % 5, "\n";
print "S3 ", do { use integer; -3 % 5 }, "\n";
print "S4 ", (($uv - 3) != $uv ? "ne" : "eq"), "\n";
print "S5 ", $uv, "\n";
print "LATE ", 0xffffffff & ($uv * 16), "\n";
my $acc = 0;
for my $c (map { ord } split //, "The quick brown fox jumps over the lazy dog") {
    $acc = (($acc * 31) + $c) & 0xffffffff;
}
print "MASKED $acc\n";
my $u = 1;
$u = $u * 31 + 7 for 1 .. 40;
print "UNMASKED $u\n";
PL

my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
print $fh $fixture;
close $fh;
my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
print $cl_fh $cl_code;
close $cl_fh;
my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
$out =~ s/^;.*\n//gm;
$out =~ s/^PCL Runtime loaded\n//gm;
$out =~ s/^\s*\n//gm;

my %got;
$got{$1} = $2 while $out =~ /^(\w+) (.*)$/mg;

sub line_is {
    my ($key, $want, $name) = @_;
    is($got{$key} // "(missing: $out)", $want, $name);
}

my $POW6 = '39402006196394479199463117884618153312446490372007876911'
         . '560089010528390154342399181505217109422728930545305988890625';

# ── the nine divergences (perl values in the comments) ───────────────────────

line_is('D1', $POW6,
        '~0 ** 6 is the exact integer          [perl: 3.94020061963945e+115]');
line_is('D2', $POW6,
        'sprintf "%u" does not clamp to UV_MAX [perl: 18446744073709551615]');
line_is('D3', '295147905179352825840',
        'use integer does not wrap on *        [perl: -16]');
line_is('D4', '295147905179352825840',
        '~0 * 16 does not promote to NV        [perl: 2.95147905179353e+20]');
line_is('D5', '18446744073709551616',
        '~0 + 1 does not promote to NV         [perl: 1.84467440737096e+19]');
line_is('D6', '9223372036854775808',
        'use integer does not wrap on +        [perl: -9223372036854775808]');
line_is('D7', '18446744073709551615',
        'sprintf "%d" does not reinterpret     [perl: -1]');
line_is('D8', '18446744073709551616',
        '2 ** 64 is exact                      [perl: 1.84467440737096e+19]');
line_is('D9', '9223372036854775808',
        'use integer; 2 ** 63 is exact         [perl: 9.22337203685478e+18]');

# ── the neighbours that AGREE — the divergence is narrow, not pervasive ──────

line_is('S1', '9223372036854775808',
        'IV_MAX + 1 outside use integer agrees with perl (IV -> UV, still exact)');
line_is('S2', '2',
        '-3 % 5 agrees with perl (2)');
line_is('S3', '-3',
        'use integer; -3 % 5 agrees with perl (-3): truncating division IS implemented');
line_is('S4', 'ne',
        '~0 - 3 != ~0 agrees with perl');
line_is('S5', '18446744073709551615',
        '~0 itself prints 18446744073709551615 on both sides');

# ── the explicit-mask rule, and its limit ────────────────────────────────────

line_is('MASKED', '3685539155',
        'an accumulator masked EVERY step agrees with perl (the pure-Perl digest idiom)');
line_is('LATE', '4294967280',
        'a mask applied AFTER an overflowed intermediate diverges [perl: 4294967295, '
        . 'because perl\'s NV clamps to UV_MAX on the way into &]');

# ── the hazard the section names ─────────────────────────────────────────────

line_is('UNMASKED',
        '556606434686927407281217512937805839416028790691202616241281',
        'an UNMASKED accumulator grows without bound [perl: 5.56606434686927e+59] '
        . '— the documented cost hazard, not just a wrong value');

ok(length($got{'UNMASKED'} // '') == 60,
   'the unmasked accumulator really is a 60-digit bignum after 40 iterations');
