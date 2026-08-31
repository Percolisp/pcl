#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# version-shim-01.t — lib/version.pm, the shim for perl's `version` module.
#
# Task #319: every t/op/packagev.t row that validates a version string calls
# version::is_strict / version::is_lax, and the shim had neither, so the file
# died with `undef-fn:version::pl-is_strict` after 5 of its 307 rows.
#
# Perl ships these two in version/regex.pm, where each pattern is composed by
# interpolating qr// objects into other qr// objects.  The shim spells each as
# ONE literal pattern instead (no qr-in-qr), so the grammars have to be
# expanded by hand — which is exactly the kind of transcription that needs a
# guard.  Every expectation below is the answer the REAL `version::` gives
# (perl 5.40.3); the strings are packagev.t's own table.

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

plan tests => 4;

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

# The acceptance table, verbatim from t/op/packagev.t's __DATA__ (the STRICT
# column is the `package NAME VERSION` grammar; the LAX column is what
# version->new accepts), plus the two `undef`/`v` oddities.
my $STRICT_CASES = <<'PL';
require version;
for my $v (qw(1.00 1.00001 0.123 12.345 42 0 0.0 v1.2.3 v1.2.3.4 v0.1.2 v0.0.0
              01 01.0203 v01 v01.02.03 1.2.3 v1.2 v0 v1 v1.2345.6 undef 1a bar _)) {
  print version::is_strict($v) ? 1 : 0;
}
print "\n";
PL
is(run_cl($STRICT_CASES), "111111111110000000000000\n",
   "is_strict: the eleven strict spellings pass, the thirteen packagev.t rejects fail");

my $LAX_CASES = <<'PL';
require version;
for my $v (qw(1.00 42 v1.2.3 01 01.0203 v01 v01.02.03 1.2.3 v1.2 v0 v1
              1.02_03 v1.2_3 v1.02_03 v1.2345.6 undef
              1.a 1._ 0_ 1_ 1_. 1.1_ 1.02_03_04 v.1.2.3 v 1a 1.2a3 bar _)) {
  print version::is_lax($v) ? 1 : 0;
}
print "\n";
PL
is(run_cl($LAX_CASES), "11111111111111110000000000000\n",
   "is_lax: the sixteen lax spellings pass, the thirteen non-versions fail");

# --------------------------------------------------------------------------
# Task #870: a version is a TUPLE OF INTEGER COMPONENTS, not a string and not
# a number.  vcmp used to be `$a cmp $b`, which made `$] < version->new(
# '5.14.0')` TRUE ("5.030000" lt "5.14.0" as text, while v5.30.0 gt v5.14.0)
# and every `$version >= 5.010` idiom in CPAN code silently wrong.
#
# Every expectation below is the REAL `version::` module's answer (perl
# 5.40.3), taken from a 3864-check run over 56 version strings covering all
# four spellings (decimal / dotted / leading-v / alpha) in both directions.

my $TUPLE_CASES = <<'PL';
require version;
my @out;
for my $t (qw(5 5.005 5.005_03 1.2 1.02 1.0203 v1.2 v1.2.3 1.2.3 1.2.3.4
              v5.14.0 5.030000 0.9933 v1.2_3 1.2.3_4 v2)) {
  my $v = version->new($t);
  push @out, $v->numify . "|" . $v->normal . "|" . ($v->is_qv ? 1 : 0) . ($v->is_alpha ? 1 : 0);
}
print join(" ", @out), "\n";
PL
is(run_cl($TUPLE_CASES),
   "5.000|v5.0.0|00 5.005|v5.5.0|00 5.005030|v5.5.30|01 1.200|v1.200.0|00 "
   . "1.020|v1.20.0|00 1.020300|v1.20.300|00 1.002000|v1.2.0|10 "
   . "1.002003|v1.2.3|10 1.002003|v1.2.3|10 1.002003004|v1.2.3.4|10 "
   . "5.014000|v5.14.0|10 5.030000|v5.30.0|00 0.993300|v0.993.300|00 "
   . "1.023000|v1.23.0|11 1.002034|v1.2.34|11 2.000000|v2.0.0|10\n",
   "numify/normal/is_qv/is_alpha: a decimal fraction cuts into 3-digit groups, "
   . "a dotted version pads to three components, and `_` is REMOVED (not a separator)");

# The comparison matrix, plus the two spellings the bug was reported through:
# a plain decimal STRING on the LEFT (the swapped overload call) and `$]`.
# The `$]` row is written so it holds for any perl >= 5.6 — PCL reports
# 5.030000 and this perl 5.040003 (task #871), and this row is about vcmp.
my $CMP_CASES = <<'PL';
require version;
my @s = ('5.030000','5.14.0','v5.14.0','5.005','v5.5.0','1.2','v1.200.0','1.02','0');
my @rows;
for my $a (@s) {
  my $r = '';
  for my $b (@s) { $r .= version->new($a) <=> version->new($b) }
  push @rows, $r;
}
print join(" ", @rows), "\n";
print "str ", (("5.030000" < version->new('5.14.0')) ? 1 : 0),
              (("5.030000" >= version->new('5.30.0')) ? 1 : 0),
              ("5.030000" <=> version->new('5.14.0')), "\n";
print "dol ", (($] >= version->new('5.6.0')) ? 1 : 0),
              (($] < version->new('99.0.0')) ? 1 : 0), "\n";
PL
is(run_cl($CMP_CASES),
   "011111111 -100111111 -100111111 -1-1-1001111 -1-1-1001111 -1-1-1-1-10011 "
   . "-1-1-1-1-10011 -1-1-1-1-1-1-101 -1-1-1-1-1-1-1-10\n"
   . "str 011\n"
   . "dol 11\n",
   "vcmp is componentwise (5.030000 == v5.30.0 > v5.14.0), in BOTH operand orders");
