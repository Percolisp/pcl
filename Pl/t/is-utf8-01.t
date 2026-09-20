#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# is-utf8-01.t - `utf8::is_utf8` asks about a STRING (tasks #1995 / #1185).
#
# PCL has no per-scalar UTF-8 flag and the ruling (#1389,
# docs/not-supported.md "The per-scalar UTF-8 flag") is that every PCL STRING
# is in the upgraded form, so 1 is the honest answer for a string.  It said
# nothing about a value that is not a string: a number, undef, a reference, a
# code ref have no character form to be upgraded, and perl answers false for
# every one of them (probed 5.40.3 over 21 shapes).  Answering 1 there made
# core JSON::PP encode EVERY NUMBER AS A STRING, because its
# `_looks_like_number` starts `return if utf8::is_utf8($value)`.
#
# The JSON::PP rows are ACCEPTANCE tests of that generic fact, not module
# support: their expectations are perl 5.40.3's own output.

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

plan skip_all => "pl2cl not found" if !-x $pl2cl;
plan skip_all => "sbcl not found"  if !`which sbcl 2>/dev/null`;

plan tests => 6;

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
    my $got = run_cl($code);
    is($got, $expected, $name);
}

# ---------------------------------------------------------------------------
# The value shapes.  Line 1 is perl's answer BYTE FOR BYTE (a non-string is
# false in both).  Line 2 is PCL's ruled divergence: perl prints "0,1,0,0"
# there because its ASCII strings carry no UTF8 flag, while every PCL string
# is in the upgraded form (#1389) -- that ruling is what this row pins.
# ---------------------------------------------------------------------------
test_cl('is_utf8: a non-string is FALSE, a string is 1',
    'sub b { utf8::is_utf8($_[0]) ? 1 : 0 }
     my $i = 5; my $f = 2.5; my $u; my $r = \1; my $ar = []; my $cr = sub { 1 };
     my $s = "abc"; my $w = "\x{263a}";
     print join(",", b($i), b($f), b(1+1), b($u), b($r), b($ar), b($cr)), "\n";
     print join(",", b($s), b($w), b(""), b("5")), "\n";',
    "0,0,0,0,0,0,0\n1,1,1,1\n");

# perl's false here is a DEFINED empty string, not undef (probed 5.40.3).
test_cl('is_utf8: false is a defined empty string',
    'my $r = utf8::is_utf8(5);
     print "defined=", (defined $r ? 1 : 0), " len=", length($r), "\n";',
    "defined=1 len=0\n");

# A number that was stringified once is still a number; a string that was
# numified once is still a string (PCL keeps the two apart by REPRESENTATION).
test_cl('is_utf8: stringifying a number does not make it a string',
    'my $n = 5; my $x = "$n"; my $s = "7"; my $y = $s + 0;
     print utf8::is_utf8($n) ? 1 : 0, utf8::is_utf8($s) ? 1 : 0, "\n";',
    "01\n");

# ---------------------------------------------------------------------------
# Acceptance: core JSON::PP.  Expectations are perl 5.40.3's own output.
# ---------------------------------------------------------------------------
test_cl('JSON::PP encodes numbers as numbers',
    'use JSON::PP; my $j = JSON::PP->new->canonical;
     print $j->encode([1, 2.5, "s", 1+1, -7, 0]), "\n";',
    "[1,2.5,\"s\",2,-7,0]\n");

test_cl('JSON::PP: a numeric-looking STRING stays a string',
    'use JSON::PP; my $j = JSON::PP->new->canonical;
     my $n = 5; my $t = "7";
     print $j->encode({ a => $n, b => $t, c => $n * 2, d => "x" }), "\n";',
    "{\"a\":5,\"b\":\"7\",\"c\":10,\"d\":\"x\"}\n");

# decode -> encode round trip.  The FLOAT is asserted; the integer half of the
# same shape is task #1513 (PCL has no 64-bit integer boundary, so JSON::PP's
# `$max_intsize` BEGIN loop never terminates and leaves undef, and every
# decoded integer comes back as a string) -- see the DONE section of #1995.
test_cl('JSON::PP: decode/encode round trip keeps a float a number',
    'use JSON::PP; my $j = JSON::PP->new->canonical;
     my $d = $j->decode(q({"a":2.5,"b":"s","c":null}));
     print $j->encode($d), "\n";',
    "{\"a\":2.5,\"b\":\"s\",\"c\":null}\n");
