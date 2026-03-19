#!/usr/bin/env perl
# vec-01.t - Tests for vec() builtin and lvalue assignment

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 17;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
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

# ── Reading vec (should already work) ──────────────────────────────────────

test_cl("vec read: bit 0 of undef is 0",
    'my $foo; print vec($foo, 0, 1), "\n";',
    "0\n");

test_cl("vec read: 8-bit from string",
    'my $s = "A"; print vec($s, 0, 8), "\n";',
    "65\n");

test_cl("vec read: beyond string is 0",
    'my $s = "A"; print vec($s, 10, 8), "\n";',
    "0\n");

# ── Writing vec (lvalue assignment) ────────────────────────────────────────

test_cl("vec lvalue: set 8-bit value",
    'my $s; vec($s, 0, 8) = 0x41; print $s, "\n";',
    "A\n");

test_cl("vec lvalue: set second 8-bit",
    'my $s = "AB"; vec($s, 1, 8) = 0x43; print $s, "\n";',
    "AC\n");

test_cl("vec lvalue: set 1-bit (bit 0)",
    'my $foo; vec($foo, 0, 1) = 1; print vec($foo, 0, 1), "\n";',
    "1\n");

test_cl("vec lvalue: bit 20 roundtrip",
    'my $foo; vec($foo, 20, 1) = 1; print vec($foo, 20, 1), "\n";',
    "1\n");

test_cl("vec lvalue: length after setting bit 20",
    'my $foo; vec($foo, 20, 1) = 1; print length($foo), "\n";',
    "3\n");

test_cl("vec lvalue: 8-bit extend",
    'my $foo; vec($foo, 0, 1) = 1; vec($foo, 1, 8) = 0xf1;
     print vec($foo, 1, 8), "\n";',
    "241\n");

test_cl("vec lvalue: 2-nibble read back",
    'my $foo; vec($foo, 0, 1) = 1; vec($foo, 1, 8) = 0xf1;
     print vec($foo, 2, 4), "\n";',
    "1\n");

test_cl("vec lvalue: high nibble read back",
    'my $foo; vec($foo, 0, 1) = 1; vec($foo, 1, 8) = 0xf1;
     print vec($foo, 3, 4), "\n";',
    "15\n");

test_cl("vec lvalue: 32-bit write and read",
    'my $Vec; vec($Vec, 0, 32) = 0xbaddacab;
     print vec($Vec, 0, 32), "\n";',
    "3135089835\n");

# ── Error handling ──────────────────────────────────────────────────────────

test_cl("vec error: illegal bits=3",
    'my $foo = "x"; my $x = eval { vec $foo, 0, 3 };
     print $@ =~ /Illegal number of bits/ ? "ok" : "fail:$@", "\n";',
    "ok\n");

test_cl("vec error: illegal bits=0",
    'my $foo = "x"; my $x = eval { vec $foo, 0, 0 };
     print $@ =~ /Illegal number of bits/ ? "ok" : "fail:$@", "\n";',
    "ok\n");

test_cl("vec error: illegal bits=-13",
    'my $foo = "x"; my $x = eval { vec $foo, 0, -13 };
     print $@ =~ /Illegal number of bits/ ? "ok" : "fail:$@", "\n";',
    "ok\n");

test_cl("vec error: negative offset lvalue",
    'my $foo = "x"; my $x = eval { vec($foo, -1, 4) = 2 };
     print $@ =~ /Negative offset/ ? "ok" : "fail:$@", "\n";',
    "ok\n");

test_cl("vec: beyond end returns 0",
    'my $foo = "x"; print vec($foo, 7, 8) == 0 ? "ok" : "fail", "\n";',
    "ok\n");
