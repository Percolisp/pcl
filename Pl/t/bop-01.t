#!/usr/bin/env perl
# bop-01.t - Tests for old-style prototype ($) argument limiting at call sites
#
# Root cause: sub _and($) { ... } has prototype ($) = 1 scalar arg.
#   is _and 0, '0', 'str'  must parse as  is(_and(0), '0', 'str')
# PCL was parsing it as  is(_and(0, '0', 'str'))  — pl-is got 1 arg → SBCL crash.
#
# Fix: handle_subcalls in PExpr.pm now calls _proto_max_args to limit $end_pars
# when the function has an old-style prototype with no @ or % params.

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

plan tests => 7;

# ── Helpers ─────────────────────────────────────────────────────────────────

sub transpile {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return scalar `$pl2cl $pl_file 2>/dev/null`;
}

sub run_cl {
    my ($code) = @_;
    my $cl_code = transpile($code);
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

# ── Codegen tests ────────────────────────────────────────────────────────────

# Test 1: ($) prototype limits to 1 arg — bop.t crash case
{
    my $cl = transpile('sub _and($) { $_[0] & $_[1] }  is _and 0, "0", "str";');
    like($cl, qr/\(pl-is \(pl-_and 0\)/,
         '($) proto: is _and 0, "0", "str" -> is(_and(0), ...)');
}

# Test 2: without fix, _and would have eaten all 3 args; verify it did not
{
    my $cl = transpile('sub _and($) { $_[0] & $_[1] }  is _and 0, "0", "str";');
    unlike($cl, qr/pl-_and 0 "0" "str"/,
           '($) proto: _and must NOT consume all 3 args');
}

# Test 3: ($$) prototype limits to 2 args
{
    my $cl = transpile('sub add($$) { $_[0] + $_[1] }  print add 3, 4, "\n";');
    like($cl, qr/pl-add 3 4/,
         '($$) proto: add 3, 4, "\n" -> add(3,4) + rest for print');
    unlike($cl, qr/pl-add 3 4 "\\\\n"/,
           '($$) proto: add must NOT consume the trailing \n arg');
}

# Test 4: no prototype — sub without prototype still eats all args
{
    my $cl = transpile('sub add { $_[0] + $_[1] }  print add 3, 4;');
    like($cl, qr/pl-add 3 4/,
         'no proto: add 3, 4 passes both args (no limiting)');
}

# ── Runtime tests ────────────────────────────────────────────────────────────

# Test 5: ($) proto limits at runtime — prototype sub gets 1 arg; rest go to push
{
    test_cl('($) proto: push gets remaining args after proto-limited call',
        'sub double_first($) { return $_[0] * 2 }
my @a;
push @a, double_first 3, 7, 9;
print scalar @a, "\n";
print $a[0], "\n";
print $a[1], "\n";
print $a[2], "\n";',
        "3\n6\n7\n9\n");
}

# Test 6: ($$) proto: sub gets exactly 2 args
{
    test_cl('($$) proto: sub gets 2 args; rest go to outer list',
        'sub sum2($$) { return $_[0] + $_[1] }
my @a;
push @a, sum2 10, 20, 30;
print scalar @a, "\n";
print $a[0], "\n";
print $a[1], "\n";',
        "2\n30\n30\n");
}
