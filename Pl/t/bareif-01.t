#!/usr/bin/env perl
# bareif-01.t - Tests for bare-if implicit return value (B1 in todo-features.md)
#
# Perl rule: the return value of a sub is the last expression *evaluated*.
# For `if (COND) {}` with no else, COND is the last thing evaluated when
# COND is false. PCL must return COND, not undef.
#
# See docs/bare-if-return-plan.md for full design.

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

plan tests => 20;

# ── Helpers ──────────────────────────────────────────────────────────────────

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
    my ($code, $expected, $name) = @_;
    my $got = run_cl($code);
    is($got, $expected, $name);
}

# ── Transpilation: check generated CL contains the let+setf pattern ──────────

# Test 1: bare if without else generates --pcl-if-ret-- let binding
{
    my $cl = transpile('sub f { if(0) { 5 } }');
    like($cl, qr/pcl-if-ret/, 'bare if-no-else generates ret-var let binding');
}

# Test 2: if with else does NOT generate --pcl-if-ret-- (no transformation needed)
{
    my $cl = transpile('sub f { if(0) { 5 } else { 9 } }');
    unlike($cl, qr/pcl-if-ret/, 'if-with-else not transformed');
}

# Test 3: non-last if does NOT generate --pcl-if-ret--
{
    my $cl = transpile('sub f { if(0){5}; 42 }');
    unlike($cl, qr/pcl-if-ret/, 'non-last if not transformed');
}

# ── Runtime: block-form if without else ──────────────────────────────────────

# Test 4: false numeric cond returns the cond value
test_cl('sub f { if(0)  { 5 } } print f()',      "0",  'if false 0 returns 0');

# Test 5: false string cond returns the cond value
test_cl('sub f { if("") { 5 } } print f()',       "",   'if false "" returns ""');

# Test 6: false var cond returns the var value
test_cl('sub f { my $n=0; if($n) { 5 } } print f()', "0", 'if false var returns var');

# Test 7: true cond returns the body's last expression
test_cl('sub f { if(1)  { 5 } } print f()',       "5",  'if true returns body last expr');

# Test 8: true cond, multi-stmt body — returns last stmt of body
test_cl('sub f { if(1)  { 3; 5 } } print f()',    "5",  'if true, multi-stmt body returns last');

# Test 9: false cond, multi-stmt body — returns the cond
test_cl('sub f { if(0)  { 3; 5 } } print f()',    "0",  'if false, multi-stmt body returns cond');

# ── Runtime: unless without else ─────────────────────────────────────────────

# Test 10: unless with true cond (body skipped) returns the cond value
test_cl('sub f { unless(1) { 5 } } print f()',    "1",  'unless true returns cond');

# Test 11: unless with false cond (body runs) returns body last expr
test_cl('sub f { unless("") { 5 } } print f()',   "5",  'unless false returns body');

# ── Runtime: elsif chain without else ────────────────────────────────────────

# Test 12: all conditions false — returns value of the last condition evaluated
test_cl('sub f { if(0){1} elsif(0){2} } print f()', "0", 'elsif all false returns last cond');

# Test 13: second branch taken
test_cl('sub f { if(0){1} elsif(7){2} } print f()', "2", 'elsif second branch returns body');

# Test 14: first branch taken
test_cl('sub f { if(3){1} elsif(0){2} } print f()', "1", 'elsif first branch taken returns body');

# ── Runtime: nested if-without-else ──────────────────────────────────────────

# Test 15: outer true, inner false — returns inner cond
test_cl('sub f { if(1) { if(0) { 5 } } } print f()', "0", 'nested if, inner false returns inner cond');

# Test 16: outer true, inner true — returns inner body
test_cl('sub f { if(1) { if(1) { 5 } } } print f()', "5", 'nested if, inner true returns body');

# Test 17: outer false — returns outer cond
test_cl('sub f { if(0) { if(1) { 5 } } } print f()', "0", 'nested if, outer false returns outer cond');

# ── Runtime: postfix if/unless ────────────────────────────────────────────────

# Test 18: postfix if, false cond — returns cond
test_cl('sub f { 5 if 0 } print f()',   "0",  'postfix if false returns cond');

# Test 19: postfix if, true cond — returns expr
test_cl('sub f { 5 if 1 } print f()',   "5",  'postfix if true returns expr');

# Test 20: postfix unless, true cond (body skipped) — returns cond
test_cl('sub f { 5 unless 1 } print f()', "1",  'postfix unless true returns cond');
