#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# special-foreach-01.t -- task #3612: `foreach $\ (LIST)` / `foreach $/ (LIST)`.
#
# perl keeps the output / input record separator in an internal copy
# (PL_ors_sv / PL_rs) that only the variable's SET-MAGIC updates.  A foreach
# ALIASES the variable to each element without that magic, so print, readline
# and chomp keep using the value from BEFORE the loop for the loop's dynamic
# extent -- while the body (and a sub it calls) reads the element as $\ / $/.
# Assigning or `local`izing $\ inside such a loop writes the ELEMENT only.
# PCL: the foreach macros freeze the pre-loop value in *p-loop-frozen-ors* /
# *p-loop-frozen-rs* (%p-magic-loop-var-wrap); print / get-input-record-
# separator read that box when it is bound.  Text-CSV t/45_eol.t and
# t/46_eol_si.t loop `for $\ (undef, $rs)`.
#
# Expected = perl 5.40.3's output of the same program (probed s523d).

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

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub cl_of { PCLCore::transpile("$pl2cl " . write_pl($_[0])) }
sub run_cl {
    my ($code) = @_;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh cl_of($code);
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

my $prog = <<'PERL';
for $\ (undef, "!") { print "a"; print "|" }
print "\n";
open my $fh, '<', \"x\ry\rz\n"; for $/ ("\r") { my @r = <$fh>; print scalar(@r), "\n" }
{ local $\ = "!"; print "L" } print "\n";
for my $x (1,2) { $\ = "#" if $x == 2; print "b" } $\ = undef; print "\n";
for my $x (1,2) { print $x } print "\n";
our $g; for $g (1,2) { print $g } print "\n";
for $, (":") { print "p","q" } print "\n";
my @e=("A"); for $\ (@e) { $\ = "!"; print "x" } print "|\n";
@e=("A"); for $\ (@e) { local $\ = "!"; print "y" } print "|\n";
sub f { print "s" } for $\ (@e) { f(); print "[$\]" } print "|\n";
$\="Q"; for $\ (@e) { print "z" } $\=undef; print "|\n";
for $/ ("\r") { print length($/), "\n"; my $s="a\r"; chomp $s; print length($s),"\n" }
for $\ (1..2) { print "r" } print "|\n";
for $\ ("a") { for $\ ("b") { print "n" } print "m" } print "|\n";
print "@e\n";
PERL

# perl 5.40.3 output, byte for byte (no ORS at the end: $\ is undef there).
my $want = <<'WANT';
a|a|
1
L!
bb#
12
12
p:q
x|
y|
s[A]|
zQ|
1
2
rr|
nm|
A
WANT

is(run_cl($prog), $want, 'foreach $\\ / $/: print, readline and chomp keep the pre-loop separator');

# the plain loop paths are unchanged
is(run_cl('for my $x (1, 2) { print $x } our $g; for $g (3, 4) { print $g } print "\\n";'), "1234\n", 'for my $x / for $pkgvar: unchanged');

done_testing();
