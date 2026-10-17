#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# A `my` in a while/until CONDITION is a fresh variable each iteration
# (task #3374, docs/ir-spec.md): a closure made in the body or the continue
# block keeps its own iteration's value (Path::Tiny's openw & co. are built in
# `while (my ($k, $v) = each %opens) { *{$k} = sub { ... $v ... } }`).
# The counter-case: a C-style for's `my` stays ONE variable.
# Every row compares against perl (5.38-safe: core only).

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

sub run_perl {
    my ($code) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return `perl $file 2>&1`;
}

# Transpile and run CL, return output
sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    my $cl_code = PCLCore::transpile(qq{$pl2cl $pl_file});

    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^-->.*\n//gm;
    $output =~ s/^==>.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^WARNING:.*\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_wm {
    my ($name, $code) = @_;
    my $perl_out = run_perl($code);
    my $cl_out   = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL:   $cl_out");
}

# 1. #3374 the reproducer: each iteration's condition `my` is its own variable.
test_wm('while-condition my: closures keep their own iteration', <<'PERL');
my %h = (a => 1, b => 2); my %s;
while (my ($k, $v) = each %h) { $s{$k} = sub { $v } }
my @l = (3, 4); my @t;
while (my $x = shift @l) { push @t, sub { $x } }
print join(",", map { "$_=" . $s{$_}->() } sort keys %s), " ", join(",", map { $_->() } @t), "\n";
PERL

# 2. next / last / continue (a closure made there too) / redo.
test_wm('while-condition my: next, last, continue and redo', <<'PERL');
my @t; my @l = (1, 2, 3, 4, 5);
while (my $x = shift @l) {
  next if $x == 2;
  last if $x == 5;
  push @t, sub { "x$x" };
} continue { push @t, sub { "c$x" }; print "cont $x\n" }
print join(",", map { $_->() } @t), "\n";
@t = (); my @z = (1, 2); my $redo = 0;
while (my $y = shift @z) { push @t, sub { "y$y" }; if (!$redo++) { redo } }
print join(",", map { $_->() } @t), "\n";
PERL

# 3. until, nested whiles, two `my`s in one condition.
test_wm('until / nested / two condition mys', <<'PERL');
my @t; my @m = (3, 2, 1);
until (!(my $u = shift @m)) { push @t, sub { "u$u" } }
my @o = ([1, 2], [3, 4]);
while (my $r = shift @o) {
  my @in = @$r;
  while (my $e = shift @in) { push @t, sub { "$r->[0]:$e" } }
}
my @n = (7, 8);
while ((my $p = shift @n) && (my $q = 1)) { push @t, sub { "$p$q" } }
print join(",", map { $_->() } @t), "\n";
PERL

# 4. the counter-cases: a C-style for's `my` IS one variable across
#    iterations in perl, and a loop with no closure keeps its last value.
test_wm('C-style for my stays one variable; no-closure while unchanged', <<'PERL');
my @t;
for (my $i = 0; $i < 3; $i++) { push @t, sub { "i$i" } }
print join(",", map { $_->() } @t), "\n";
my @w = (5, 6); my $last;
while (my $v = shift @w) { $last = $v }
print "last=$last\n";
PERL

done_testing();
