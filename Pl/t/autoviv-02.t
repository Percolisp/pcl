#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# autoviv-02.t — NESTED-ELEMENT autovivification, the write side.
#
# autoviv-01.t is the gate's slowest file, so this is its second home (the
# metric is a file's WALL time, not its row count).
#
# #1058 (s470bk): `$h{a}{b}++` on a fresh hash SILENTLY LOST the increment.
# The chain lowers to (p-gethash-box (p-gethash-box %h "a") "b"); the inner
# call creates key "a" with an undef box and hands it to the outer call — a
# place to vivify INTO — and the outer call returned a fresh DETACHED box
# instead, so `++` incremented a box nobody could reach.  `exists $h{a}` was
# still 1, which is what made it invisible.  The array-slot spelling of the
# same undef is NIL, and that reached SBCL's GETHASH and crashed.
#
# #1057 (s470bk): the coercing compound assigns (`.=` `+=` `*=` `x=` …) build
# their read-modify-write over the place with CL's SETF, whose container
# subform is the plain READ accessor — `(setf (p-gethash (p-gethash %h "a")
# "b") …)` handed :UNDEF to GETHASH and died with an SBCL type error.  Both
# halves are now one rule: an undef container that is a WRITABLE PLACE is
# dereferenced-and-created, which is perl's.
#
# Every expectation below is the live `perl` 5.40.3 answer (probed s470bk,
# scratch/s470bk/p1058/).

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

plan tests => 3;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = PCLCore::transpile("$pl2cl $pl_file");
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# ── 1. #1058: the counting idiom.  The value must be READABLE afterwards,
# through the container and through an alias, and a second ++ must see the
# first — a detached box passes none of those. ─────────────────────────────
is(run_cl(<<'PL'), "1\n2\n1\n-1\n1\n", '#1058 nested ++/-- through an undef container is not lost');
my %h; $h{a}{b}++;               print $h{a}{b}, "\n";
my %g; $g{x}{y}++; $g{x}{y}++;   print $g{x}{y}, "\n";
my %i; $i{p}{q}{r}++;            print $i{p}{q}{r}, "\n";
my %j; $j{m}{n}--;               print $j{m}{n}, "\n";
my $r; $r->{p}{q}++;             print $r->{p}{q}, "\n";
PL

# ── 2. #1058: the array-shaped intermediate.  `$a[0]{k}++` CRASHED ("nil is
# not of type hash-table") because a promoted array HOLE is a box of NIL,
# not of *p-undef*; `$a[0][1]++` and `$h{a}[0]++` lost the write silently. ──
is(run_cl(<<'PL'), "1\n1\n1\nHASH\nARRAY\n", '#1058 array-shaped intermediates vivify to the right kind');
my @a; $a[0]{k}++;   print $a[0]{k}, "\n";
my @b; $b[0][1]++;   print $b[0][1], "\n";
my %h; $h{a}[0]++;   print $h{a}[0], "\n";
my @c; $c[0]{k}++;   print ref($c[0]), "\n";
my @d; $d[0][1]++;   print ref($d[0]), "\n";
PL

# ── 3. #1058: the vivified slot is the CONTAINER's own, so an alias taken
# before the write sees it and one taken after tracks it.  (A detached box
# gave inner=0 here where perl gives 1.) ───────────────────────────────────
is(run_cl(<<'PL'), "1 1\n1\nyes\n", '#1058 the vivified slot is live, not detached');
my %h; my $ref = \$h{a}{b};
print((exists $h{a} ? 1 : 0), " ", (exists $h{a}{b} ? 1 : 0), "\n");
$$ref = 1; print $h{a}{b}, "\n";
my %g; $g{a}{b}++; my $r2 = \$g{a}{b}; $$r2 += 0;
print(($g{a}{b} == 1 ? "yes" : "no"), "\n");
PL

