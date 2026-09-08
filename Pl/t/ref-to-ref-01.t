#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ref-to-ref-01.t — ref() / reftype() distinguish a scalar reference from a
# reference-to-a-reference (session 217).
#
# The bug: p-ref classified a ref-to-ref ("REF") as "SCALAR" because box-nesting
# depth does not separate the two — a `my`-bound ref ($r = \$x) round-trips to
# the same depth as a plain scalar ref. The fix uses the box `is-ref` flag (set
# only by p-backslash on scalar-ref wrappers) to find the referent, and reports
# "REF" iff that referent is itself a wrapper or *holds* a reference. The
# held-a-ref test is non-recursive, so a self-referential scalar ($x = \$x) does
# not loop and plain scalars (incl. undef, '' array elements) stay "SCALAR".

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

plan tests => 20;

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
    is(run_cl($code), $expected, $name);
}

# ── plain scalar ref vs ref-to-ref, through `my` variables ───────────────────
# NB run_cl strips blank lines, so an empty ref() prints nothing observable.
test_cl('ref(non-ref) is empty',
    'my $x = 1; print "[", ref($x), "]\n";', "[]\n");

test_cl('ref($r) where $r = \\$x is SCALAR',
    'my $x = 1; my $r = \$x; print ref($r), "\n";', "SCALAR\n");

test_cl('ref($rr) where $rr = \\$r is REF',
    'my $x = 1; my $r = \$x; my $rr = \$r; print ref($rr), "\n";', "REF\n");

# ── direct (no intermediate variable) ────────────────────────────────────────
test_cl('ref(\\$x) direct is SCALAR',
    'my $x = 5; print ref(\$x), "\n";', "SCALAR\n");

test_cl('ref(\\\\1) direct is REF',
    'print ref(\\\\1), "\n";', "REF\n");

test_cl('ref(\\$r) direct is REF',
    'my $x = 1; my $r = \$x; print ref(\$r), "\n";', "REF\n");

# ── aggregate refs unaffected ────────────────────────────────────────────────
test_cl('ref(\\@a) is ARRAY',
    'my @a = (1,2); print ref(\@a), "\n";', "ARRAY\n");

test_cl('ref of scalar holding an arrayref, through a var, is REF',
    'my @a = (1,2); my $ar = \@a; my $rar = \$ar; print ref($rar), "\n";', "REF\n");

# ── undef referent stays SCALAR (regression guard: *p-undef* must not look ref) ─
test_cl('ref(\\$undef) is SCALAR',
    'my $u; print ref(\$u), "\n";', "SCALAR\n");

# ── self-referential scalar must not loop, and is REF ────────────────────────
test_cl('self-referential $x = \\$x is REF (no hang)',
    'my $x; $x = \$x; print ref($x), "\n";', "REF\n");

# ── reftype of a ref-to-ref is SCALAR (the referent scalar) ──────────────────
test_cl('reftype($rr) is SCALAR',
    'use Scalar::Util qw(reftype);'
  . 'my $x=1; my $r=\$x; my $rr=\$r; print reftype($rr), "\n";', "SCALAR\n");

test_cl('reftype($r) of a plain scalar ref is SCALAR',
    'use Scalar::Util qw(reftype);'
  . 'my $x=1; my $r=\$x; print reftype($r), "\n";', "SCALAR\n");

# ── glob ref numifies to a (non-zero) address like other refs; a *bare* glob
#    numifies to 0.  Both share box-value=typeglob — the is-ref flag set by
#    p-backslash (and preserved by box-set) is the only discriminator.  (Exact
#    address round-trip is GC-fragile, so we only assert non-zero-ness here.)
test_cl('glob ref numifies to a non-zero address',
    'our $g_t = 5; my $r = \*g_t; print +($r + 0 != 0 ? "nonzero" : "zero"), "\n";',
    "nonzero\n");

test_cl('bare glob in scalar numifies to 0',
    'our $g_t = 5; my $g = *g_t; print 0 + $g, "\n";', "0\n");

# ── ${ARRAY-or-HASH ref} is perl's fatal (task #1249(1), s473h) ──────────────
#    The REFERENT rule is the discriminator: `\@a`'s referent is the array
#    itself, while a `\$aref` read back out of a container has the same UNBOXED
#    shape but a scalar BOX for a referent (#154's ambiguity).  A CODE referent
#    is excluded on purpose — PCL collapses a scalar-ref-to-coderef, which is
#    Sub::Quote's shape (Pl/t/moo-01.t).
test_cl('${$aryref} dies "Not a SCALAR reference"',
    'my @a=(1,2); my $r=\@a; my $v = eval { "".${$r} };'
  . 'print +($@ =~ /^Not a SCALAR reference/ ? "died" : "no:[$@]"), "\n";',
    "died\n");

test_cl('${$hashref} dies "Not a SCALAR reference"',
    'my %h=(k=>1); my $r=\%h; my $v = eval { "".${$r} };'
  . 'print +($@ =~ /^Not a SCALAR reference/ ? "died" : "no:[$@]"), "\n";',
    "died\n");

test_cl('${$coderef} does NOT die (PCL collapses scalar-ref-to-coderef)',
    'sub cc { 1 } my $r=\&cc; my $v = eval { "".${$r} };'
  . 'print +($@ ? "died:[$@]" : "ok"), "\n";', "ok\n");

test_cl('${ \\$aryref } is the array ref, not a fatal',
    'my @a=(1,2); my $ar=\@a; my $rr=\$ar; print ref(${$rr}), "\n";', "ARRAY\n");

test_cl('${ \\$hashref } is the hash ref, not a fatal',
    'my %h=(k=>1); my $hr=\%h; my $rr=\$hr; print ref(${$rr}), "\n";', "HASH\n");

test_cl('\\(@b, 9) is (ARRAY, SCALAR) and ${$r[0]} is the fatal',
    'my @b=(7,8); my @r = \(@b, 9);'
  . 'print scalar(@r), ref($r[0]), ref($r[1]), "\n";'
  . 'eval { my $v = "".${$r[0]} };'
  . 'print +($@ =~ /^Not a SCALAR reference/ ? "died" : "no"), "\n";',
    "2ARRAYSCALAR\ndied\n");
