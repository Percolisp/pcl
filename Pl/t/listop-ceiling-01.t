#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# listop-ceiling-01.t — task #343 (Option B phase 2, Track B2): the paren-less
# list-operator argument CEILING.  Design: docs/b2-ceiling-fix-s418.md;
# mechanism: docs/b2-stale-operand-ceiling-s417.md.
#
# `f ref $u, "m" or g "fb"` must parse as `f(ref($u), "m") or g("fb")` — the
# argument list of a paren-less list operator ends before the nearest
# same-level `and`/`or`/`xor`.  handle_subcalls used to CACHE that operator's
# index during its right-to-left scan; the same scan's own reductions splice
# @$e and shift the operator left, so the cached position went stale and the
# argument list swallowed the `or`.  The error is (elements the intervening
# reduction consumed − 1): with one `ref $u` between word and `or` the
# statement DROPPED, with `ref $h{k}` (three elements) it RAN WRONG —
# perl `f() g(fb)`, PCL `g(fb) f(1)`.  The fix derives the boundary from the
# CURRENT @$e at the point of use; there is no cached index to go stale.
#
# Every expectation below is the live `perl` answer (probed s418).

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

plan tests => 5;

# The f/g/h vocabulary every row below shares: each sub prints its name and
# args, and returns what the row needs for the short-circuit under test.
my $SUBS = <<'PL';
sub f0 { print "f(@_)\n"; 0 }
sub f1 { print "f(@_)\n"; 1 }
sub g0 { print "g(@_)\n"; 0 }
sub g1 { print "g(@_)\n"; 1 }
sub h1 { print "h(@_)\n"; 1 }
my $u = bless {}, "main";
PL

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

# Transpile (a DROPPED statement fails the row, via PCLCore) and run.
sub run_cl {
    my ($code) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# ── 1. The family that was swallowed: a reduction between the word and the
# boundary — the s417 DROP, #343's headline, the shift-2 SILENT WRONG (perl
# order f() then g(fb); PCL printed g(fb) then f(1)), the and/xor spellings,
# two boundaries (nearest wins), and two reductions before one boundary. ─────
is(run_cl($SUBS . <<'PL'),
my %h = (k => "v");
my $v = [1];
f0 ref $u or g1 "fb";
f1 ref $u, "m" or g1 "fb";
f0 ref $h{k} or g1 "fb";
f0 ref $u and g1 "x";
f0 ref $u xor g1 "y";
f0 ref $u or g0 "a" or h1 "b";
f0 ref $u, ref $v or g1 "x";
PL
   "f(main)\ng(fb)\nf(main m)\nf()\ng(fb)\nf(main)\nf(main)\ng(y)\n"
   . "f(main)\ng(a)\nh(b)\nf(main ARRAY)\ng(x)\n",
   'a reduction between a paren-less call and its and/or/xor boundary');

# ── 2. The two population shapes the fix un-dropped ──────────────────────────
# bless.t:179's grammar (user subs standing in for is/diag — same parse), and
# split.t:503's: a list-assignment of a grep/map pipeline with `or` after,
# probed on BOTH branches so the boundary is proven on the value path too.
is(run_cl($SUBS . <<'PL'),
f0 ref $u, "main", "desc" or g1 "diagnosed";
sub skipit { print "skip(@_)\n" }
my ($sp) = grep /x/, map chr, reverse 65 .. 70 or skipit 'x', 9;
my ($q) = grep /B/, map chr, reverse 65 .. 70 or skipit 'y', 9;
print "sp=", (defined $sp ? $sp : "undef"), " q=", (defined $q ? $q : "undef"), "\n";
PL
   "f(main main desc)\ng(diagnosed)\nskip(x 9)\nsp=undef q=B\n",
   'the bless.t and split.t row shapes (the two un-dropped census sites)');

# ── 3. INVERSE GUARDS — shapes that must NOT move ────────────────────────────
# No reduction between word and boundary (the always-correct case), explicit
# parens, and no boundary at all.
is(run_cl($SUBS . q{f0 "a" or g1 "fb"; f1 "b", "m" or g1 "x"; f0(ref $u, "m") or g1 "p"; f1 ref $u, "m";} . "\n"),
   "f(a)\ng(fb)\nf(b m)\nf(main m)\ng(p)\nf(main m)\n",
   'inverses: no-reduction, parenthesized, and boundary-less all unchanged');

# The stale-but-benign reg_fold.t shape: eval (a named unary) with a
# reduction inside and `or` after — the term walker bounds the operand, and
# the boundary still ends the statement's or-chain correctly.
is(run_cl(q{my @t = ("print 'A';", "print 'B';"); eval join "", @t or die $@; print "\n";} . "\n"),
   "AB\n",
   'eval join ... or die $@ — the benign named-unary shape stays correct');

# ── 5. The BLOCK-FORM argument run gets the SAME ceiling (s454ae, rule 11):
# _take_rest_as_args used to consume to END OF STREAM, so `grep { … } LIST or
# EXPR` swallowed the `or` (SILENT WRONG: r read 1, perl says 2) and
# `$c ? grep { … } @a : ()` swallowed `: ()` (orphaned colon → drop).  The
# nested-ternary-inside-the-args negative rides along.  Perl-probed s455. ─────
is(run_cl($SUBS . <<'PL'),
sub skipit { print "skip(@_)\n" }
my $r = grep { $_ > 1 } (1,2,3) or g1 "never";
print "r=$r\n";
my @z = 1 ? grep { $_ % 2 } (1,2,3,4) : ();
print "z=@z\n";
my @w = map { $_ + 1 } 1 ? (10,20) : (30,40);
print "w=@w\n";
my ($e) = grep { /x/ } ("a","b") or skipit "bf", 3;
print "e=", (defined $e ? $e : "undef"), "\n";
PL
   "r=2\nz=1 3\nw=11 21\nskip(bf 3)\ne=undef\n",
   'block-form grep/map argument run ends at or / enclosing-ternary colon');
