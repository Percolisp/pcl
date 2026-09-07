#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# perf-levers-04.t — the round-31 ARITHMETIC levers (task #1514,
# docs/plan-speed-and-ir-s470.md §A.4), guarded the way perf-levers-03.t
# guards round 29's: the levers are RUNTIME-only, `pl2cl`'s output is
# byte-identical with and without them (corpus-diff IDENTICAL over 111), so a
# transpile grep can say nothing.  What can be asserted is (a) the MECHANISM —
# the two cold arms exist and `p-int` is inline for user code — and (b) that
# every shape either arm can be handed still answers perl 5.40.3's answer.
#
#   p-int (task #1514 half (b)): int() used to be ONE FULL CALL with
#     `to-number`'s typecase in front of the float test.  It is now a typecase
#     whose two hot arms — an INTEGER is its own int(), a DOUBLE-FLOAT is one
#     nan/inf test and a truncate — are INLINE at the call site, with
#     everything else (a box, a string, a single-float, a ratio) one call away
#     in %p-int-slow.  That call cost was 9.8 ns of a 50 ns arithmetic
#     iteration, and `use integer` pays it on EVERY operand.
#   %p-divide-numbers (same task): `/` on two integers built a RATIO — a gcd
#     and a heap allocation — and then coerced it, dividing all over again.
#     Inside `(signed-byte 53)` both operands are exactly representable as
#     doubles, so ONE truncate (for perl's exact-division answer) plus an IEEE
#     divide gives the identical double, bit for bit.  Outside that range the
#     old exact-rational body stands, in %p-divide-general.
#
# EVERY EXPECTATION BELOW IS PERL 5.40.3's OWN OUTPUT, probed by running the
# same program under perl (scratch/s473r/guard-sem.pl in the s473r worktree),
# with ONE labelled exception: `div-o53`, an accepted PRE-EXISTING divergence
# in the boundless-integer family (#1513), filed as task #1515 — it is here
# because it is the case the fast arm DECLINES, so it is the row that proves
# the decline happens.
use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use lib "$RealBin/../..";
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl   = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);
plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;
plan tests => 59;

sub run_pl {
    my ($src) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $src;
    close $fh;
    my $cl = PCLCore::transpile(qq{$pl2cl $file});
    my ($cfh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cfh $cl;
    close $cfh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^(?:caught |compilation unit|-->|==>|PCL Runtime loaded).*\n//gm;
    return $out;
}

# ─────────────────────────────────────────────────────────────────────────────
# THE MECHANISM — four facts read out of the loaded runtime itself.
# ─────────────────────────────────────────────────────────────────────────────
# These are the rows that FAIL on a pre-#1514 tree: without the lever `p-int`
# has no inline declaim at all and neither cold arm exists.  The `p-+` row is
# the control — it is INLINE on both trees, so a probe that answered `nil` for
# everything would not pass silently.
my ($mfh, $mfile) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
print $mfh <<'LISP';
(format t "inlinep-p-int ~a~%" (sb-int:info :function :inlinep 'pcl::p-int))
(format t "inlinep-p-plus ~a~%" (sb-int:info :function :inlinep 'pcl::p-+))
(format t "slow-arm ~a~%" (and (fboundp 'pcl::%p-int-slow) t))
(format t "divide-general ~a~%" (and (fboundp 'pcl::%p-divide-general) t))
LISP
close $mfh;
my $mech = `sbcl @sbcl_rt --load $mfile 2>&1`;
like($mech, qr/^inlinep-p-int INLINE$/mi,
     'p-int is declaimed INLINE for generated code (it was a full call)');
like($mech, qr/^inlinep-p-plus INLINE$/mi,
     'control: p-+ is inline on every tree, so the probe itself works');
like($mech, qr/^slow-arm T$/mi,
     '%p-int-slow exists: int()\'s cold shapes are one call away, not inline');
like($mech, qr/^divide-general T$/mi,
     '%p-divide-general exists: the exact-rational arm left the inline half');

# ─────────────────────────────────────────────────────────────────────────────
# THE ANSWERS — perl 5.40.3's, for every shape either lever can be handed.
# ─────────────────────────────────────────────────────────────────────────────
my $sem = run_pl(<<'PERL');
printf "int-pos %s\n", int(3.7);
printf "int-neg %s\n", int(-3.7);
printf "int-half %s\n", int(-0.5);
printf "int-int %s\n", int(42);
printf "int-str %s\n", int("3.9xyz");
printf "int-nan-str %s\n", int("abc");
printf "int-undef %s\n", int(undef);
printf "int-spaced %s\n", int("  12  ");
printf "int-butt %s\n", int("0 but true");
printf "int-exp %s\n", int("1e3");
printf "int-ratio %s\n", int(7/2);
printf "int-nratio %s\n", int(-7/2);
printf "int-big %s\n", int(123456789012345678);
printf "int-chain %s\n", int(int(10/3) / 2);
my $inf = 9**9**9;
printf "int-inf %s\n", int($inf);
printf "int-ninf %s\n", int(-$inf);
my $nan = $inf - $inf;
printf "int-nan %s\n", (int($nan) == int($nan) ? "number" : "NaN");
{ package N; use overload '0+' => sub { 7.9 }, fallback => 1; sub new { bless {}, shift } }
printf "int-ovl %s\n", int(N->new);
printf "div-exact %s\n", 10/5;
printf "div-inexact %.17g\n", 8/7;
printf "div-negl %.17g\n", -8/7;
printf "div-negr %.17g\n", 8/-7;
printf "div-negb %.17g\n", -8/-7;
printf "div-third %.17g\n", 1/3;
printf "div-str %s\n", "10"/"4";
printf "div-float %s\n", 3.5/0.5;
printf "div-mixed %.17g\n", 1/3.0;
printf "div-b53 %.17g\n", 4503599627370495/7;
printf "div-o53 %.17g\n", 9007199254740993/7;
printf "div-huge %.17g\n", 123456789012345678901234567890/7;
printf "div-ovl %s\n", (N->new / 2);
my $z = 0;
printf "div-zero %s\n", (eval { my $q = 5/$z; 1 } ? "NO DIE" : "DIED");
printf "div-zerof %s\n", (eval { my $y = 0.0; my $q = 5/$y; 1 } ? "NO DIE" : "DIED");
{
  use integer;
  printf "ui-mul %s\n", 7 * 3;
  printf "ui-div %s\n", -7 / 2;
  printf "ui-divp %s\n", 7 / 2;
  printf "ui-float %s\n", 7.9 + 0.2;
  printf "ui-str %s\n", "3abc" * 2;
  printf "ui-mod %s\n", -7 % 3;
  printf "ui-loop %s\n", do { my $s = 0; for my $i (1..20) { $s = ($s * 3 + $i / 7) % 1000003 } $s };
}
printf "nui-mod %s\n", -7 % 3;
PERL
my %got = map { /^(\S+) (.*)$/ ? ($1 => $2) : () } split /\n/, $sem;

# int(): the INTEGER arm, the DOUBLE-FLOAT arm, and every shape that must fall
# through to %p-int-slow.
my @int_rows = (
    ['int-pos',     '3',                  'int(3.7) truncates toward zero'],
    ['int-neg',     '-3',                 'int(-3.7) truncates toward zero, not floor'],
    ['int-half',    '0',                  'int(-0.5) is 0, not -0'],
    ['int-int',     '42',                 'the INTEGER arm: int() of an integer is itself'],
    ['int-str',     '3',                  'a leading-numeric STRING still goes through %p-int-slow'],
    ['int-nan-str', '0',                  'a non-numeric string is 0'],
    ['int-undef',   '0',                  'undef is 0'],
    ['int-spaced',  '12',                 'leading and trailing space'],
    ['int-butt',    '0',                  '"0 but true"'],
    ['int-exp',     '1000',               'exponent notation in a string'],
    ['int-ratio',   '3',                  'int(7/2) — the DOUBLE-FLOAT arm'],
    ['int-nratio',  '-3',                 'int(-7/2) truncates toward zero'],
    ['int-big',     '123456789012345678', 'a big integer is its own int()'],
    ['int-chain',   '1',                  'int(int(10/3)/2) — both levers in one expression'],
    ['int-inf',     'Inf',                'Inf comes back unchanged (perl 5.36+)'],
    ['int-ninf',    '-Inf',               '-Inf comes back unchanged'],
    ['int-nan',     'NaN',                'NaN comes back unchanged, so it is still not = itself'],
    ['int-ovl',     '7',                  'a blessed operand still reaches its 0+ overload'],
);
is($got{$_->[0]}, $_->[1], "int(): $_->[2]") for @int_rows;

# `/`: the fast arm's exact and inexact halves, both signs, both sides of the
# (signed-byte 53) boundary, the shapes that decline it, and the two fatals.
my @div_rows = (
    ['div-exact',   '2',                   'an EXACT division answers the integer, as perl does'],
    ['div-inexact', '1.1428571428571428',  '8/7 — the fast arm\'s IEEE quotient'],
    ['div-negl',    '-1.1428571428571428', 'a negative dividend'],
    ['div-negr',    '-1.1428571428571428', 'a negative divisor'],
    ['div-negb',    '1.1428571428571428',  'both negative'],
    ['div-third',   '0.33333333333333331', '1/3 to the last bit'],
    ['div-str',     '2.5',                 'STRING operands: numberp fails, %p-/-slow coerces'],
    ['div-float',   '7',                   'two floats decline the integer arm'],
    ['div-mixed',   '0.33333333333333331', 'one integer, one float'],
    ['div-b53',     '643371375338642.12',  'just INSIDE (signed-byte 53): the fast arm'],
    ['div-huge',    '1.763668414462081e+28','a bignum operand: %p-divide-general'],
    ['div-ovl',     '3.95',                'a blessed operand still reaches its overload'],
    ['div-zero',    'DIED',                'an integer zero divisor is still fatal'],
    ['div-zerof',   'DIED',                'a float zero divisor is still fatal'],
);
is($got{$_->[0]}, $_->[1], "/: $_->[2]") for @div_rows;

# THE DECLINE, and an accepted divergence.  9007199254740993 is 2**53+1, one
# past the fast arm's type, so this row takes %p-divide-general — and that arm
# rounds the EXACT rational where perl rounds the two operands to doubles
# FIRST and divides those, so perl answers 1286742750677284.5.  PCL has
# answered 1286742750677284.8 since long before this lever (verified on a
# bfa11c72 extraction); it is the boundless-integer model (#1513) and is filed
# as task #1515.  Assert PCL's answer so the DECLINE is guarded: if the fast
# arm ever widened past 2**53 this row would move to perl's number.
is($got{'div-o53'}, '1286742750677284.8',
   '/: 2**53+1 DECLINES the fast arm and keeps the exact-rational answer (#1515: perl says .5)');

# `use integer` — the pragma wraps EVERY operand in int(), so it is p-int's
# heaviest user; these are perl's answers and none of them may move.
my @ui_rows = (
    ['ui-mul',   '21',     'a plain product'],
    ['ui-div',   '-3',     '-7/2 truncates toward zero under the pragma'],
    ['ui-divp',  '3',      '7/2 truncates'],
    ['ui-float', '7',      'float operands are truncated before the op'],
    ['ui-str',   '6',      'a leading-numeric string is still 3'],
    ['ui-mod',   '-1',     '% under the pragma follows the LEFT operand\'s sign (C, not perl)'],
    ['ui-loop',  '392571', 'the #1514 loop body, 20 iterations'],
);
is($got{$_->[0]}, $_->[1], "use integer: $_->[2]") for @ui_rows;
is($got{'nui-mod'}, '2',
   'use integer is SCOPED: outside the block, -7 % 3 is perl\'s 2 again');

# ─────────────────────────────────────────────────────────────────────────────
# THE PACKAGE-PREAMBLE GUARD (task #1189) — `p-defpackage` runs CL's
# `defpackage` only when the package is not already there with `(:use :cl
# :pcl)`.  Every emitted program, INCLUDING the one a string eval produces,
# opens with the preamble for each package it mentions, so a program whose
# modules generate code at run time re-runs it: `moo-objs` does it 12 times
# per loop iteration, and `defpackage` on an existing package walks every
# package in the image under a system mutex.
# ─────────────────────────────────────────────────────────────────────────────
my ($pfh, $pfile) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
print $pfh <<'LISP';
(format t "ready-fn ~a~%" (and (fboundp 'pcl::%p-package-ready-p) t))
(format t "guarded ~a~%"
        (and (search "package-ready-p"
                     (string-downcase
                      (with-output-to-string (s)
                        (write (macroexpand-1 (quote (pcl::p-defpackage :some-new-pkg)))
                               :stream s))))
             t))
(pcl::p-defpackage :s473r-made)
(format t "ready-pcl ~a~%" (pcl::%p-package-ready-p "S473R-MADE"))
(format t "ready-absent ~a~%" (pcl::%p-package-ready-p "NO-SUCH-PACKAGE-XYZ"))
(make-package "S473R-BARE" :use '())
(format t "ready-bare ~a~%" (pcl::%p-package-ready-p "S473R-BARE"))
LISP
close $pfh;
my $pk = `sbcl @sbcl_rt --load $pfile 2>&1`;
like($pk, qr/^ready-fn T$/mi,
     '%p-package-ready-p exists (the guard the preamble consults)');
like($pk, qr/^guarded T$/mi,
     'p-defpackage expands to the guarded form, not a bare defpackage');
like($pk, qr/^ready-pcl T$/mi,
     'a package p-defpackage just made answers READY: its preamble re-run is a no-op');
like($pk, qr/^ready-absent NIL$/mi,
     'NEGATIVE: an absent package is NOT ready — the defpackage must run');
like($pk, qr/^ready-bare NIL$/mi,
     'NEGATIVE: a package that exists WITHOUT :use pcl is NOT ready (a perl '
   . '`package` naming an existing CL package must still get the use-list)');

# The perl-level answers: re-opened packages, string evals into an existing
# package, an @ISA set inside an eval, and repeated evals into one package —
# perl 5.40.3's own output (scratch/s473r/probe-pkg.pl in the s473r worktree).
my $pkg = run_pl(<<'PERL');
package Foo;
our @ISA = ();
sub hi { "hi-" . __PACKAGE__ }
package main;
print "a ", Foo::hi(), "\n";
my $ok = eval 'package Foo; sub bye { "bye-" . __PACKAGE__ } 1';
print "b ", ($ok ? "evalok" : "evalfail:$@"), "\n";
print "c ", Foo::bye(), "\n";
package Foo;
sub again { "again" }
package main;
print "d ", Foo::again(), "\n";
print "e ", scalar(@Foo::ISA), "\n";
package Base; sub new { bless {}, shift } sub who { "base" }
package main;
eval 'package Kid; our @ISA = ("Base"); sub who { "kid" } 1' or print "evalerr $@\n";
my $k = Kid->new;
print "f ", $k->who, "\n";
print "g ", Base->new->who, "\n";
eval 'package Late; sub x { 42 } 1' or print "evalerr2 $@\n";
print "h ", Late::x(), "\n";
for my $i (1..3) { eval "package Rep; sub s$i { $i } 1" or die $@ }
print "i ", Rep::s1() + Rep::s2() + Rep::s3(), "\n";
PERL
my %pg = map { /^(\S+) (.*)$/ ? ($1 => $2) : () } split /\n/, $pkg;
my @pkg_rows = (
    ['a', 'hi-Foo',  'a package defined once still answers its own name'],
    ['b', 'evalok',  'a string eval that RE-OPENS an existing package compiles'],
    ['c', 'bye-Foo', 'the sub it defined is callable, in the right package'],
    ['d', 'again',   'a second `package Foo;` section in the file still adds subs'],
    ['e', '0',       '@ISA survives the re-run (it is ensured OUTSIDE the guard)'],
    ['f', 'kid',     'a class whose @ISA is set INSIDE an eval dispatches to itself'],
    ['g', 'base',    '…and its parent still answers for its own instances'],
    ['h', '42',      'a package that exists ONLY inside a string eval is created'],
    ['i', '6',       'three evals into ONE package — the moo-objs shape — all land'],
);
is($pg{$_->[0]}, $_->[1], "package preamble: $_->[2]") for @pkg_rows;
