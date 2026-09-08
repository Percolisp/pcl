#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# defclass-01.t — `p-defclass`, the guarded form of the CLOS class that carries
# a Perl package's MRO (task #1518, the other half of #1189's finding; the
# `p-defpackage` guard shipped in s473r is the precedent).
#
# `p-defclass` skips the `defclass` when the class is already there IN THAT
# EXACT SHAPE.  It was filed (#1518) expecting the `p-defpackage` half's prize
# — "12 `ensure-class` per `moo-objs` iteration" — and s473s MEASURED that
# claim false: the preamble a string `eval`'s program carries declares its
# PACKAGE and not its class, so `moo-objs` runs 12 package-readiness tests per
# iteration and exactly 20 class ones IN TOTAL, N-independent, none of them a
# hit.  The change is kept for the emission (no bare host `defclass` in the
# IR) and for correctness if a path ever does re-open one.  These rows are
# therefore about the GUARD BEING RIGHT, not about it being fast.
#
# The word EXACT is the whole safety argument, and it is why this file exists:
# a guard keyed on mere EXISTENCE would freeze a package's parents at whatever
# the first `@ISA` said, so a second `package K; our @ISA = ('PB')` would keep
# dispatching to PA — silently, which is the worst failure mode here.  Rows 2/3
# and 5 are that claim; rows 8/9 are the forward-referenced-parent case (the
# comparison is by class NAME, so it gives the same answer before and after the
# parent class arrives); row 10 is rule 12 — a form carrying SLOTS is a shape
# the readiness test cannot answer, so it dies rather than guessing.
#
# Section B runs the same claims from PERL, through string eval, and every
# expected line is real perl 5.40.3's output for that program.
#
# Inverse guard: on a tree without the change `p-defclass` is not a macro at
# all, so section A dies at load and section C sees a bare `(defclass`.
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

plan skip_all => "pl2cl not found" if ! -x $pl2cl;
plan skip_all => "sbcl not found"  if ! `which sbcl 2>/dev/null`;
plan tests => 12;

# -- A. the macro and its readiness test, in raw CL (one SBCL spawn) --------
my $LISP = <<'LISP';
(in-package :pcl)
(p-defclass plc-a () ())
(p-defclass plc-b () ())
(p-defclass plc-x (plc-a) ())
(format t "1 ~A~%" (mapcar #'class-name (sb-mop:class-direct-superclasses (find-class 'plc-x))))
(format t "2 ~A~%" (%p-class-ready-p 'plc-x '(plc-a)))
(format t "3 ~A~%" (%p-class-ready-p 'plc-x '(plc-b)))
;; a re-open with the SAME parent must not rebuild: the class OBJECT stays eq
(let ((before (find-class 'plc-x)))
  (p-defclass plc-x (plc-a) ())
  (format t "4 ~A~%" (eq before (find-class 'plc-x))))
;; a re-open with a DIFFERENT parent must rebuild the parent list
(p-defclass plc-x (plc-b) ())
(format t "5 ~A~%" (mapcar #'class-name (sb-mop:class-direct-superclasses (find-class 'plc-x))))
;; a bare class reads as ready against () — CLOS records STANDARD-OBJECT as the
;; direct superclass of (defclass foo () ()), so the two spellings have to be
;; compared in one vocabulary or a bare class would never look ready
(p-defclass plc-c () ())
(format t "6 ~A ~A~%" (%p-class-ready-p 'plc-c '()) (%p-class-ready-p 'plc-c '(plc-a)))
(format t "7 ~A~%" (%p-class-ready-p 'plc-never-defined '()))
;; a forward-referenced parent compares by NAME, before and after it arrives
(p-defclass plc-fwd (plc-later) ())
(format t "8 ~A~%" (%p-class-ready-p 'plc-fwd '(plc-later)))
(p-defclass plc-later () ())
(format t "9 ~A~%" (%p-class-ready-p 'plc-fwd '(plc-later)))
;; slots are refused, loudly (rule 12)
(format t "10 ~A~%"
        (handler-case (progn (eval '(p-defclass plc-s () ((a :initform 1)))) "no-error")
          (error (e) (if (search "slots are not part" (princ-to-string e)) "refused" "other"))))
LISP

my ($lfh, $lfile) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
print $lfh $LISP;
close $lfh;
my $lout = `sbcl @sbcl_rt --load $lfile 2>&1`;
my %got = map { /^(\d+) (.*)$/ ? ($1 => $2) : () } split /\n/, $lout;
diag("raw CL output was:\n$lout") if ! defined $got{10};

is($got{1}, '(plc-a)',  'p-defclass defines the class with the parents it is given');
is($got{2}, 't',        'ready: same class, same direct superclass');
is($got{3}, 'nil',      'NOT ready when the direct superclass differs — the parents are compared, not just existence');
is($got{4}, 't',        'a re-open with the same parents does not rebuild the class (object stays EQ)');
is($got{5}, '(plc-b)',  'a re-open with DIFFERENT parents does rebuild');
is($got{6}, 't nil',    'a parentless class is ready against () and not against a parent');
is($got{7}, 'nil',      'a class that does not exist is never ready');
is($got{8}, 't',        'a forward-referenced parent compares by NAME before the parent arrives');
is($got{9}, 't',        '... and gives the same answer after it arrives');
is($got{10}, 'refused', 'a form carrying SLOTS dies: the readiness test cannot answer for it (rule 12)');

# -- B. the same claims from Perl, via string eval -------------------------
# The shape #1518 is about: a package re-opened by generated code.  Every
# expected line is real perl 5.40.3's output for this program.
sub run_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = PCLCore::transpile(qq{$pl2cl $pl_file});
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

my $prog = <<'EOF';
use strict; use warnings;
package PA; sub new { bless {}, $_[0] } sub who { "PA" }
package PB; sub new { bless {}, $_[0] } sub who { "PB" }
package main;

# a class defined, then RE-OPENED in a string eval with the SAME parent
eval 'package K1; our @ISA = ("PA"); 1' or die $@;
print "1 ", K1->new->who, "\n";
eval 'package K1; our @ISA = ("PA"); sub extra { "K1::extra" } 1' or die $@;
print "2 ", K1->new->who, " ", K1->new->extra, "\n";

# the same package REDEFINED with a DIFFERENT parent: the class is rebuilt,
# not frozen at the first parent
eval 'package K2; our @ISA = ("PA"); 1' or die $@;
print "3 ", K2->new->who, "\n";
eval 'package K2; our @ISA = ("PB"); 1' or die $@;
print "4 ", K2->new->who, "\n";

# a deeper chain built in two evals
eval 'package M1; our @ISA = ("PA"); 1' or die $@;
eval 'package M2; our @ISA = ("M1"); 1' or die $@;
print "5 ", M2->new->who, "\n";
print "6 ", (M2->isa("PA") ? "isa-PA" : "no"), " ", (M2->isa("M1") ? "isa-M1" : "no"), "\n";

# a package re-opened in the SAME file (not an eval)
package R1; our @ISA = ('PA'); sub r { "R1::r" }
package main;
print "7 ", R1->new->who, " ", R1->new->r, "\n";
package R1; sub r2 { "R1::r2" }
package main;
print "8 ", R1->new->who, " ", R1->new->r2, "\n";

# @ISA changed at run time after the class exists
@K1::ISA = ('PB');
print "9 ", K1->new->who, "\n";
EOF
is(run_pl($prog),
   "1 PA\n2 PA K1::extra\n3 PA\n4 PB\n5 PA\n6 isa-PA isa-M1\n"
   . "7 PA R1::r\n8 PA R1::r2\n9 PB\n",
   'a package re-opened by generated code keeps the parents its @ISA names');

# -- C. the emission: every defclass site is guarded -----------------------
# pl2cl writes the class form at seven sites (Pl/Parser.pm x6, Pl/Parser2.pm
# x1).  A bare `(defclass` left anywhere in an emitted program is one of them
# unconverted — the inverse guard, and the thing that would quietly put the
# cost back.
{
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh <<'EOF';
package Animal; sub new { bless {}, shift } sub speak { "..." }
package Dog; our @ISA = ('Animal'); sub speak { "woof" }
package main;
print Dog->new->speak, "\n";
EOF
    close $fh;
    my $cl = PCLCore::transpile(qq{$pl2cl $pl_file});
    my $guarded = () = $cl =~ /\(p-defclass /g;
    my $bare    = () = $cl =~ /\(defclass /g;
    ok($guarded >= 2 && $bare == 0,
       "every emitted class form is a p-defclass (guarded $guarded, bare $bare)");
}
