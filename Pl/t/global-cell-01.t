#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# global-cell-01.t — the direction-D runtime pair: p-defcell (declare an
# ORDINARY package global as a symbol-macro cell) and p-local-cell (localize
# one).  Task #289, plan docs/direction-d-plan.md; the partition that decides
# WHICH names go through them is Pl::GlobalPartition (global-partition-01.t).
#
# These are the two macros the flip emits, so their contract is pinned here
# rather than checked ad hoc: every row below is a property that, if it broke,
# would break quietly.  The one that nearly went wrong when p-defcell was
# written: DEFINE-ONCE.  Several sections forward-declare the same name, and a
# module can load twice — an unconditional (setf symbol-global-value) would
# WIPE a value an earlier section had already assigned.  Row B is that guard.
#
# Raw CL against the loaded runtime: no transpile, one SBCL spawn.
use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $runtime = "$RealBin/../../cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;
plan tests => 8;

my $LISP = <<'LISP';
(defpackage :usr (:use :cl :pcl))
(in-package :usr)
(p-defcell |$g|   (make-p-box nil))
(p-defcell |@arr| (make-array 0 :adjustable t :fill-pointer 0))
(p-scalar-= |$g| "one")
(format t "A ~a~%" (to-string |$g|))
;; A SECOND declaration (another section, or a module loaded twice) must be a
;; no-op, exactly as defvar is — never a clobber of the assigned value.
(p-defcell |$g| (make-p-box "CLOBBERED"))
(format t "B ~a~%" (to-string |$g|))
(format t "C ~a~%" (p-local-cell |$g| (p-box-for-local "inner") (to-string |$g|)))
(format t "D ~a~%" (to-string |$g|))
(handler-case (p-local-cell |$g| (p-box-for-local "dying") (error "boom"))
  (error () (format t "E ~a~%" (to-string |$g|))))
;; name-based access (the glob / symbolic-ref / eval-free-name helpers all go
;; through these) must reach the SAME cell the symbol macro reads
(format t "F ~a~%" (to-string (symbol-value '|$g|)))
(setf (symbol-value '|$g|) (make-p-box "via-name"))
(format t "G ~a~%" (to-string |$g|))
;; a container cell initializes to its own fresh container
(format t "H ~a~%" (length |@arr|))
LISP

my ($fh, $file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
print $fh $LISP;
close $fh;
my $out = `sbcl @sbcl_rt --load $file 2>&1`;
my %got = map { /^([A-H]) (.*)$/ ? ($1 => $2) : () } split /\n/, $out;

is($got{A}, 'one',      'p-defcell: cell reads and writes through the symbol macro');
is($got{B}, 'one',      'p-defcell is DEFINE-ONCE — a second declaration does not clobber');
is($got{C}, 'inner',    'p-local-cell: the body sees the installed value');
is($got{D}, 'one',      'p-local-cell restores on normal exit');
is($got{E}, 'one',      'p-local-cell restores through a die (unwind-protect)');
is($got{F}, 'one',      'symbol-value reads the same cell as the symbol macro');
is($got{G}, 'via-name', 'a name-based write is visible through the symbol macro');
is($got{H}, '0',        'a container cell initializes to its own fresh container');
diag("sbcl output:\n$out") if grep { !defined $got{$_} } qw(A B C D E F G H);
