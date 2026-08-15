#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# The XS bridge: a CPAN distribution's C, running inside PCL.
#
# The chain this gates, end to end:
#
#     Foo.xs --xsubpp--> Foo.c --cc -I pclxs/include--> Foo.so
#                                                          |
#     PCL sub call <-- cl/pcl-xs.lisp <-- vtable <-- libpclxs
#
# Everything except the last two steps lives in the pclxs repo (a sibling
# checkout, pinned by xs-pin) and is tested there.  What is tested HERE is
# the part only PCL can test: that a shim-built XSUB becomes an ordinary
# PCL sub, with PCL's calling convention, PCL's coercions, and PCL's die.
#
# Skips cleanly when the sibling checkout is absent or unbuilt -- the
# bridge is optional, and nothing else in PCL depends on it.

use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use FindBin;

my $pcl_root = "$FindBin::Bin/../..";
my $pclxs    = $ENV{PCLXS_DIR} || "$pcl_root/../pclxs";

if (! -d $pclxs) {
    plan skip_all => "no pclxs checkout at $pclxs (set PCLXS_DIR)";
}
if (! -f "$pclxs/build/libpclxs.so") {
    plan skip_all => "libpclxs not built (run: tools/build-pclxs)";
}
plan tests => 6;

# Build a small module through the real pipeline -- xsubpp and all.
my $dist = tempdir(CLEANUP => 1);
open my $xs, '>', "$dist/Arith.xs" or die $!;
print {$xs} <<'XS';
#define PERL_NO_GET_CONTEXT
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

MODULE = Arith    PACKAGE = Arith

PROTOTYPES: DISABLE

IV
add(a, b)
    IV a
    IV b
  CODE:
    RETVAL = a + b;
  OUTPUT:
    RETVAL

SV *
concat(x, y)
    SV *x
    SV *y
  PREINIT:
    STRLEN xl, yl;
    const char *xp, *yp;
  CODE:
    xp = SvPV(x, xl);
    yp = SvPV(y, yl);
    RETVAL = newSVpvn(xp, xl);
    sv_catpvn(RETVAL, yp, yl);
  OUTPUT:
    RETVAL

void
triple(n)
    IV n
  PPCODE:
    EXTEND(SP, 3);
    mPUSHi(n);
    mPUSHi(n + 1);
    mPUSHi(n + 2);

void
boom()
  CODE:
    croak("Arith::boom went off");
XS
close $xs;

my $out = "$dist/out";
my $log = `$^X $pclxs/tools/xs-build --out $out --suffix .so $dist 2>&1`;
my ($so) = $log =~ /^built:\s*(\S+)/m;
ok($so && -f $so, 'xs-build produced a loadable object') or diag($log);
BAIL_OUT('nothing to load') if ! $so;

# Drive it from SBCL exactly as generated PCL code would.
my $script = "$dist/drive.lisp";
open my $lisp, '>', $script or die $!;
print {$lisp} <<"LISP";
(require :sb-posix)
(load "$pcl_root/cl/pcl-runtime.lisp")
(in-package :pcl)
(load "$pcl_root/cl/pcl-xs.lisp")
(p-xs-boot "$so" "boot_Arith")
(let* ((pkg (%pcl-find-package "Arith"))
       (add (find-symbol (%pcl-cl-sub-name "add") pkg))
       (cat (find-symbol (%pcl-cl-sub-name "concat") pkg))
       (tri (find-symbol (%pcl-cl-sub-name "triple") pkg))
       (bm  (find-symbol (%pcl-cl-sub-name "boom") pkg)))
  (format t "~&ADD=~A~%" (let ((*wantarray* nil)) (funcall add 2 3)))
  (format t "COERCE=~A~%" (let ((*wantarray* nil)) (funcall add "40" "2")))
  (format t "CAT=~A~%" (let ((*wantarray* nil)) (funcall cat "ca" "fe")))
  (format t "LIST=~{~A~^,~}~%"
          (let ((*wantarray* t)) (map 'list #'unbox (funcall tri 7))))
  (format t "DIE=~A~%"
          (handler-case (progn (funcall bm) "NOT REACHED")
            (error (e) (princ-to-string e)))))
LISP
close $lisp;

my $got = `sbcl --script $script 2>&1`;

like($got, qr/^ADD=5$/m,      'an XSUB is callable as an ordinary PCL sub');
like($got, qr/^COERCE=42$/m,  "string arguments coerce through PCL's own to-number");
like($got, qr/^CAT=cafe$/m,   'byte strings cross the boundary intact');
like($got, qr/^LIST=7,8,9$/m, 'list context returns a PCL array');
like($got, qr/^DIE=Arith::boom went off$/m,
     'croak in C becomes a PCL die, catchable where any other die would be');
diag($got) if $ENV{PCL_XS_VERBOSE};
