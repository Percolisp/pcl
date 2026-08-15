#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# XSLoader::load finds a cached artifact — the other half of the XS bridge.
#
# Pl/t/xs-01.t asks "does an XSUB work once we boot it by hand".  This asks
# the question a *user* asks: `use Foo;` reaches XSLoader::load, and there
# has to be something that turns a module NAME into a loadable object
# without anybody passing a path.
#
# The two properties that matter, and both are here:
#
#   1. A module whose artifact is in the cache boots and runs.
#   2. A module whose artifact is NOT there fails with perl's exact
#      message.  That message is load-bearing: every dual-life module on
#      CPAN (Data::Dumper, Time::HiRes, ...) is written as
#      `eval { require XSLoader; XSLoader::load(...); 1 } or $Useperl = 1;`
#      and falls back to pure Perl on precisely that failure.  A friendlier
#      error here would break more modules than it helped.
#
# See docs/xs-artifact-cache.md for why the cache is keyed on the pclxs ABI
# and what would make us change that.

use strict;
use warnings;
use Test::More;
use Cwd qw(abs_path);
use File::Basename qw(dirname);
use File::Temp qw(tempdir);

my $pcl_root = abs_path(dirname(abs_path($0)) . '/../..');
my $pclxs    = $ENV{PCLXS_DIR} || abs_path("$pcl_root/../pclxs");

plan skip_all => "no pclxs checkout at $pclxs (set PCLXS_DIR)" if ! -d $pclxs;
plan skip_all => 'libpclxs not built (run: tools/build-pclxs)'
    if ! -f "$pclxs/build/libpclxs.so";
plan skip_all => 'sbcl not found' if system('command -v sbcl >/dev/null 2>&1');

plan tests => 4;

my $tmp   = tempdir(CLEANUP => 1);
my $cache = "$tmp/xscache";
my $dist  = "$tmp/Cached";
mkdir $dist or die $!;

open my $xs, '>', "$dist/Cached.xs" or die $!;
print {$xs} <<'XS';
#define PERL_NO_GET_CONTEXT
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

MODULE = Cached    PACKAGE = Cached

PROTOTYPES: DISABLE

IV
twice(n)
    IV n
  CODE:
    RETVAL = n * 2;
  OUTPUT:
    RETVAL
XS
close $xs;

my $log = qx{PCL_XS_CACHE=$cache $^X $pcl_root/tools/pcl-xs-install $dist 2>&1};
my ($abi) = do {
    open my $p, '<', "$pcl_root/xs-pin" or die $!;
    my $n; while (<$p>) { $n = $1, last if /^abi\s+(\d+)/ } $n;
};
ok(-f "$cache/abi-$abi/auto/Cached/Cached.so",
   "pcl-xs-install put the artifact at the ABI-keyed path the loader derives")
    or diag($log);

# Drive it the way a transpiled `use Cached;` eventually will: no paths,
# just the module name.
my $script = "$tmp/drive.lisp";
open my $l, '>', $script or die $!;
print {$l} <<"LISP";
(require :sb-posix)
(load "$pcl_root/cl/pcl-runtime.lisp")
(in-package :pcl)
(format t "~&LOADED=~A~%" (XSLoader::pl-load "Cached"))
(let* ((pkg (%pcl-find-package "Cached"))
       (f   (find-symbol (%pcl-cl-sub-name "twice") pkg)))
  (format t "TWICE=~A~%" (let ((*wantarray* nil)) (funcall f 21))))
(format t "MISSING=~A~%"
        (handler-case (XSLoader::pl-load "No::Such::Module")
          (error (e) (princ-to-string e))))
LISP
close $l;

my $got = qx{PCL_XS_CACHE=$cache sbcl --script $script 2>&1};

like($got, qr/^LOADED=1$/m,
     'XSLoader::load finds the cached artifact from the module name alone')
    or diag($got);
like($got, qr/^TWICE=42$/m, '...and the XSUB it booted actually runs');
like($got, qr/^MISSING=Can't locate loadable object for module No::Such::Module in \@INC/m,
     "a module with no artifact still fails exactly as perl does "
   . "(dual-life modules depend on this)");
diag($got) if $ENV{PCL_XS_VERBOSE};
