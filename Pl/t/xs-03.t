#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# XS OO via ext-magic — the ABI-6 `magic` capability group (task #115).
#
# Digest::MD5-class modules keep their C state as PERL_MAGIC_ext magic on
# the REFERENT of a blessed scalar ref:
#
#     SV *sv  = newSV(0);                 /* referent, stays undef      */
#     SV *obj = newRV_noinc(sv);          /* the object the user holds  */
#     sv_bless(obj, ...);
#     sv_magicext(sv, NULL, PERL_MAGIC_ext, &vtbl, (char*)state, 0);
#     ...later, in a method:
#     for (mg = SvMAGIC(SvRV(self)); mg; ...)  /* find state again      */
#
# Two host-side properties carry this, and this file pins both:
#
#   1. magic_set/magic_get key on the referent BOX's identity, and the
#      word is never copied on assignment — two objects never share one
#      C struct, and a copied $ref still reaches the same state.
#   2. xs-ref-target returns the REFERENT box (one unwrap past the is-ref
#      wrapper p-backslash builds) — SvRV identity is what magic (and
#      blessing, task #99) key on, and what a write-through must reach.
#
# Self-contained like xs-02.t: builds a fixture dist with Digest::MD5's
# exact shape, installs into a temp cache, and drives a TRANSPILED Perl
# program end to end.

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
my $dist  = "$tmp/Magik";
mkdir $dist or die $!;

open my $xs, '>', "$dist/Magik.xs" or die $!;
print {$xs} <<'XS';
#define PERL_NO_GET_CONTEXT
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include <stdlib.h>

#ifndef PERL_MAGIC_ext
# define PERL_MAGIC_ext '~'
#endif

/* Identity tag, Digest::MD5's exact fallback shape: the shim's MGVTBL is
 * opaque, so declare an own struct and cast -- MD5.xs does the same when
 * MGf_DUP is absent. */
static const struct {
    int (*svt_get)(SV *sv, MAGIC *mg);
    int (*svt_set)(SV *sv, MAGIC *mg);
    U32 (*svt_len)(SV *sv, MAGIC *mg);
    int (*svt_clear)(SV *sv, MAGIC *mg);
    int (*svt_free)(SV *sv, MAGIC *mg);
} vtbl_magik = { NULL, NULL, NULL, NULL, NULL };

static IV *get_state(pTHX_ SV *self)
{
    MAGIC *mg;
    for (mg = SvMAGIC(SvRV(self)); mg; mg = mg->mg_moremagic) {
        if (mg->mg_type == PERL_MAGIC_ext
            && mg->mg_virtual == (const MGVTBL *)&vtbl_magik)
            return (IV *)mg->mg_ptr;
    }
    croak("Failed to get Magik state pointer");
    return (IV *)0;
}

MODULE = Magik    PACKAGE = Magik

PROTOTYPES: DISABLE

SV *
new(klass, iv)
    char *klass
    IV iv
  CODE:
    {
        SV *sv  = newSV(0);
        SV *obj = newRV_noinc(sv);
        IV *state = (IV *)malloc(sizeof(IV));
        *state = iv;
        sv_bless(obj, gv_stashpv(klass, GV_ADD));
        sv_magicext(sv, NULL, PERL_MAGIC_ext, (const MGVTBL *)&vtbl_magik,
                    (char *)state, 0);
        RETVAL = obj;
    }
  OUTPUT:
    RETVAL

void
bump(self)
    SV *self
  CODE:
    (*get_state(aTHX_ self))++;

IV
value(self)
    SV *self
  CODE:
    RETVAL = *get_state(aTHX_ self);
  OUTPUT:
    RETVAL
XS
close $xs;

my $log = qx{PCL_XS_CACHE=$cache $^X $pcl_root/tools/pcl-xs-install $dist 2>&1};
my ($abi) = do {
    open my $p, '<', "$pcl_root/xs-pin" or die $!;
    my $n; while (<$p>) { $n = $1, last if /^abi\s+(\d+)/ } $n;
};
ok(-f "$cache/abi-$abi/auto/Magik/Magik.so", 'fixture dist built and cached')
    or diag($log);

# The Perl side of the module, found via "." on @INC at require time.
mkdir "$tmp/run" or die $!;
open my $pm, '>', "$tmp/run/Magik.pm" or die $!;
print {$pm} "package Magik;\nrequire XSLoader;\nXSLoader::load('Magik');\n1;\n";
close $pm;

open my $pl, '>', "$tmp/run/t.pl" or die $!;
print {$pl} <<'PL';
use Magik;
my $a = Magik->new(5);
my $b = Magik->new(40);
$a->bump; $a->bump;
$b->bump;
print "A=", $a->value, " B=", $b->value, "\n";
my $c = $a;          # copying the REF reaches the same object...
$c->bump;
print "SHARED=", $a->value, "\n";
my $chain = Magik->new(1);
$chain->bump;
print "CHAIN=", $chain->value, "\n";
PL
close $pl;

my $tr = qx{cd $tmp/run && $pcl_root/pl2cl < t.pl > t.lisp 2>&1};
my $run = "cd $tmp/run && PCL_XS_CACHE=$cache sbcl --noinform --disable-debugger"
        . " --load $pcl_root/cl/pcl-runtime.lisp"
        . " --eval '(setf pcl::*pcl-skip-cache* t)'"
        . " --load t.lisp --quit 2>&1";
my $got = qx{$run};

like($got, qr/^A=7 B=41$/m,
     'two objects hold separate C state (magic word never shared)')
    or diag($got);
like($got, qr/^SHARED=8$/m,
     'a copied reference reaches the SAME state (magic keys the referent, not the value)');
like($got, qr/^CHAIN=2$/m, 'state survives across separate method calls');
diag($got) if $ENV{PCL_XS_VERBOSE};
