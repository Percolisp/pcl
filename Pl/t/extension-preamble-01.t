#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# extension-preamble-01.t — a checked-in EXTENSION artifact carries no PROGRAM
# preamble, and loading one may not touch the running program's state
# (tasks #349 and #217).
#
# cl/pcl-pack.lisp, cl/pcl-mro.lisp and cl/pcl-warnings.lisp are PCL output
# checked into the tree, `load`ed lazily by p-load-extension at the first
# pack/unpack/mro::*/warnings::enabled call — INTO a program that already has
# its own preamble.  They used to carry one anyway, so that first pack() call
# REPLACED the running program's @INC with the build machine's list:
#
#     push @INC, "/tmp/mylib"; pack("N", 42);
#     print scalar grep { $_ eq "/tmp/mylib" } @INC;    perl: 1   PCL: 0
#
# — silent until a later `require` could not find a module the program had just
# put on @INC.  It is also why the artifacts embedded this machine's absolute
# paths at all (#217): the preamble is where they came from.
#
# `pl2cl --extension` emits no program preamble; this file guards both halves —
# the artifacts stay clean, and the runtime REFUSES a future one that is not
# (rule 12: the load dies naming the extension instead of quietly editing @INC).

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $root    = "$RealBin/../..";
my $pl2cl   = "$root/pl2cl";
my $runtime = "$root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;
plan tests => 9;

# The three program-preamble forms an extension must never carry.  Each is
# global state the HOST program owns: its module search path, the transpiler
# it shells out to for string eval, and the built-in-module dirs.
my $PREAMBLE_RX = qr{\(setf \s+ pcl::\@INC | \*pcl-pl2cl-path\* | \*p-core-inc-dirs\*}x;

# --- 1. the flag itself ---------------------------------------------------
my $tmp = tempdir(CLEANUP => 1);
open my $mfh, '>', "$tmp/Ext.pm" or die "write $tmp/Ext.pm: $!";
print $mfh "package Ext;\nsub hi { 42 }\n1;\n";
close $mfh;

my $ext_cl = `$pl2cl --extension $tmp/Ext.pm 2>/dev/null`;
unlike($ext_cl, $PREAMBLE_RX,
       'pl2cl --extension emits no program preamble (@INC / pl2cl-path / core-inc-dirs)');
like($ext_cl, qr{^;;;\s*pcl:\s*pipeline=\S+\s+gen=\S+}m,
     'pl2cl --extension keeps the line-1 gen stamp (docs/ir-spec.md §9.2)');

# --- 2. the checked-in artifacts -----------------------------------------
# Discovered BY THE STAMP, never by a list — the same rule
# artifact-staleness-01.t and no-hardcoded-paths-01.t use, and the reason s399
# found the third artifact nobody had listed.
my @artifacts;
for my $f (sort glob "$root/cl/*.lisp") {
  open my $fh, '<:raw', $f or next;
  my $first = <$fh>;
  close $fh;
  push @artifacts, $f if defined $first && $first =~ /^;;;\s*pcl:\s*pipeline=\S+\s+gen=\S+/;
}
my @dirty = grep {
  open my $fh, '<:raw', $_ or die "cannot read $_: $!";
  my $src = do { local $/; <$fh> };
  close $fh;
  $src =~ $PREAMBLE_RX;
} @artifacts;
is_deeply(\@dirty, [],
          'no checked-in cl/*.lisp artifact carries a program preamble')
  or diag("regenerate with pl2cl --extension: @dirty");

# --- 3. the behaviour that was silently wrong ----------------------------
# The probe from the task, verbatim: a runtime `push @INC` must survive the
# first pack().  perl prints 1.
my ($plfh, $plfile) = tempfile(SUFFIX => '.pl', UNLINK => 1);
print $plfh <<'PL';
push @INC, "/tmp/pcl-ext-guard";
my $p = pack("N", 42);
print scalar(grep { $_ eq "/tmp/pcl-ext-guard" } @INC), "\n";
PL
close $plfh;
my $cl = `$pl2cl $plfile 2>/dev/null`;
my ($clfh, $clfile) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
print $clfh $cl;
close $clfh;
my $out = `sbcl @sbcl_rt --load $clfile 2>&1`;
$out =~ s/^;.*\n//gm;
$out =~ s/^PCL Runtime loaded\n//gm;
like($out, qr/^1$/m,
     'a runtime `push @INC` survives the first pack() (the extension load leaves @INC alone)');

# --- 4. rule 12: a future dirty extension DIES, it does not edit @INC -----
my $extdir = tempdir(CLEANUP => 1);
open my $efh, '>', "$extdir/pcl-badext.lisp" or die "write pcl-badext.lisp: $!";
print $efh qq{(in-package :pcl)\n(vector-push-extend "/tmp/pcl-badext-dir" \@INC)\n};
close $efh;
my $lisp = qq{(handler-case }
         . qq{(let ((pcl::*pcl-runtime-directory* #P"$extdir/")) }
         . qq{(pcl::p-load-extension "pcl-badext")) }
         . qq{(error (e) (format t "DIED: ~a~%" e)))};
my ($gfh, $gfile) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
print $gfh $lisp;
close $gfh;
my $guard_out = `sbcl @sbcl_rt --load $gfile 2>&1`;
like($guard_out, qr/DIED:.*pcl-badext/s,
     'p-load-extension dies naming an extension that changes @INC (rule 12)');

# --- 5. the STUB DELEGATION invariant (task #980) -------------------------
# A self-loading stub gets exactly ONE chance: p-load-extension answers T for
# the load that actually happened and NIL for every later call, so the
# delegation must reach the definition the extension file just installed OVER
# the stub.  A DIRECT self-reference — `(apply #'p-pack …)` inside `p-pack` —
# does not: SBCL turns a defun's reference to its own name into a LOCAL call
# whenever the policy has (> speed debug), and the stub then re-enters itself
# and raises "cl/pcl-pack.lisp not found" on a file that is right there.
# Measured s463av: one `(declaim (optimize (speed 3)))` at the top of the
# runtime is enough.  `(symbol-function ',name)` is an fdefinition lookup SBCL
# cannot fold away, and that is what %pcl-def-ext-stub uses.
#
# This is a SOURCE invariant because the bug is invisible at the policy the
# tree compiles at — a behavioural row would pass on the broken spelling.  It
# fails on a tree where any stub is hand-written again.
my $rt_src = do {
  open my $fh, '<:raw', $runtime or die "cannot read $runtime: $!";
  local $/; <$fh>;
};
my @macro_defs = $rt_src =~ /^\(defmacro \%pcl-def-ext-stub\b/mg;
is(scalar @macro_defs, 1,
   'the runtime defines %pcl-def-ext-stub exactly once (one mechanism, rule 11)');

# Every CALL of p-load-extension must be the macro's — that is what makes the
# macro the single place the delegation is spelled.  (Its own `(defun
# p-load-extension` reads differently and is not counted.)  A hand-written stub
# shows up here as a second call site.
my @uses = $rt_src =~ /\(p-load-extension\b/g;
is(scalar @uses, 1,
   'p-load-extension is CALLED from exactly one place: %pcl-def-ext-stub')
  or diag("a hand-written self-loading stub has appeared; route it through "
        . "%pcl-def-ext-stub (task #980) — a direct self-call breaks under "
        . "(> speed debug)");

like($rt_src, qr/^\(\%pcl-def-ext-stub p-pack "pcl-pack"\)$/m,
     'p-pack is a %pcl-def-ext-stub, not a hand-written self-call (#980)');
like($rt_src, qr/^\(\%pcl-def-ext-stub p-unpack "pcl-pack"\)$/m,
     'p-unpack is a %pcl-def-ext-stub, not a hand-written self-call (#980)');
