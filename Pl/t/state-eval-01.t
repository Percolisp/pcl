#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# state-eval-01.t — task #401's eval half (s418): a `state` variable in a
# NAMED sub is visible to a STRING EVAL under its ORIGINAL name.
#
# The rename gives the decl a defvar'd package cell `$x__state__N`, which is
# not let-bound, so the eval-site capture alist (`_eval_lexical_alist`) could
# never carry it and the route REFUSED any sub containing a string eval
# ("Parser2 TODO: state $x in named sub (string eval)").  The fix: Parser2
# registers original→cell when the DECL STATEMENT lowers
# (`_eval_state_captures`, scoped by `_lower_sub`'s save/restore) and the
# alist appends the pair after the let-bound ones — so visibility starts at
# the decl (an eval BEFORE it still sees the outer name), dies with the sub,
# and stays open to nested subs and closures (perl's pad chain).
#
# The cache leg (mandatory, ruled s416): p-eval caches compiled text by
# (source, package, features, capture NAMES) — #296-B1 — and resolves a
# captured name at runtime through the alist VALUE.  Two subs eval'ing the
# SAME text therefore share one cache entry and still each see their own
# cell; that is what the a1/b1 row proves.
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

plan tests => 3;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

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

# ── 1. op/sub.t's own shape: the eval sits in a NESTED named sub, and the
# package variable of the same name must not leak in.  Plus the same-sub
# read (`state` advanced by ++), and a dynamic `eval $src`. ─────────────────
is(run_cl(<<'PL'), "got=42\nc=6\nc=7\ndyn=DYN\n",
use feature 'state';
sub outer {
    state $x = 42;
    sub inner { print "got=", ((eval '$x') // "undef"), "\n"; }
}
outer();
$main::x = "WRONG";
inner();
sub c { state $n = 5; $n++; return eval '$n' }
print "c=", c(), "\n"; print "c=", c(), "\n";
sub dy { state $d = "DYN"; my $src = '$d'; return eval $src }
print "dyn=", dy(), "\n";
PL
   'state cell reaches string eval — nested named sub, same sub, dynamic');

# ── 2. The cache-key row (mandatory leg) and the write path: two subs eval
# the SAME text and each must see its OWN cell (shared cache entry, alist
# value resolves at runtime); an eval WRITE lands in the cell. ──────────────
is(run_cl(<<'PL'), "ABA\nw=11\nw=21\n",
use feature 'state';
sub a1 { state $v = "A"; return eval '$v' }
sub b1 { state $v = "B"; return eval '$v' }
print a1(), b1(), a1(), "\n";
sub w1 { state $k = 1; eval '$k = $k + 10'; return $k }
print "w=", w1(), "\n"; print "w=", w1(), "\n";
PL
   'one cached compile, two cells (p-eval keys on capture NAMES); eval writes');

# ── 3. SCOPE guards: an eval BEFORE the decl sees the outer name; a sibling
# sub's eval sees the package variable; a `my` shadow inside the sub wins. ──
is(run_cl(<<'PL'), "d=PKGST\ns=STGLOB\nm=MY\n",
use feature 'state';
no warnings;
our $p = "PKG";
sub d1 { my $r = eval '$p'; state $p = "ST"; return $r . (eval '$p') }
print "d=", d1(), "\n";
our $z = "GLOB";
sub s1 { state $z = "ST"; return eval '$z' }
sub s2 { return eval '$z' }
print "s=", s1(), s2(), "\n";
sub m1 { state $q = "STQ"; my $q = "MY"; return eval '$q' }
print "m=", m1(), "\n";
PL
   'pair starts at the decl, dies with the sub, loses to a my-shadow');
