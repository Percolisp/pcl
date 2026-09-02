#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# tail-return-01.t — THE TAIL-POSITION `return` (task #994, s465bb).
#
# A `return EXPR` that IS a sub body's last statement does not need to reach
# the frame by throwing: nothing stands between it and the `(catch :p-return …)`
# inside `p-sub-frame`, so its value is already the frame's value.  PCL used to
# emit `(p-return X)` there anyway — a throw, a catch, a *wantarray* rebind and
# an out-of-line `p-return-value` call on EVERY returning call, ~10 % of the
# fib(27) bench row (measured s464 by direct A/B against the bare-tail spelling).
#
# The emission is now `(p-tail-value X)`, wrapped in `(p-caller-ctx …)` exactly
# when the sub-body :void regime is active (a multi-statement body binds
# *wantarray* to :void once, so the tail must restore the caller's context —
# ir-spec §4; a single-statement body never rebinds it and needs no wrap).
# It is registered as the Kind-A emission `tail-return`, so `PCL_OPT=none` and
# `PCL_OPT=-tail-return` run the general `p-return` form (Pl/t/passes-01.t is
# the model for that contract).
#
# WHAT `p-tail-value` IS FOR, and why the elision is not just "drop the call":
# two of `p-return-value`'s arms fire on a value the frame exit (%p-leavesub)
# does NOT touch — an ARRAY in scalar context is its element COUNT, and raw nil
# in list context is the empty list.  Dropping them made `(return @a) . ""`
# print ARRAY(0x1) where perl prints 3.  `p-tail-value` is `p-return-value`
# with its IDENTITY fast path inlined (a value that is neither raw nil nor a
# raw non-string vector comes back unchanged), so the ordinary scalar return
# pays two type tests and no call.
#
# EVERY expected string below was probed against perl 5.40.3 first (run the
# same program with `perl` and with `./runpcl`: byte-equal), on BOTH emission
# paths.  The value rows are semantic invariants — they pass before the change
# too; the SHAPE rows are the ones that fail on a base worktree.
#
# The COPY rule (#964) is unchanged by this and has its own guard,
# Pl/t/return-copy-01.t — program B below re-asserts it through the new
# emission because a tail return is exactly where an elision could lose it.

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

plan tests => 9;

sub run_cl {
    my ($code, %opt) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    local $ENV{PCL_OPT} = $opt{opt} if defined $opt{opt};
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

sub transpile {
    my ($code, %opt) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    local $ENV{PCL_OPT} = $opt{opt} if defined $opt{opt};
    return scalar `$pl2cl $pl_file 2>/dev/null`;
}

# ─────────────────────────────────────────────────────────────────────────────
# A — VALUE rows: context, the array/undef/empty edges, and the two spellings
# of a body (multi-statement, so the :void regime is on and the caller context
# has to be restored; single-statement, where it never was bound).
# `ra-concat` is the row that says p-return-value's array→count arm is still
# applied: perl prints 3, and a plain elision printed ARRAY(0x1).
# ─────────────────────────────────────────────────────────────────────────────
my $PROG_A = <<'PL';
my @arr = (7,8,9);
my @wa  = (10,20,30);
our $VOID = '';
sub s_multi  { my $x = shift; my $y = 2; return $x + $y }
sub s_single { my $x = shift; return $x * 2 }
sub ctx      { my $z = 1; return wantarray ? 'LIST' : defined(wantarray) ? 'SCALAR' : 'VOID' }
sub ctxv     { my $z = 1; $VOID = defined(wantarray) ? 'DEF' : 'VOID'; return 1 }
sub wcond    { my $z = 0; return wantarray ? @wa : $wa[0] }
sub ra       { my $z = 0; return @arr }
sub ra_empty { my @e; my $z = 0; return @e }
sub bare     { my $z = 0; return; }
sub bare1    { return; }
sub rundef   { my $z = 0; return undef }
sub two      { my $x = shift; return 'early' if $x; my $z = 0; return 'tail' }
sub nest     { my $x = shift; if ($x) { return 1 } else { return 2 } }
print "s_multi=", s_multi(4), "\n";
print "s_single=", s_single(4), "\n";
my @c = ctx(); my $cs = ctx(); ctxv();
print "ctx-list=$c[0] ctx-scalar=$cs ctx-void=$VOID\n";
my @w = wcond();
print "wcond-list=@w wcond-scalar=", scalar(wcond()), "\n";
print "ra-list=", join(',', ra()), " ra-scalar=", scalar(ra()), " ra-concat=", ra() . "", "\n";
print "ra-empty-scalar=", scalar(ra_empty()), " ra-empty-list=", scalar(my @e2 = ra_empty()), "\n";
print "bare-scalar=", defined(scalar(bare())) ? 'DEF' : 'UNDEF',
      " bare-list=", scalar(my @b1 = bare()),
      " bare1-list=", scalar(my @b2 = bare1()), "\n";
print "undef-scalar=", defined(scalar(rundef())) ? 'DEF' : 'UNDEF',
      " undef-list=", scalar(my @u = rundef()), "\n";
print "two-early=", two(1), " two-tail=", two(0), "\n";
print "nest-1=", nest(1), " nest-0=", nest(0), "\n";
PL

my $OUT_A = <<'OUT';
s_multi=6
s_single=8
ctx-list=LIST ctx-scalar=SCALAR ctx-void=VOID
wcond-list=10 20 30 wcond-scalar=10
ra-list=7,8,9 ra-scalar=3 ra-concat=3
ra-empty-scalar=0 ra-empty-list=0
bare-scalar=UNDEF bare-list=0 bare1-list=0
undef-scalar=UNDEF undef-list=1
two-early=early two-tail=tail
nest-1=1 nest-0=2
OUT

is(run_cl($PROG_A), $OUT_A, 'A: tail-return values — context, array count, bare/undef edges');
is(run_cl($PROG_A, opt => 'none'), $OUT_A, 'A: same under PCL_OPT=none (the general p-return form)');

# ─────────────────────────────────────────────────────────────────────────────
# B — the rules a tail elision could LOSE: the #964 copy at every aliasing
# consumer, `local` restored at the frame exit AFTER the value is taken, the
# frames that are NOT sub frames (an eval's own :p-return catch, a sort
# comparator's, `goto &sub`), and object identity through the copy.
# ─────────────────────────────────────────────────────────────────────────────
my $PROG_B = <<'PL';
my $x = 'orig';
my @csrc = ('a','b');
our $G = 'global';
our %LH = (k => 'orig');
our @LA = ('a0');
my $obj = bless {v=>5}, 'K';
sub cp     { my $z = 0; return $x }
sub cpa    { my $z = 0; return @csrc }
sub cparg  { my $z = 0; return $_[0] }
sub writer { $_[0] = 'Z' }
sub loc1   { local $G = 'localized'; my $z = 0; return $G }
sub loc2   { local $LH{k} = 'tmp'; my $z = 0; return $LH{k} }
sub loc3   { local $LA[0] = 'tmp0'; my $z = 0; return $LA[0] }
sub loct   { local $_ = 'topic'; my $z = 0; return $_ }
sub robj   { my $z = 0; return $obj }
sub ev1    { my $z = 0; eval { return 'from-eval' }; return 'after-eval' }
sub ev2    { my $z = 0; return eval { 'inner' } }
sub ev3    { my $z = 0; return eval "2+3" }
sub gt_t   { my $z = 0; return "target(@_)" }
sub gt_f   { my $z = 0; goto &gt_t }
sub reset_x { $x = 'orig'; @csrc = ('a','b') }
reset_x(); for my $v (cp()) { $v = 'W' }          print "copy-foreach=$x\n";
reset_x(); my $r = \cp(); $$r = 'X';              print "copy-backslash=$x\n";
reset_x(); writer(cp());                          print "copy-argwrite=$x\n";
reset_x(); for my $v (cpa()) { $v = 'Q' }         print "copy-array=@csrc\n";
my $sv = 'shifted'; for my $v (cparg($sv)) { $v = 'M' } print "copy-argelem=$sv\n";
print "local-value=", loc1(), " local-after=$G\n";
print "localhash-value=", loc2(), " localhash-after=$LH{k}\n";
print "localarr-value=", loc3(), " localarr-after=$LA[0]\n";
print "local-topic=", loct(), "\n";
print "obj-ref=", ref(robj()), " obj-same=", (robj() == $obj ? 'SAME' : 'DIFF'), "\n";
print "eval-block=", ev1(), " eval-value=", ev2(), " eval-string=", ev3(), "\n";
print "goto=", gt_f('g1'), "\n";
my $an = sub { my $q = shift; my $z = 1; return $q * 2 };
my $ansrc = 'anon-src';
my $an3 = sub { my $z = 0; return $ansrc };
for my $v ($an3->()) { $v = 'AW' }
print "anon=", $an->(21), " anon-copy=$ansrc\n";
print "sort=", join(',', sort { return $a <=> $b } (3,1,2)),
      " sort2=", join(',', sort { my $c = $a <=> $b; return $c } (30,10,20)), "\n";
sub fib { my $m = shift; return $m < 2 ? $m : fib($m-1)+fib($m-2) }
sub gcd { my ($p,$q)=@_; return gcd($p-$q,$q) if $p>$q; return gcd($p,$q-$p) if $p<$q; $p }
print "fib=", fib(10), " gcd=", gcd(48,18), "\n";
PL

my $OUT_B = <<'OUT';
copy-foreach=orig
copy-backslash=orig
copy-argwrite=orig
copy-array=a b
copy-argelem=shifted
local-value=localized local-after=global
localhash-value=tmp localhash-after=orig
localarr-value=tmp0 localarr-after=a0
local-topic=topic
obj-ref=K obj-same=SAME
eval-block=after-eval eval-value=inner eval-string=5
goto=target(g1)
anon=42 anon-copy=anon-src
sort=1,2,3 sort2=10,20,30
fib=55 gcd=6
OUT

is(run_cl($PROG_B), $OUT_B, 'B: copy rule, local restore, non-sub frames survive the elision');
is(run_cl($PROG_B, opt => 'none'), $OUT_B, 'B: same under PCL_OPT=none');

# ─────────────────────────────────────────────────────────────────────────────
# SHAPE rows — the emission itself.  These are what fails on a base worktree.
# ─────────────────────────────────────────────────────────────────────────────
my $SRC_MULTI  = 'sub f { my $x = shift; my $y = 2; return $x + $y } print f(1), "\n";';
my $SRC_SINGLE = 'sub f { my $x = shift; return $x + 1 } print f(1), "\n";';
my $SRC_BARE   = 'sub f { my $x = shift; return; } f(1);';
my $SRC_INNER  = 'sub f { my $x = shift; return 1 if $x; my $z = 0; return 2 } print f(1), "\n";';
my $SRC_SORT   = 'my @s = sort { return $a <=> $b } (3,1,2); print "@s\n";';

my $cl_multi = transpile($SRC_MULTI);
like($cl_multi, qr/\(p-caller-ctx\s*\(p-tail-value\s*\(p-\+ \$x \$y\)\)\)/,
     'shape: a multi-statement body tail return is (p-caller-ctx (p-tail-value …))');

my $cl_single = transpile($SRC_SINGLE);
like($cl_single, qr/\(p-tail-value\s*\(p-\+ \$x 1\)\)/,
     'shape: a single-statement body tail return is (p-tail-value …), no context wrap')
  or diag($cl_single);

my $cl_bare = transpile($SRC_BARE);
like($cl_bare, qr/\(p-return-empty\)/,
     'shape: a bare tail `return;` is (p-return-empty)');

my $cl_inner = transpile($SRC_INNER);
like($cl_inner, qr/\(p-return 1\)/,
     'shape: a NON-tail return still throws — only the last statement is elided');

# The general form: PCL_OPT=none must emit p-return for exactly the shapes the
# gate above converts, and never p-tail-value.  One transpile covers both
# claims for the two body spellings and the sort comparator (whose `return`
# belongs to the COMPARATOR frame and is never a body tail in either mode).
my $none_multi = transpile($SRC_MULTI, opt => 'none');
my $none_sort  = transpile($SRC_SORT);
ok($none_multi =~ /\(p-return \(p-\+ \$x \$y\)\)/
     && $none_multi !~ /p-tail-value/
     && $none_sort  =~ /\(p-return \(p-<=>/,
   'shape: PCL_OPT=none restores p-return; a sort comparator keeps it either way')
  or diag("none:\n$none_multi\nsort:\n$none_sort");
