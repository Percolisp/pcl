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

plan tests => 7;

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

# ── 4-6. #1501 (s473t4): a `state` decl in an INNER BLOCK of a named sub.
# Perl ends the name's scope with that block, so a string eval OUTSIDE the
# block can never resolve it — scanning the whole sub for `eval EXPR` refused
# shapes that cannot go wrong, and t/op/coresubs.t (1109 rows) was refused
# whole for exactly that.  The narrowing is licensed by TWO conditions, and
# rows 5 and 6 are the ones that must STILL refuse: an eval inside the decl's
# own block (which perl DOES let name the variable), and the name occurring
# outside the block (where the positional rename would reach past it).
is(run_cl(<<'PL'), "in:1\neval:42\nin:2\neval:42\neval:42\n",
use strict; use warnings;
sub f {
  my ($w) = @_;
  if ($w) {
    use feature 'state';
    state $n = 1;
    print "in:$n\n";
    $n++;
  }
  my $code = 'sub { 42 }';
  my $r = eval $code;
  print "eval:", $r->(), "\n";
}
f(1); f(1); f(0);
PL
   'inner-block state + a string eval OUTSIDE the block: perl 5.40.3 byte-for-byte');

# The eval is INSIDE the decl's block and can name the variable: perl prints
# 6 then 7.  PCL must keep REFUSING, loudly, rather than answer the global.
{
    my $pl = write_pl(<<'PL');
use strict; use warnings;
sub g {
  if ($_[0]) {
    use feature 'state';
    state $n = 5;
    my $v = eval '$n + 1';
    print "inner-eval:", (defined $v ? $v : "undef"), "\n";
    $n++;
  }
}
g(1); g(1);
PL
    my $err = `$pl2cl < $pl 2>&1 >/dev/null`;
    like($err, qr/state \$n in named sub \(string eval\)/,
         'an eval INSIDE the decl block keeps the refusal (perl: 6 then 7)');
}

# The name also occurs OUTSIDE the block (here the package `our $n`), so the
# rename would reach past the block's end.  perl prints state:1 / outer:100 /
# eval:100.  PCL must keep refusing.
{
    my $pl = write_pl(<<'PL');
use strict; use warnings;
our $n = 100;
sub k {
  if ($_[0]) {
    use feature 'state';
    state $n = 1;
    print "state:$n\n";
    $n++;
  }
  print "outer:$n\n";
  my $v = eval '$n';
  print "eval:", (defined $v ? $v : "undef"), "\n";
}
k(1); k(1);
PL
    my $err = `$pl2cl < $pl 2>&1 >/dev/null`;
    like($err, qr/state \$n in named sub \(string eval\)/,
         'the name used OUTSIDE the block keeps the refusal (perl: 1/100/100)');
}

# ── 7. #4821 (s543h): a `state` of the SOURCE rewrite (file level, a file-level
# loop or bare block, a map block, an anon sub, expression position) is
# visible to a string eval by its source name -- the eval words that see the
# declaration carry its cell (an anon sub's per-clone wrapper cell rides the
# alist).  Before, every eval read the empty package global.  The cache leg:
# the same text in two loops with two cells; the position leg: an eval before
# the declaration reads the global.
is(run_cl(<<'PL'), "f=5\nl=11 12\nb=7\na=21 22 21\nm=1,2,3\ni=si\nw=1 2 3\nc=A A B B\nx=5\np=global state\n",
use feature 'state'; no warnings;
state $s = 5; print "f=", eval('$s'), "\n";
my @l; for (1..2) { state $t = 10; $t++; push @l, eval('$t') } print "l=@l\n";
{ state $b = 7; print "b=", eval('$b'), "\n"; }
my @c = map { sub { state $u = 20; $u++; eval '$u' } } 1, 2;
print "a=", $c[0]->(), " ", $c[0]->(), " ", $c[1]->(), "\n";
print "m=", join(",", map { state $m = 0; $m++; eval '$m' } 1 .. 3), "\n";
state $si = "si"; sub inner_reader { eval '$si' } print "i=", inner_reader(), "\n";
my @w; for (1..3) { state $wr = 0; eval '$wr++'; push @w, $wr } print "w=@w\n";
my @cv; for (1..2) { state $cv = "A"; push @cv, eval('$cv') } for (1..2) { state $cv = "B"; push @cv, eval('$cv') } print "c=@cv\n";
sub ex { my $x = (state $ex = 3) + 1; $ex++; eval '$ex' } ex(); print "x=", ex(), "\n";
$main::bd = "global"; my $p1 = eval('$bd'); state $bd = "state"; print "p=$p1 ", eval('$bd'), "\n";
PL
   'a source-rewritten state is visible to a string eval by its name (perl 5.40.3)');
