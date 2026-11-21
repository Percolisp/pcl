#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# local-stmt-01.t -- the `local` STATEMENT lowered as trees (s533c, parser
# program step 1d / C2 kind 1, task #3694), with the two fixes that rode
# along: #4185 (a LIST local of globs), #4441 (an array element localized
# to a BOX value) and #4443 (a block ending in a `local` takes its value).
#
# Every runtime expectation is perl 5.40.3's output, probed s533c.  On the
# 0f2db817 extraction TAP rows 1-6 (#4185: the statement was refused), 7
# (#4441: SCALAR(0x1)), 13 (#4443: undef) and the shape rows 14-15 (a v1
# source echo, the text layout) and 17 (a stash glob's name: the base reads
# `*A::C::` as "A::C::" in main, by a stale `$1`) FAIL; rows 8-12 and 16 pass
# on both trees and pin the families the structured lowering replaced (a
# second program, so they do not depend on the #4185 rows).

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

plan tests => 18;

my $fixture = <<'PL';
no strict 'refs';
our ($x, $y, $v, $g) = ('x0', 'y0', 'v0', 'g0');
our @g = (1, 2);
my $caller = 'main';
{ local (*{"${caller}::x"}, *{"${caller}::y"}) = (\"x1", \"y1"); print "1 $x $y\n" }
print "2 $x $y\n";
our ($a1, $b1) = (1, 2);
sub cb { "$a1-$b1" }
my @r;
for my $pair ([3, 4], [5, 6]) {
  local (*{"${caller}::a1"}, *{"${caller}::b1"}) = (\$pair->[0], \$pair->[1]);
  push @r, cb();
}
print "3 @r ", cb(), "\n";
{ local ($v, *g) = ('v1', \"g1"); print "4 $v $g @g\n" } print "5 $v $g @g\n";
{ local (*g, *v) = (\"n1", \"n2") if 0; print "6 $g $v\n" }
our @a = (1, 2);
my @e;
for my $i (5, 6) { local $a[0] = $i; push @e, "@a" }
print "7 @e / @a\n";
#--SPLIT--
our ($x, @a, %h) = ('x0');
@a = (1, 2);
%h = (k => 'K');
sub show { "$x|$h{k}|@a" }
eval { local $x = 'D'; local $h{k} = 'E'; die "z\n" }; print "8 ", show(), "\n";
sub ret { local $x = 'R'; local @a[0, 1] = ('p', 'q'); return show() } print "9 ", ret(), " ", show(), "\n";
for my $i (1 .. 2) { local $x = "L$i"; next if $i == 1; print "10 ", show(), "\n" }
eval q{ local $x = 'S'; print "11 ", show(), "\n"; 1 }; print "12 ", show(), "\n";
sub tf { local $x = 5 } sub tk { local $h{q} = 3 }
my $tv = eval { local $x = 9 };
my @tm = map { local $_ = $_ * 2 } (1, 2);
print "16 ", scalar(tf()), " ", scalar(tk()), " $tv @tm $x\n";
PL

# Two programs, so the family rows (8-12) do not depend on the #4185 rows.
my (%got, $out);
for my $prog (split /^#--SPLIT--\n/m, $fixture) {
  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
  print $fh $prog;
  close $fh;
  my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
  my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
  print $cl_fh $cl_code;
  close $cl_fh;
  my $o = `sbcl @sbcl_rt --load $cl_file 2>&1`;
  $out .= $o;
  $got{$1} = $2 while $o =~ /^(\d+) (.*)$/mg;
}
sub line_is {
  my ($key, $want, $name) = @_;
  is($got{$key} // "(missing; full output: $out)", $want, $name);
}

# -- #4185: a LIST local of globs ----------------------------------------------
line_is(1, 'x1 y1', '#4185: local (*{"P::x"}, *{"P::y"}) = (\\..., \\...) installs both');
line_is(2, 'x0 y0', '#4185: ...and restores both at scope exit');
line_is(3, '3-4 5-6 1-2', '#4185: the sort-callback idiom (Mojo::DOM58 _Collection) per iteration');
line_is(4, 'v1 g1 1 2', '#4185: a scalar and a glob in one list; the glob keeps its other slots');
line_is(5, 'v0 g0 1 2', '#4185: ...both restored');
line_is(6, 'g0 v0', '#4185: a false modifier localizes nothing');
# -- #4441 -----------------------------------------------------------------------
line_is(7, '5 2 6 2 / 1 2', '#4441: local $a[0] = $i (a foreach alias) stores the VALUE');
# -- the families, restored on every exit -------------------------------------
line_is(8, 'x0|K|1 2', 'a scalar and an element restored through a die');
line_is(9, 'R|K|p q x0|K|1 2', 'a scalar and a slice restored at return');
line_is(10, 'L2|K|1 2', 'restored per iteration through next');
line_is(11, 'S|K|1 2', 'local in a string eval');
line_is(12, 'x0|K|1 2', '...restored after it');
line_is(16, '5 3 9 2 4 x0', '#4443: a block whose last statement is a local takes its value');

# -- #5005 (s543b): `local` binds TIGHTER than the comma ------------------------
# `local $a, $b` is `(local $a), $b`; `do { local $/, <$fh> }` reads the whole
# file (the readline was DROPPED: undef); the tail runs inside the new scope
# and is the statement's value by context; a parenthesised list, an
# initializer that is a list operator's (`join ",", @a`), a slice `(1, 2)[1]`
# and `local $x, local $y` keep their readings; a SUB's tail under the body's
# :void regime reads the CALLER's context (rows 17 18, s543b resumed: it read
# the ambient :void and returned the tail alone).  The oracle is perl itself.
# INVERSE: main c5f618f2 differs on rows 1 2 3 6 7 11 13 (and drops row 16's
# statement at compile time).
{
  my $prog = <<'PL';
no strict; no warnings;
my $f = "/tmp/pcl-local-comma-$$.txt"; open my $w, ">", $f or die; print $w "abc\ndef\n"; close $w;
our ($a1, $b1, $x, $y, @a, @b, %h, $p, $q);
sub show { join " ", map { defined $_ ? $_ : "u" } @_ }
{ open my $h, "<", $f or die; my $r = do { local $/, <$h> }; print "1 [", ($r =~ s/\n/|/gr), "]\n"; }
{ open my $h, "<", $f or die; my @r = do { local $/, <$h> }; print "2 ", scalar(@r), " [", join(",", map { defined $_ ? s/\n/|/gr : "u" } @r), "]\n"; }
{ ($a1, $b1) = (1, 2); sub f3 { local $a1, $b1; $a1 = 5; $b1 = 6 } f3(); print "3 ", show($a1, $b1), "\n"; }
{ ($x, $y) = (10, 20); sub f4 { local $x = 1, $y = 2; "in:$x,$y" } my $r = f4(); print "4 $r out:", show($x, $y), "\n"; }
{ %h = (a => 1, b => 2); sub f5 { local $h{a}, $h{b}; $h{a} = 9; $h{b} = 8 } f5(); print "5 ", show($h{a}, $h{b}), "\n"; }
{ @a = (1); @b = (2); sub f6 { local @a, @b; push @a, 7; push @b, 8 } f6(); print "6 [@a] [@b]\n"; }
{ ($p, $q) = (3, 4); sub f8 { local ($p, $q); $p = 7; $q = 8 } f8(); print "8 ", show($p, $q), "\n"; }
{ $x = 10; sub f9 { local $x = (1, 2)[1]; $x } print "9 ", f9(), " ", $x, "\n"; }
{ open my $h, "<", $f or die; local $/ = undef, my $t = <$h>; print "10 [", ($t =~ s/\n/|/gr), "]\n"; }
{ $x = 1; $y = 2; sub f11 { local $x, $y } my @r = f11(); my $s = f11(); print "11 ", scalar(@r), " [", show(@r), "] [", show($s), "]\n"; }
{ sub f13 { local $_ = shift, return "r:$_"; "notreached" } print "13 ", f13("v"), "\n"; }
{ @a = (1, 2, 3); sub f14 { local $x = join ",", @a; $x } print "14 ", f14(), "\n"; }
{ sub f15 { local $x, local $y; $x = 3; $y = 4; } ($x, $y) = (1, 2); f15(); print "15 ", show($x, $y), "\n"; }
{ my @log; sub lg { push @log, @_; scalar @log } sub f16 { local $x, lg("t") } f16(); print "16 [@log]\n"; }
{ sub f17 { if ($_[0]) { local $x, "a", "b" } else { local $x = 1, "c" } } my @r = f17(1); my $s = f17(1); my @r0 = f17(0); print "17 ", scalar(@r), " $s ", scalar(@r0), " [", show(@r0[1 .. $#r0]), "]\n"; }
{ open my $h0, "<", $f or die; sub f18 { my $hh = shift; my $n = 1; local $/, <$hh> } my @r = f18($h0); print "18 ", scalar(@r), " [", join(",", map { defined $_ ? s/\n/|/gr : "u" } @r), "]\n"; }
unlink $f;
PL
  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
  print $fh $prog;
  close $fh;
  my $want = `$^X $pl_file 2>&1`;
  my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
  my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
  print $cl_fh $cl_code;
  close $cl_fh;
  my $o = `sbcl @sbcl_rt --load $cl_file 2>&1`;
  # SBCL's compiler diagnostic block (here the style-warning for f18's unused
  # `my $n`, on the base too) opens with an EMPTY line before its `;' lines:
  # the block is not program output, so both go (the full gate of s543b's
  # second resume failed on that one blank line).
  $o =~ s/^\n(?=;)//gm;
  $o =~ s/^;.*\n//gm;
  $o =~ s/^PCL Runtime loaded\n//gm;
  is($o, $want, '#5005: `local` takes ONE term; the comma tail runs in the new scope and is the value by context');
}

# -- shape: the statement is a tree ---------------------------------------------
my ($sfh, $shape_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
print $sfh q{our ($x, %h);
sub f { local $x = 5; local $h{k} = 3; local ($x, $h{j}) = (1, 2); g() }
};
close $sfh;
my $cl = PCLCore::transpile("$pl2cl $shape_file");
unlike($cl, qr/;; local/, 'no v1 source echo of a local statement');
like(PCLCore::strip_loc($cl), qr/\(p-local-cell \$x \(p-box-for-local 5\)\n\s+\(p-local-hash-elem-init %h "k" 3\n/,
     'the opens nest, each header on its head line');
like($cl, qr/\(p-list-= \(vector \$x \(p-gethash %h "j"\)\) pcl-local-rhs-\d+\)/,
     'a list local assigns through the targets\' places');

# -- a STASH glob's static name (t/mro/package_aliases.t:406) -------------------
# s533c's first tree spelling read `*Fooo::` as the glob named "" (caught by
# the wide emission A/B); v1's text got "Fooo::" only by a stale `$1`.  The
# aliasing itself does not happen under `local` -- #4444.
my ($gfh, $glob_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
print $gfh q{sub f { local *Fooo:: = \%Baro::; local *A::C::; 1 }
};
close $gfh;
my $gcl = PCLCore::transpile("$pl2cl $glob_file");
like($gcl, qr/\(p-local-glob "main" "Fooo::" .*\n(?s:.*)\(p-local-glob "A" "C::" :none/,
     'a stash glob *Pkg:: / *A::B:: is the glob named Pkg:: in main / B:: in A');
