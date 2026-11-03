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
# source echo, the text layout) FAIL; rows 8-12 and 16 pass on both trees and
# pin the families the structured lowering replaced (a second program, so
# they do not depend on the #4185 rows).

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

plan tests => 16;

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

# -- shape: the statement is a tree ---------------------------------------------
my ($sfh, $shape_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
print $sfh q{our ($x, %h);
sub f { local $x = 5; local $h{k} = 3; local ($x, $h{j}) = (1, 2); g() }
};
close $sfh;
my $cl = PCLCore::transpile("$pl2cl $shape_file");
unlike($cl, qr/;; local/, 'no v1 source echo of a local statement');
like($cl, qr/\(p-local-cell \$x \(p-box-for-local 5\)\n\s+\(p-local-hash-elem-init %h "k" 3\n/,
     'the opens nest, each header on its head line');
like($cl, qr/\(p-list-= \(vector \$x \(p-gethash %h "j"\)\) pcl-local-rhs-\d+\)/,
     'a list local assigns through the targets\' places');
