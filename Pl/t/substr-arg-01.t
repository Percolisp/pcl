#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# substr-arg-01.t -- task #3141: a substr / vec / pos / $#a ARGUMENT of a user
# sub is an LVALUE.  @_ aliases perl's LV, so a write through $_[0] (or
# through \$_[0]) lands in the caller's string -- IO::Zlib's READ is
# `gzread(substr($$bufref, $offset), $n)` and Archive::Tar read nothing.
# PCL passed the substring's VALUE and the write vanished silently.  The
# argument is now the write-through cell `\substr(...)` already used
# (ExprToCL::_alias_slice_arg), and VarAnnotator boxes its target when the
# callee is one the walk cannot see (a method, a code ref, an imported name).
#
# One program, every row a KEY=VALUE line compared with perl's own output.

use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $root    = "$RealBin/../..";
my $pl2cl   = "$root/pl2cl";
my @sbcl_rt = PCLCore::sbcl_prefix("$root/cl/pcl-runtime.lisp");

my $prog = <<'PROG';
sub fill  { my $r = \$_[0]; $$r = "data" . $_[1]; 4 }
sub fill2 { $_[0] = "direct" }
sub app   { $_[0] .= "+" }
sub rd    { return uc $_[0] }
sub two   { $_[0] = "A"; $_[1] = "B" }
sub pr($) { $_[0] = "x" }
sub num   { $_[0] = 66 }
sub g5    { $_[0] = 5 }
sub q1    { $_[0] = "Q" }
my $b1 = ""; fill(substr($b1, 0), 1);            print "s1=[$b1]\n";
my $b2 = "head:"; fill(substr($b2, 5), 2);       print "s2=[$b2]\n";
my $b3 = "xx"; fill2(substr($b3, 1));            print "s3=[$b3]\n";
my $b5 = "hello world"; fill2(substr($b5, 6, 5)); print "s4=[$b5]\n";
my %h = (k => "keyval"); fill2(substr($h{k}, 3)); print "s5=[$h{k}]\n";
my $b6 = "0123456789"; app(substr($b6, 2, 3));   print "s6=[$b6]\n";
my $b8 = "lower"; my $u8 = rd(substr($b8, 0, 3)); print "s7=[$u8 $b8]\n";
my $b9 = "12345"; two(substr($b9, 0, 1), substr($b9, 4, 1)); print "s8=[$b9]\n";
my $p1 = "abc"; pr(substr($p1, 1));              print "s9=[$p1]\n";
my $f4 = "abc"; q1(substr($f4, 0, 1, "Y"));      print "s10=[$f4]\n";
my $v1 = "abc"; num(vec($v1, 1, 8));             print "s11=[$v1]\n";
my $y1 = "aaa"; $y1 =~ /a/g; g5(pos($y1)); my $pp = pos($y1); print "s12=[$pp]\n";
my @a1 = (1, 2, 3); g5($#a1);                     print "s13=[", scalar(@a1), "]\n";
package O; sub m1 { $_[1] = "M" } package main;
my $t1 = "abc"; O->m1(substr($t1, 2));            print "s14=[$t1]\n";
my $cr = sub { $_[0] = "C" }; my $c1 = "abc"; $cr->(substr($c1, 0, 1)); print "s15=[$c1]\n";
my $o1 = "ab"; my $rr = rd(substr($o1, 5)); $rr = defined $rr ? $rr : "U"; print "s16=[$rr]\n";
my $o2 = "ab"; my $ok = eval { fill2(substr($o2, 5)); 1 } ? "lived" : "died"; print "s17=[$ok $o2]\n";
my $ok2 = eval { fill2(substr("abc", 1)); 1 } ? "lived" : "died"; print "s18=[$ok2]\n";
my $b4 = "abcdef"; my $r4 = \substr($b4, 1, 2); $$r4 = "XYZ"; print "s19=[$b4]\n";
my $b7 = "abc"; for my $x (substr($b7, 1, 1)) { $x = "Q" } print "s20=[$b7]\n";
PROG

my ($pfh, $pl) = tempfile(SUFFIX => '.pl', UNLINK => 1);
print $pfh $prog;
close $pfh;
my %perl = map { /^(s\d+)=(.*)$/ ? ($1 => $2) : () } split /\n/, `perl $pl 2>/dev/null`;
my $cl = PCLCore::transpile("$pl2cl $pl");
my ($cfh, $clf) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
print $cfh $cl;
close $cfh;
my %pcl = map { /^(s\d+)=(.*)$/ ? ($1 => $2) : () } split /\n/, `sbcl @sbcl_rt --load $clf 2>/dev/null`;

my @desc = (
  'fill(substr($b,0)) writes through \$_[0] (IO::Zlib READ shape)',
  'a substr window at the END of the string is written',
  '$_[0] = ... writes the window directly',
  'a 3-arg window in the middle',
  'a substr of a HASH ELEMENT',
  '$_[0] .= ... (a read-modify-write through the cell)',
  'a READ-only use reads the window, the source unchanged',
  'two windows of one string in one call',
  'a ($) prototype slot keeps the alias (no p-scalar copy)',
  'a 4-arg substr is NOT an lvalue (returns the replaced text)',
  'vec(...) as an argument is an lvalue',
  'pos(...) as an argument is an lvalue',
  '$#a as an argument is an lvalue (resizes the array)',
  'a METHOD call aliases a substr argument (raw slot boxed)',
  'a CODE-REF call aliases a substr argument (raw slot boxed)',
  'an out-of-range window READS as undef (no death)',
  'an out-of-range window dies only on the WRITE',
  'a constant source dies on the write (read-only value)',
  'inverse: \substr(...) unchanged',
  'inverse: foreach (substr(...)) unchanged',
);
plan tests => scalar @desc;
for my $i (1 .. @desc) {
  my $k = "s$i";
  is($pcl{$k}, $perl{$k}, "#3141 $desc[$i-1] (perl: " . ($perl{$k} // 'undef') . ")");
}
