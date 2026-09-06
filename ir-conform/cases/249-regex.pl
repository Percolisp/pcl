# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 249-regex -- harvested from s470/bn-agent-a28fa0a7a0844c434/s470bn/p21-l3.pl

use strict; use warnings;
# The shapes the array-run must get right, and the ones it must decline.
my @a = (1,2,3); my @b = (4,5); my @e = (); my @c = (6);
my $s = 0;
for my $x (@a, @b) { $s += $x } print "1:$s\n";
$s = 0; for my $x (@a, @e, @b) { $s += $x } print "2:$s\n";
$s = 0; for my $x (@e, @e) { $s += $x } print "3:$s ", (defined $s ? 'd' : 'u'), "\n";
$s = 0; for my $x (@a, @b, @c) { $s += $x } print "4:$s\n";
# last / next / redo and a LABEL
$s = 0; OUT: for my $x (@a, @b) { next OUT if $x == 2; last OUT if $x == 5; $s += $x } print "5:$s\n";
$s = 0; for my $x (@a, @b) { next if $x % 2; $s += $x } print "6:$s\n";
$s = 0; my $r = 0; for my $x (@a, @b) { $r++; if ($x == 3 && $r < 10) { redo } $s += $x } print "7:$s $r\n";
# a continue block
$s = 0; for my $x (@a, @b) { $s += $x } continue { $s += 100 } print "8:$s\n";
# NEGATIVES: a slice, a deref, a scalar mixed in, a single array, a written array
my $ar = \@a;
$s = 0; for my $x (@a[0,1], @b) { $s += $x } print "9:$s\n";
$s = 0; for my $x (@$ar, @b) { $s += $x } print "10:$s\n";
$s = 0; for my $x (@a, 99) { $s += $x } print "11:$s\n";
$s = 0; for my $x (@a) { $s += $x } print "12:$s\n";
# an array WRITTEN in the body: perl sees the write through the alias
my @w = (1,2,3); my @v = (4,5);
$s = 0; for my $x (@w, @v) { $w[2] = 99; $s += $x } print "13:$s @w\n";
# nested runs
$s = 0; for my $x (@a, @b) { for my $y (@b, @c) { $s += $x * $y } } print "14:$s\n";
# a run whose loop variable is WRITTEN (declines the read-only arm entirely)
my @p = (1,2); my @q = (3,4);
for my $x (@p, @q) { $x *= 10 } print "15:@p @q\n";
# hash flattening is NOT a bare-array list
my %h = (k => 1);
$s = 0; for my $x (@c, %h) { $s += ($x =~ /^\d+$/ ? $x : 0) } print "16:$s\n";
