# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 250-regex -- harvested from s470/bn-agent-a28fa0a7a0844c434/s470bn/p23-fa.pl

use strict; use warnings;
sub c1  { my @a=(1,2,3); my @b=(4,5); my $s=0; for my $x (@a,@b) { $s+=$x } return $s }
sub c2  { my @a=(1,2,3); my @e=(); my @b=(4,5); my $s=0; for my $x (@a,@e,@b) { $s+=$x } return $s }
sub c3  { my @e=(); my @f=(); my $s=0; for my $x (@e,@f) { $s+=$x } return "$s" }
sub c5  { my @a=(1,2,3); my @b=(4,5); my $s=0; OUT: for my $x (@a,@b) { next OUT if $x==2; last OUT if $x==5; $s+=$x } return $s }
sub c7  { my @a=(1,2,3); my @b=(4,5); my $s=0; my $r=0; for my $x (@a,@b) { $r++; if ($x==3 && $r<10) { redo } $s+=$x } return "$s $r" }
sub c8  { my @a=(1,2,3); my @b=(4,5); my $s=0; for my $x (@a,@b) { $s+=$x } continue { $s+=100 } return $s }
sub c9  { my @a=(1,2,3); my @b=(4,5); my $s=0; for my $x (@a[0,1],@b) { $s+=$x } return $s }
sub c10 { my @a=(1,2,3); my $ar=\@a; my @b=(4,5); my $s=0; for my $x (@$ar,@b) { $s+=$x } return $s }
sub c11 { my @a=(1,2,3); my $s=0; for my $x (@a,99) { $s+=$x } return $s }
sub c13 { my @w=(1,2,3); my @v=(4,5); my $s=0; for my $x (@w,@v) { $w[2]=99; $s+=$x } return "$s @w" }
sub c14 { my @a=(1,2,3); my @b=(4,5); my @c=(6); my $s=0; for my $x (@a,@b) { for my $y (@b,@c) { $s+=$x*$y } } return $s }
sub c15 { my @p=(1,2); my @q=(3,4); for my $x (@p,@q) { $x*=10 } return "@p @q" }
sub c16 { my @c=(6); my %h=(k=>1); my $s=0; for my $x (@c,%h) { $s += ($x=~/^\d+$/ ? $x : 0) } return $s }
sub c17 { my @a=(1,2,3); my @b=(4,5); my $s=0; for my $x (@a,@b) { push @a, 9 if @a < 5; $s+=$x } return "$s ".scalar(@a) }
print join(' ', c1(),c2(),c3(),c5(),c7(),c8(),c9(),c10(),c11(),c13(),c14(),c15(),c16(),c17()), "\n";
