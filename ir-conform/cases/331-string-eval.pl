# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 331-string-eval -- harvested from s470/bn-agent-a28fa0a7a0844c434/s470bn/p22-l3.pl

use strict; use warnings;
# Each case in its OWN sub, so the #1140 array facts are scoped to it and the
# licensed shapes actually take the run.
sub c1 { my @a=(1,2,3); my @b=(4,5); my $s=0; for my $x (@a,@b) { $s+=$x } return $s }
sub c2 { my @a=(1,2,3); my @e=(); my @b=(4,5); my $s=0; for my $x (@a,@e,@b) { $s+=$x } return $s }
sub c3 { my @e=(); my @f=(); my $s=0; for my $x (@e,@f) { $s+=$x } return "$s" }
sub c4 { my @a=(1,2,3); my @b=(4,5); my @c=(6); my $s=0; for my $x (@a,@b,@c) { $s+=$x } return $s }
sub c5 { my @a=(1,2,3); my @b=(4,5); my $s=0; OUT: for my $x (@a,@b) { next OUT if $x==2; last OUT if $x==5; $s+=$x } return $s }
sub c6 { my @a=(1,2,3); my @b=(4,5); my $s=0; for my $x (@a,@b) { next if $x%2; $s+=$x } return $s }
sub c7 { my @a=(1,2,3); my @b=(4,5); my $s=0; my $r=0; for my $x (@a,@b) { $r++; if ($x==3 && $r<10) { redo } $s+=$x } return "$s $r" }
sub c8 { my @a=(1,2,3); my @b=(4,5); my $s=0; for my $x (@a,@b) { $s+=$x } continue { $s+=100 } return $s }
sub c9 { my @a=(1,2,3); my @b=(4,5); my $s=0; for my $x (@a[0,1],@b) { $s+=$x } return $s }
sub c10 { my @a=(1,2,3); my $ar=\@a; my @b=(4,5); my $s=0; for my $x (@$ar,@b) { $s+=$x } return $s }
sub c11 { my @a=(1,2,3); my $s=0; for my $x (@a,99) { $s+=$x } return $s }
sub c12 { my @a=(1,2,3); my $s=0; for my $x (@a) { $s+=$x } return $s }
sub c13 { my @w=(1,2,3); my @v=(4,5); my $s=0; for my $x (@w,@v) { $w[2]=99; $s+=$x } return "$s @w" }
sub c14 { my @a=(1,2,3); my @b=(4,5); my @c=(6); my $s=0; for my $x (@a,@b) { for my $y (@b,@c) { $s+=$x*$y } } return $s }
sub c15 { my @p=(1,2); my @q=(3,4); for my $x (@p,@q) { $x*=10 } return "@p @q" }
sub c16 { my @c=(6); my %h=(k=>1); my $s=0; for my $x (@c,%h) { $s += ($x=~/^\d+$/ ? $x : 0) } return $s }
sub c17 { my @a=(1,2,3); my @b=(4,5); my $s=0; for my $x (@a,@b) { push @a, 9 if @a < 5; $s+=$x } return "$s ".scalar(@a) }
sub c18 { my @a=(1,2); my @b=(3,4); my $s=""; for my $x (@a,@b) { $s .= ref(\$x) } return $s }
my @r;
push @r, "$_:" . eval "c$_()" for 1..18;
print join("\n", @r), "\n";
