# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 148-loops-exits -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1059/guardshapes.pl

sub f { $_[0] }
my @a = (1, 2, 4, 8);
my $t1 = 0; for (my $i = f(4); -- $i;) { $t1 += $a[$i]; last if $t1 > 99 }
my $t2 = 0; for (my $j = f(3); $j --;) { $t2 += $a[$j]; last if $t2 > 99 }
my $t3 = 0; for (my $p = f(0); ++ $p < 4;) { $t3 += $a[$p]; last if $t3 > 99 }
my $t4 = 0; for (my $q = f(0); $q ++ < 3;) { $t4 += $a[$q]; last if $t4 > 99 }
print "$t1 $t2 $t3 $t4\n";
