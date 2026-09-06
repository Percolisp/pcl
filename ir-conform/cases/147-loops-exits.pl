# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 147-loops-exits -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1059/guard2.pl

sub f { $_[0] }
my @a = (1, 2, 4, 8);
my ($b1, $b2, $b3, $b4) = (0, 0, 0, 0);
my $t1 = 0; for (my $i = f(4); -- $i;) { $t1 += $a[$i]; last if ++$b1 > 20 }
my $t2 = 0; for (my $j = f(3); $j --;) { $t2 += $a[$j]; last if ++$b2 > 20 }
my $t3 = 0; for (my $p = f(0); ++ $p < 4;) { $t3 += $a[$p]; last if ++$b3 > 20 }
my $t4 = 0; for (my $q = f(0); $q ++ < 3;) { $t4 += $a[$q]; last if ++$b4 > 20 }
print "$t1 $t2 $t3 $t4\n";
