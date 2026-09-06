# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 005-array -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1059/holes.pl

sub f { $_[0] }
my @a = (1, 2, 4, 8);
# (a) C-for condition, prefix
my $t1 = 0;
for (my $i = f(4); -- $i;) { $t1 += $a[$i]; }
# (b) C-for condition, postfix
my $t2 = 0;
for (my $j = f(3); $j --;) { $t2 += $a[$j]; }
# (c) statement-modifier while
my $k = f(4);
my $t3 = 0;
$t3 += $a[$k - 1] while -- $k;
# (d) C-for step, value discarded (the raw twin's own shape)
my $t4 = 0;
for (my $m = f(0); $m < 4; $m ++) { $t4 += $a[$m]; }
print "$t1 $t2 $t3 $t4\n";
