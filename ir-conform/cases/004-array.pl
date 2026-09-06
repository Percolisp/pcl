# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 004-array -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1056/pr18.pl

my @a = (10, 20, 30, 40, 50);
my $t = 0;
for (my $i = @a; $i > 0; $i--) { $t += $a[$i - 1]; }
print "P18a $t\n";
my @b = (10, 20, 30);
my $c = 0;
my $j = @b;
$c += $b[$j - 1];
print "P18b $j $c\n";
