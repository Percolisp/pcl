# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 283-sort -- harvested from s470/bn-agent-a28fa0a7a0844c434/s470bn/p10-sort.pl

my $n = $ENV{N};
my @src = map { ($_ * 37) % 101 } 1..50;
my $s=0;
for (1..$n) { my @x = sort { $a <=> $b } @src; $s += $x[0] + $x[49] }
print "$s\n";
