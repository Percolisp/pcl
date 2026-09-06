# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 153-loops-exits -- harvested from s470/bj-agent-a7cf766b7d18923d1/s470bj/probe/facts1.pl

my @a;
for my $i (1..5) { push @a, $i * 2; }
print scalar(@a), " ", $a[0], " ", $a[-1], "\n";
my @b = (1,2,3);
my $r = \@b;
push @b, 9;
my @c = (1,2,3);
for my $x (@c) { $c[0] = 99; print "c:$x\n"; last }
my @d = (1,2,3);
for my $y (@d) { print "d:$y\n" }
