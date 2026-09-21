# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-17-flipflop-chained.pl
use strict; use warnings;
my @lines = map { "l$_" } 1..9; my @sel;
for (@lines) { push @sel, $_ if /l3/ .. /l5/ } print "@sel\n";
@sel = (); for (@lines) { push @sel, $_ if /l2/ ... /l2|l4/ } print "@sel\n";
my ($p, $q, $r) = (1, 5, 9); print(($p < $q && $q < $r ? "ordered" : "not"), " ", join(",", grep { $_ % 2 } 1..9), " ", join(",", (1..3) x 2), " ", scalar(() = (1..5)), "\n");
my @m = (1..5); my ($min, $max) = (sort { $a <=> $b } @m)[0, -1]; my @sl = @m[1..$#m]; my @ev = @m[grep { $_ % 2 == 0 } 0..$#m]; print "$min $max @sl @ev $#{[1,2,3]} ", "@m[-2,-1]", "\n";
