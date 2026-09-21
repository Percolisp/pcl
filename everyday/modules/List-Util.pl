# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-04-List-Util.pl
use strict; use warnings; use List::Util qw(first sum sum0 max min maxstr minstr reduce shuffle uniq uniqnum any all none notall pairs pairkeys pairvalues pairmap product head tail);
my @n = (3, 1, 4, 1, 5, 9, 2, 6);
print join(" ", sum(@n), sum0(), max(@n), min(@n), maxstr(qw(a c b)), (first { $_ > 4 } @n), (reduce { $a * $b } 1..5), scalar(uniq @n), product(2, 3), (any { $_ == 9 } @n) ? "any" : "none", (all { $_ > 0 } @n) ? "all" : "notall", (none { $_ > 9 } @n) ? "none" : "some"), "\n";
print join(",", pairkeys(a => 1, b => 2)), " ", join(",", pairvalues(a => 1, b => 2)), " ", join(",", pairmap { "$a-$b" } (a => 1, b => 2)), " ", join(",", map { "$_->[0]=$_->[1]" } pairs(x => 1, y => 2)), " ", join(",", head(2, @n)), " ", join(",", tail(2, @n)), " ", scalar(my @s = shuffle(@n)), "\n";
