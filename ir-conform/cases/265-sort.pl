# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 265-sort -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1056/pr01.pl

# P1  a key read through ->{} : the shape #1056 is about
my $r = {};
for my $n (1..3) { my $k = "k" . $n; $r->{$k} = $n * 10; }
my $s = 0;
for my $n (1..3) { my $k = "k" . $n; $s += $r->{$k}; }
print "P1 $s ", join(",", map { "$_=$r->{$_}" } sort keys %$r), "\n";
