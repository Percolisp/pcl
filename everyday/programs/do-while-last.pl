# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-18-do-while-last.pl
use strict; use warnings;
my $i = 0; do { $i++ } while ($i < 5); my $j = 10; do { $j-- } until $j <= 7; print "$i $j\n";
my @out; for my $n (1..10) { next if $n % 2; last if $n > 8; push @out, $n; } print "@out\n";
my $k = 0; while (1) { $k++; if ($k == 3) { next } last if $k >= 5 } continue { push @out, "c$k" } print "@out $k\n";
my %seen; my @u = grep { !$seen{$_}++ } qw(a b a c b); my $cnt = () = "abcabc" =~ /b/g; my ($x, $y) = (10, 3); ($x, $y) = ($y, $x); print "@u $cnt $x $y\n";
my @stack = (1..5); my @popped = (pop @stack, shift @stack); unshift @stack, 0; push @stack, 9; my @sp = splice(@stack, 1, 2); splice(@stack, 1, 0, "in", "s"); print "@popped | @stack | @sp | ", join(",", reverse 1..4), " ", join("", map { chr(ord($_) + 1) } split //, "HAL"), "\n";
my $str = join ",", map { "$_=" . ($_ ** 2) } grep { $_ & 1 } 1..6; my @parts = split /,/, $str, 2; print "$str | @parts | ", scalar(@{[ split ' ', "  a  b c " ]}), " ", join("|", split(/(,)/, "a,b")), " ", join("|", split(//, "abc", 2)), "\n";
