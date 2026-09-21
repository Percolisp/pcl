# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-02-sort-variants.pl
use strict; use warnings;
my @n = (10, 9, 100, 1); my @s = qw(b10 a2 B1 a10);
print join(",", sort @n), "|", join(",", sort { $a <=> $b } @n), "|", join(",", reverse sort { $a <=> $b } @n), "\n";
print join(",", sort { lc($a) cmp lc($b) or $a cmp $b } @s), "\n";
my %h = (x => 3, y => 1, z => 3); print join(",", sort { $h{$b} <=> $h{$a} || $a cmp $b } keys %h), "\n";
sub by_len { length($a) <=> length($b) or $a cmp $b } print join(",", sort by_len qw(ccc a bb aa)), "\n";
my @recs = map { { n => $_, k => $_ % 3 } } 1..7; print join(",", map { $_->{n} } sort { $a->{k} <=> $b->{k} } @recs), "\n";
