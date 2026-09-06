# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 232-refs -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1056/shape2.pl

sub g { $_[0] }
my $r = {};
my $ar = [];
my $t = 0;
for my $n (1..3) { my $k1 = g($n); $t += $r->{$k1}; }
for my $n (1..3) { my $k2 = g($n); my @v = @$r{$k2, "z"}; }
for my $n (1..3) { my $k3 = g($n); my %w = %$r{$k3}; }
for my $n (1..3) { my $i1 = g($n); $t += $ar->[$i1]; }
for my $n (1..3) { my $i2 = g($n); my @u = @$ar[$i2, 0]; }
print "$t\n";
