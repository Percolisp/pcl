# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 036-autoviv -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1056/shape1.pl

sub g { $_[0] }
my $r = {};
my $t = 0;
for my $n (1..3) { my $k = g($n); $t += $r->{$k}; }
for my $n (1..3) { my $k = g($n); my @v = @$r{$k, "z"}; }
for my $n (1..3) { my $k = g($n); my %v = %$r{$k}; }
my $ar = [];
for my $n (1..3) { my $i = g($n); $t += $ar->[$i]; }
for my $n (1..3) { my $i = g($n); my @v = @$ar[$i, 0]; }
for my $n (1..3) { my $k = g($n); $t += $r->{$k}{x}; }
print "$t\n";
