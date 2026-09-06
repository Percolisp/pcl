# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 065-autoviv -- harvested from s470/s471a-agent-ab8307152aef20823/guard7.pl

my @a = (1,2,3); $a[-1] .= "x";  print "@a\n";
my @b = (1,2,3); $b[-1]++;       print "@b\n";
my @c = (1,2,3); $c[-1] ||= 7;   print "@c\n";
my @d = (1,2,3); $d[-1] = 9;     print "@d\n";
my @e = (1,2,3); my $i = -1; $e[$i] .= "x"; print "@e\n";
my %h = (k => [1,2]); $h{k}[-1] *= 2; print "@{$h{k}} $#{$h{k}}\n";
my @f = (1,2,3); print((defined $f[-4] ? "d" : "u"), (exists $f[-4] ? 1 : 0), "\n");
my @g = (1,2,3); delete $g[-4]; print "@g\n";
