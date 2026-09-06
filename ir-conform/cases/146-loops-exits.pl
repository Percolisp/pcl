# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 146-loops-exits -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1056/pr19.pl

# P19  a PREFIX decrement used as a C-for CONDITION on a raw-numeric slot.
sub g { $_[0] }
my @a = (1, 2, 3);
my $t = 0;
for (my $i = g(3); -- $i;) { $t += $a[$i]; last if $t > 100; }
print "P19 $t\n";
