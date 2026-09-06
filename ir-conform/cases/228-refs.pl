# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 228-refs -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1056/g1.pl

sub g { $_[0] }
my $r = {};
my $t = 0;
for my $n (1..3) { my $k = g($n); $t += $r->{$k}; }
print "$t\n";
