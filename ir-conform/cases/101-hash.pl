# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 101-hash -- harvested from s470/bn-agent-a28fa0a7a0844c434/s470bn/p20-fe2.pl

my $n = $ENV{N};
my @a = (1..500); my @b = (501..1000); my $s=0;
for (1..$n) { for my $x (@a, @b) { $s += $x } }
print "$s\n";
