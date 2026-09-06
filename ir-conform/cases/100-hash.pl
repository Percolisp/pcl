# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 100-hash -- harvested from s470/bn-agent-a28fa0a7a0844c434/s470bn/p14-loop.pl

my $n = $ENV{N};
my $x = 2;
for my $i (1 .. $n) { $x *= 3; $x %= 1000003 }
print "$x\n";
