# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 188-loops-exits -- harvested from s470/bl-agent-a4ce305d3fd5cf10a/s470bl/p/q13-map-lexlast.pl

my @s; for my $i (1..3) { push @s,$i; my @y = map { last; $_ } (1,2); push @s,"X" } print "@s\n";
