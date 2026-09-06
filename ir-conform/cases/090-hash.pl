# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 090-hash -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p996/pushloc.pl

my $n = $ENV{N}; my @a; for my $i (1..$n) { push @a, $i * 2 } print scalar(@a), " ", $a[0], " ", $a[-1], "\n";
