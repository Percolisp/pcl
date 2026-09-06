# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 271-sort -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p996/sortstr.pl

my $n = $ENV{N}; my @src = map { "k" . (($_ * 37) % 101) } 1..50; my $s=0; for (1..$n) { my @x = sort { $a cmp $b } @src; $s += length($x[0]) + length($x[49]) } print "$s\n";
