# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 088-hash -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1046/subret.pl

my $n = $ENV{N}; sub add1 { my $x = shift; return $x + 1 } my $s=0; for my $i (1..$n) { $s += add1($i) } print "$s\n";
