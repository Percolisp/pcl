# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 087-hash -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1046/gcdret.pl

my $n = $ENV{N}; sub gcdret { my ($x,$y)=@_; if ($x==$y) { return $x } return $x>$y ? gcdret($x-$y,$y) : gcdret($x,$y-$x) } my $r=0; $r += gcdret($_ % 97 + 1, 89) for 1..$n; print "$r\n";
