# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 086-hash -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1046/fibret.pl

my $n = $ENV{N}; sub fibret { my $m=shift; return $m<2 ? $m : fibret($m-1)+fibret($m-2) } my $r=0; $r=fibret(27) for 1..$n; print "$r\n";
