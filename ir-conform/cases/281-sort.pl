# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 281-sort -- harvested from s470/bj-agent-a7cf766b7d18923d1/s470bj/probe/repro.pl

my @fa = (1,2,3);
for my $x (@fa) { $fa[0] = 99; print "plain:$x\n"; last }
my @fs = (3,1,2);
for my $x (sort { $a <=> $b } @fs) { $fs[1] = 99; print "sorted:$x\n"; last }
