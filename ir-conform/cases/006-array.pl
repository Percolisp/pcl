# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 006-array -- harvested from s470/bj-agent-a7cf766b7d18923d1/s470bj/probe/feread.pl

my $n=3; my @a = (1..1000); my $s=0; for (1..$n) { for my $x (@a) { $s += $x } } print "$s\n";
