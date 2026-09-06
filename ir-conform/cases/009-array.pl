# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 009-array -- harvested from s470/bj-agent-a7cf766b7d18923d1/s470bj/probe/pushloc.pl

my $n=10; my @a; for my $i (1..$n) { push @a, $i * 2 } print scalar(@a), " ", $a[0], " ", $a[-1], "
";
