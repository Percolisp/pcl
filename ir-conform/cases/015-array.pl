# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 015-array -- harvested from s470/bl-agent-a4ce305d3fd5cf10a/s470bl/p/q14-many-entries.pl

sub add1 { $_[0] + 1 } my $s=0; for my $k (1..200000) { for my $j (1..3) { $s += add1($j) } } print "s=$s\n";
