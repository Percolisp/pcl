# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 001-aliasing -- harvested from s470/bl-agent-a4ce305d3fd5cf10a/s470bl/p/p57-fe-array-w.pl

sub f { last } my @a=(1,2,3); my $n=0; for my $x (@a) { $x = $x; $n++; f(); $n+=100 } print "n=$n\n";
