# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 195-loops-exits -- harvested from s470/bl-agent-a4ce305d3fd5cf10a/s470bl/p/s04-nested-frames.pl

sub g { last } sub f { my $t=0; for my $j (1..3) { $t++; g(); $t+=100 } return $t } my $n=0; for my $i (1..2) { $n += f() * 1000 } print "n=$n\n";
