# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 192-loops-exits -- harvested from s470/bl-agent-a4ce305d3fd5cf10a/s470bl/p/q18-inner-of-two.pl

sub g { last } my @s; OUT: for my $i (1..2) { for my $j (1..2) { push @s,"$i$j"; g(); push @s,"X" } push @s,"o$i" } print "@s\n";
