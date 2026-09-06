# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 209-oo -- harvested from s470/bl-agent-a4ce305d3fd5cf10a/s470bl/p/r02-destroy.pl

package D; sub DESTROY { last } package main; my $n=0; for my $i (1..3) { $n++; { my $d = bless {}, "D"; } $n+=100 } print "n=$n\n";
