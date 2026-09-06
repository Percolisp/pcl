# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 167-loops-exits -- harvested from s470/bl-agent-a4ce305d3fd5cf10a/s470bl/p/p23-redo-bare.pl

my $c=0; sub f { redo if $c++ < 2 } my $n=0; { $n++; f(); $n+=100 } print "n=$n c=$c\n";
