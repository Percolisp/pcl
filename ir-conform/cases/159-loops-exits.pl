# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 159-loops-exits -- harvested from s470/bl-agent-a4ce305d3fd5cf10a/s470bl/p/p06-last-until.pl

sub f { last } my $n=0; my $g=0; until ($g++ >= 3) { $n++; f(); $n+=100 } print "n=$n g=$g\n";
