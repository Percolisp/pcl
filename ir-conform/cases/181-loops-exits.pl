# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 181-loops-exits -- harvested from s470/bl-agent-a4ce305d3fd5cf10a/s470bl/p/p61-labelled.pl

sub f { last OUT } my $ok=0; OUT: { $ok=1; f(); $ok=0 } print "ok=$ok\n";
