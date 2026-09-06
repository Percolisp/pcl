# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 010-array -- harvested from s470/bj-agent-a7cf766b7d18923d1/s470bj/probe/ro2.pl

my @a=(1,2);
print "A", scalar(@a), "\n";
Internals::SvREADONLY(@a,1);
print "B", scalar(@a), "\n";
