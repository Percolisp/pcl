# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 151-loops-exits -- harvested from s470/bi-agent-a4b9fedc2175a6513/s470bi/p1022/b.pl

sub do_next { next }
my $n = 0; my $g = 0;
while ($g++ < 3) { $n++; do_next(); $n += 100; }
print "n=$n\n";
