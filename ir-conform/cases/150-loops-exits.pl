# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 150-loops-exits -- harvested from s470/bi-agent-a4b9fedc2175a6513/s470bi/p1022/a.pl

sub do_last { last }
my $n = 0;
for my $i (1..3) { $n++; do_last(); $n += 100; }
print "n=$n\n";
