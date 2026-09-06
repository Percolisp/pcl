# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 204-numeric -- harvested from s470/bk-agent-ad0b34477151560d8/s470bk/bench-num.pl

my $n = 1;
for my $i (1 .. 4000000) { $n *= 3; $n %= 1000003 }
print "check $n\n";
