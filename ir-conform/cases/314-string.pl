# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 314-string -- harvested from s470/bk-agent-ad0b34477151560d8/s470bk/bench-cat.pl

my $s = "";
for my $i (1 .. 400000) { $s .= "ab" }
print length($s), "\n";
