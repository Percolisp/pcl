# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 316-string -- harvested from s470/bm-agent-ac55b94993add67d0/s470bm/f7.pl

my $x = length("abc") % -10;
my $y = int(3 / -10) * -10;
print "$x $y\n";
