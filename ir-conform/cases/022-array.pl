# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 022-array -- harvested from s470/bo-agent-a4964c8ec026d7882/s470bo/w2.pl

my (@fuu) = \(1..2,3);
print scalar(@fuu), " ", ${$fuu[0]}, ${$fuu[1]}, ${$fuu[2]}, "\n";
my @b = (7,8);
my @r = \(@b, 9);
print scalar(@r), " ", ${$r[0]}, ${$r[1]}, ${$r[2]}, "\n";
