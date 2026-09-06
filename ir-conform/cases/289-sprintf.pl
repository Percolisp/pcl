# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 289-sprintf -- harvested from s470/bk-agent-ad0b34477151560d8/s470bk/p1057/g5.pl

use feature 'bitwise';
no warnings 'experimental::bitwise';
my %k; $k{a}{b} = "\xff"; $k{a}{b} &.= "x"; print $k{a}{b}, "\n";
my %l; $l{a}{b} .= "\x01"; $l{a}{b} |.= "x"; print $l{a}{b}, "\n";
my @m; $m[0]{z} = "\xff"; $m[0]{z} ^.= "\x0f"; printf "%vd\n", $m[0]{z};
