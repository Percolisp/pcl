# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 051-autoviv -- harvested from s470/bk-agent-ad0b34477151560d8/s470bk/p1058/rows/r16.pl

{ my @a; $a[0][1] .= "x";       print "16 a-a.=   val=", (defined $a[0][1] ? $a[0][1] : "U"), "\n"; }
