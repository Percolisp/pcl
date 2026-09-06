# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 014-array -- harvested from s470/bk-agent-ad0b34477151560d8/s470bk/p1058/rows/r15.pl

{ my @a; $a[0]{k} .= "x";       print "15 a-h.=   val=", (defined $a[0]{k} ? $a[0]{k} : "U"), "\n"; }
