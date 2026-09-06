# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 059-autoviv -- harvested from s470/bk-agent-ad0b34477151560d8/s470bk/p1058/rows/r26.pl

{ my %h; $h{a}{b} &&= 5;        print "27 andeq   val=", (defined $h{a}{b} ? $h{a}{b} : "U"), " outer=", (exists $h{a} ?1:0), "\n"; }
