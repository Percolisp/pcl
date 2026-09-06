# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 047-autoviv -- harvested from s470/bk-agent-ad0b34477151560d8/s470bk/p1058/rows/r10.pl

{ my %h; my $d = defined $h{a}{b}; print "10 defined d=", ($d?1:0), " outer=", (exists $h{a} ?1:0), " keys=", scalar(keys %h), "\n"; }
