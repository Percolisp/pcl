# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 052-autoviv -- harvested from s470/bk-agent-ad0b34477151560d8/s470bk/p1058/rows/r19.pl

{ my $r; $r->{p}{q}++;          print "19 ref++   val=", (defined $r->{p}{q} ? $r->{p}{q} : "U"), " root=", (defined $r ?1:0), "\n"; }
