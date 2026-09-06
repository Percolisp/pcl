# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 011-array -- harvested from s470/bj-agent-a7cf766b7d18923d1/s470bj/probe/tt2.pl

sub t { my @c = @_; for my $r (@c) { print $r } }
t(1,2,3);
