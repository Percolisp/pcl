# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 070-autoviv -- harvested from s470/s471a-agent-ab8307152aef20823/probes2/S7.pl

my @a=(1,2,3); print "exists=", (exists $a[-4] ? 1 : 0), "\n";
