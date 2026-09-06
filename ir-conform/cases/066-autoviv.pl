# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 066-autoviv -- harvested from s470/s471a-agent-ab8307152aef20823/probes/B.pl

my @b = ([1,2,3]); $b[0][-1] += 5; print "b=@{$b[0]} #=$#{$b[0]}\n";
