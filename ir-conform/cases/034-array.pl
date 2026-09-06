# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 034-array -- harvested from s470/s471a-agent-ab8307152aef20823/probes2/S19.pl

my @a=(1,2,3); eval { $a[-4]{k} = 1; 1 } or print "caught: $@"; print "ok
";
