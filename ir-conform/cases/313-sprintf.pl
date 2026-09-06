# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 313-sprintf -- harvested from s470/s471a-agent-ab8307152aef20823/p1271b/K12.pl

my $fh; open($fh, "<", "/nonexistent-x-1271"); printf "binmode=[%s]\n", (defined(my $r = binmode($fh)) ? $r : "undef");
