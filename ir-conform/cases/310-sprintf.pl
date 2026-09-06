# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 310-sprintf -- harvested from s470/s471a-agent-ab8307152aef20823/p1271b/K09.pl

my $fh; open($fh, "<", "/nonexistent-x-1271"); my $fh2 = $fh; printf "copy defined=%s same=%s\n", (defined $fh2?1:0), ((defined $fh && defined $fh2 && "$fh" eq "$fh2")?1:0);
