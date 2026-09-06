# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 298-sprintf -- harvested from s470/s471a-agent-ab8307152aef20823/p1271/J03.pl

my $fh; my $ok = open($fh, "</nonexistent-x-1271") ? 1:0; printf "ok=%s defined=%s\n", $ok, (defined $fh ?1:0);
