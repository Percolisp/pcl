# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 303-sprintf -- harvested from s470/s471a-agent-ab8307152aef20823/p1271/J11.pl

my $fh = "preset"; my $ok = open($fh, "<", "/nonexistent-x-1271")?1:0; printf "ok=%s val=[%s]\n", $ok, (defined $fh ? $fh : "undef");
