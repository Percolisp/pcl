# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 264-regex -- harvested from s470/s471a-agent-ab8307152aef20823/p1271/J10.pl

my $fh; open($fh, "<", "/nonexistent-x-1271"); printf "str=[%s]\n", (defined $fh ? ($fh =~ /^GLOB/ ? "GLOBish" : "other:$fh") : "undef");
