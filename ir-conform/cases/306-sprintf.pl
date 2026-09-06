# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 306-sprintf -- harvested from s470/s471a-agent-ab8307152aef20823/p1271b/K05.pl

my $fh; open($fh, "<", "/nonexistent-x-1271"); my $n = 0; $n++ while defined(my $l = <$fh>); printf "lines=%s\n", $n;
