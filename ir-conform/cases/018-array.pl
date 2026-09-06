# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 018-array -- harvested from s470/bm-agent-ac55b94993add67d0/s470bm/f5.pl

my @a = (1,2);
my @r = \(@a, 3);
print scalar(@r), "\n";
