# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 142-local -- harvested from s470/br-agent-ae6b0c4523801002e/s470br/p1190b.pl

my %h;
eval { local $h{a}{b} = 5; print "in: $h{a}{b}\n" };
