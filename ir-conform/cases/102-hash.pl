# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 102-hash -- harvested from s470/bp-agent-a26094bc450615b09/s470bp/ar.pl

my $n = $ENV{N}; my @a; my $s=0; for (1..$n) { @a = (1..20, $_); $s += @a } print "$s\n";
