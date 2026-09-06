# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 106-hash -- harvested from s470/bp-agent-a26094bc450615b09/s470bp/sl.pl

my $n = $ENV{N}; my @a = (1..50); my %h = map { $_ => $_ } 1..50; my @k = (1..10); my $s=0; for (1..$n) { my @v = @a[1..5]; my @w = @h{@k}; $s += $v[0] + $w[9] } print "$s\n";
