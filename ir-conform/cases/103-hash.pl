# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 103-hash -- harvested from s470/bp-agent-a26094bc450615b09/s470bp/fe2-push.pl

my $n = $ENV{N}; my @a; my @b; for my $i (1..500) { push @a, $i } for my $i (501..1000) { push @b, $i } my $s=0; for (1..$n) { for my $x (@a, @b) { $s += $x } } print "$s\n";
