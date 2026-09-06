# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 008-array -- harvested from s470/bj-agent-a7cf766b7d18923d1/s470bj/probe/mix.pl

my @a; my @b;
for my $i (1..3) { push @a, $i; push @b, $i * 2 }
my @c = map { $_ } @a;
for my $x (@c) { print $x }
print "\n@a|@b|@c\n";
