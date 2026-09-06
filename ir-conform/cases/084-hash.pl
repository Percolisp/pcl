# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 084-hash -- harvested from s469/bg-agent-af8ba53a7b37a7a58/s469bg/bench-loop.pl

my $n = $ENV{N};
my @a = (1 .. 1000);
my $s = 0;
for (1 .. $n) { for my $x (@a) { $s += $x } }
print "$s\n";
