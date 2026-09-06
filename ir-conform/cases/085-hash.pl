# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 085-hash -- harvested from s469/bg-agent-af8ba53a7b37a7a58/s469bg/bench-subs.pl

my $n = $ENV{N};
sub gcd { my ($x, $y) = @_; return gcd($x - $y, $y) if $x > $y; return gcd($x, $y - $x) if $x < $y; $x }
my $r = 0;
$r += gcd($_ % 97 + 1, 89) for 1 .. $n;
print "$r\n";
