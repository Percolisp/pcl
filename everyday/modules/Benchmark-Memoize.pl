# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-21-Benchmark-Memoize.pl
use strict; use warnings; use Benchmark qw(timeit timestr timethis); use Memoize;
my $calls = 0; sub slow { $calls++; $_[0] * 2 } memoize("slow"); slow(2) for 1..5; slow(3); my $t = timeit(1, sub { my $x = 0; $x += $_ for 1..1000 });
print "$calls ", (ref($t) eq "Benchmark" ? "bench-obj" : ref($t)), " ", (timestr($t) =~ /wallclock/ ? "timestr" : "notimestr"), " ", ($t->iters == 1 ? "iters" : "noiters"), "\n";
