# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-07-sprintf-rounding.pl
use strict; use warnings;
printf "%.2f %.2f %.0f %.0f %.1f %d %s\n", 2.675, 1.005, 0.5, 1.5, 0.25, 3.99, 0.1 + 0.2;
printf "%5s|%-5s|%05d|%+d|%x|%X|%o|%b|%e|%g|%g\n", "ab", "ab", 42, 42, 255, 255, 8, 5, 1234.5, 0.0001234, 1234567;
printf "%s %s %s %s\n", 1e15, 1e16, 1/3, 1e-5;
printf "%3\$s %1\$s %2\$s\n", "a", "b", "c"; printf "%-*s|%.*f\n", 6, "pad", 2, 3.14159;
printf "%s %s %s\n", 9007199254740993, 18446744073709551615, -9223372036854775808;
printf "%%|%c|%5.1f%%|%s\n", 65, 99.5, join(",", map { sprintf "%03b", $_ } 1..3);
