# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s493 perlfunc index walk / pf-sprintf-v.pl
printf "%vd %s\n", "1.22.333", sprintf("%v02x", "1.2.3"); printf "%s\n", sprintf("%*d|%-*d|%.*f", 5, 42, 5, 42, 2, 3.14159); printf "%2\$s %1\$s\n", "a", "b"; printf "%e %g %G\n", 12345.678, 0.0000123, 1e20; printf "%#o %#x %#b %+d % d\n", 8, 255, 5, 3, 3;
