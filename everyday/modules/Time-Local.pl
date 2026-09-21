# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-11-Time-Local.pl
use strict; use warnings; use Time::Local qw(timegm timelocal timegm_nocheck);
print join(" ", map { timegm(0, 0, 0, 15, $_, 2024) } 0..11), "\n", timegm(59, 59, 23, 31, 11, 1999), " ", timegm_nocheck(0, 0, 0, 1, 0, 2000), " ", (timelocal(localtime(1700000000)) == 1700000000 ? "rt" : "drift"), " ", (eval { timegm(0, 0, 0, 31, 1, 2024); 1 } ? "no-die" : "range-die"), "\n";
