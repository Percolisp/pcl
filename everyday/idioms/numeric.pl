# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-24-numeric.pl
no warnings;
print join(" ", 7 / 2, int(7 / 2), 7 % 3, -7 % 3, 2 ** 10, 10 ** -2, sqrt(16), abs(-3), 0.1 + 0.2 == 0.3 ? "eq" : "ne", 1e3, 0x1f, 0b101, 0o17 // 017, 1_000_000, "3 apples" + 2, int(-3.7), sprintf("%.0f", 2.5), sprintf("%d", 3.99)), "\n";
