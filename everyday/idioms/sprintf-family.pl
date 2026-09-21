# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-08-sprintf-family.pl
no warnings;
printf("%5.2f|%-6s|%03d|%x|%e|%s\n", 3.14159, "ab", 7, 255, 12345.678, join(",", map { sprintf "%02d", $_ } 1..3));
