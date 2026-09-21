# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s493 perlfunc index walk / pf-format.pl
our ($name, $amt) = ("apple", 1.5); format STDOUT_TOP =
Name       Amount
---------- ------
.
format STDOUT =
@<<<<<<<<< @##.##
$name,     $amt
.
write; ($name, $amt) = ("pear", 22.25); write;
