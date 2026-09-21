# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s493 perlfunc index walk / pf-stat.pl
my @s = stat("/etc/passwd"); print scalar(@s), " ", ($s[7] > 0 ? "size-ok" : "no-size"), " ", (-f _ ? "f" : "notf"), "\n"; my @l = lstat("/etc/passwd"); print scalar(@l), "\n"; print ((stat "/nonexistent") ? "y" : "n", "\n");
