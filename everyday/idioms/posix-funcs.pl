# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-33-posix-funcs.pl
no warnings;
use POSIX qw(floor ceil fmod strftime INT_MAX); print floor(-3.5), " ", ceil(3.2), " ", fmod(10, 3), " ", strftime("%Y-%m-%d", gmtime(0)), " ", (INT_MAX > 1e9 ? "int" : "bad"), "\n";
