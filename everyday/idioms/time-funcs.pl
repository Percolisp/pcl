# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-25-time-funcs.pl
no warnings;
my @t = localtime(0); my @g = gmtime(86400 * 365); print scalar(@t), " $g[5] ", (time > 1e9 ? "now" : "bad"), " ", scalar(gmtime(0)), "\n";
