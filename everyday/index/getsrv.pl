# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s493 perlfunc index walk / pf-getsrv.pl
my @s = getservbyname("http", "tcp"); print "$s[0] $s[2] $s[3]\n"; my ($n) = getservbyport(22, "tcp"); print "$n\n"; my @p = getprotobyname("tcp"); print "$p[0] $p[2]\n";
