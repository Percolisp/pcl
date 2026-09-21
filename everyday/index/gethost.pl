# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s493 perlfunc index walk / pf-gethost.pl
my @h = gethostbyname("localhost"); print "$h[0] ", length($h[4]), "\n"; use Socket; my $a = inet_ntoa(scalar gethostbyname("localhost")); print "$a\n"; my $n = gethostbyaddr(inet_aton("127.0.0.1"), AF_INET); print defined $n ? "name\n" : "undef\n";
