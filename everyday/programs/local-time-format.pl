# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-29-local-time-format.pl
use strict; use warnings; use Time::Local qw(timegm timelocal);
my $t = timegm(5, 4, 3, 2, 0, 2026); my @g = gmtime($t); my $iso = sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ", $g[5] + 1900, $g[4] + 1, @g[3, 2, 1, 0];
my @days = qw(Sun Mon Tue Wed Thu Fri Sat); my $stamp = scalar gmtime($t); my $rt = timegm(@g[0..5]);
print "$t $iso $days[$g[6]] yday=$g[7] '$stamp' ", ($rt == $t ? "roundtrip" : "drift"), " ", (timelocal(localtime($t)) == $t ? "local-rt" : "local-drift"), " ", join("/", (gmtime(0))[5, 4, 3]), "\n";
my $d1 = timegm(0, 0, 0, 1, 2, 2024); my $d2 = timegm(0, 0, 0, 28, 1, 2024); print((($d1 - $d2) / 86400), " days; leap=", ((gmtime($d2 + 86400))[3]), "\n");
