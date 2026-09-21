# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-28-sprintf-local-number-format.pl
use strict; use warnings;
sub commify { my $n = reverse shift; $n =~ s/(\d{3})(?=\d)/$1,/g; scalar reverse $n } sub human { my $b = shift; my @u = qw(B KB MB GB); my $i = 0; while ($b >= 1024 && $i < $#u) { $b /= 1024; $i++ } sprintf("%.1f %s", $b, $u[$i]) }
print join(" | ", commify(1234567), commify(12), human(1536), human(5 * 1024 ** 3), sprintf("%05.1f%%", 45.678), sprintf("%-10s|%10s|", "left", "right"), sprintf("%s", join ",", map { sprintf "%3d%%", $_ * 25 } 0..4), sprintf("%.3g %.3g %g", 1234.5678, 0.00012345, 100000), sprintf("%d items", "3 apples" =~ /(\d+)/), sprintf('%2$s-%1$s', "a", "b")), "\n";
