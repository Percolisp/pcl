# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-21-loop-control.pl
no warnings;
my @o; OUTER: for my $i (1..3) { for my $j (1..3) { next OUTER if $j == 2; last OUTER if $i == 3; push @o, "$i$j" } } my $k = 0; do { $k++ } while ($k < 3); my $u = 10; $u-- until $u <= 7; print "@o $k $u\n";
