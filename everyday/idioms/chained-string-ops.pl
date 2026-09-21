# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-27-chained-string-ops.pl
no warnings;
my @w = ("b", "a", "c"); my $j = join "-", sort @w; my $u = uc join "", reverse @w; my @sp = split //, "xyz"; my ($a, $b, @rest) = split /,/, "1,2,3,4"; print "$j $u @sp $a $b @rest ", "a,b,,," =~ tr/,//, " ", scalar(my @e = split /,/, "a,b,,,"), "\n";
