# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-09-sort-complex.pl
no warnings;
my @r = map { $_->[1] } sort { $a->[0] <=> $b->[0] or $a->[1] cmp $b->[1] } map { [length, $_] } qw(pear fig apple kiwi date); print "@r\n";
