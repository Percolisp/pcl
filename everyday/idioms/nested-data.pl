# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-11-nested-data.pl
no warnings;
my $d = { list => [1, { x => [10, 20] }], h => { k => "v" } }; push @{ $d->{list} }, 3; $d->{new}{auto}[2] = "viv"; print "$d->{list}[1]{x}[1] $$d{h}{k} ", scalar @{$d->{list}}, " ", scalar @{$d->{new}{auto}}, " ", join(",", sort keys %$d), "\n";
