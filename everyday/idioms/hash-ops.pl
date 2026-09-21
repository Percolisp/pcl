# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-10-hash-ops.pl
no warnings;
my %h = (a=>1, b=>2, c=>3); delete $h{b}; my @k = sort keys %h; my %inv = reverse %h; my @sl = @h{qw(a c)}; print "@k $inv{3} @sl ", exists $h{b} ? 1 : 0, " ", scalar(%h) ? "nonempty" : "empty", "\n";
