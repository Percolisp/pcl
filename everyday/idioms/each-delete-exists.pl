# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-41-each-delete-exists.pl
no warnings;
my %h = (a=>1, b=>2, c=>3); my $sum = 0; while (my ($k, $v) = each %h) { $sum += $v } my @a = (1, 2, 3); my $d = delete $h{a}; my @gone = delete @h{qw(b c)}; print "$sum $d @gone ", scalar(keys %h), " ", exists $a[1] ? 1 : 0, exists $a[9] ? 1 : 0, " ", defined $a[9] ? 1 : 0, "\n";
