# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-26-sprintf-list-ctx.pl
no warnings;
my @a = (3, 1, 2); my $count = @a; my ($first) = @a; my $last = $a[-1]; my $str = "@a[0,1]"; my @s = sort { $a <=> $b } @a; print "$count $first $last $str @s ", scalar(@a) + 0, " $#a ", join(",", reverse 1..4), " ", "@{[ grep { $_ & 1 } 1..6 ]}", "\n";
