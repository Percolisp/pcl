# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 270-sort -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p996/sort1.pl

my @a = (5, 3, 9, 1);
my @n = sort { $a <=> $b } @a;
my @m = sort { $b <=> $a } @a;
my @s = sort { $a cmp $b } @a;
my @t = sort { $b cmp $a } @a;
my @u = sort @a;
my @v = sort { $a->{k} <=> $b->{k} } ();
print "@n|@m|@s|@t|@u\n";
