# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 279-sort -- harvested from s470/a5-agent-a9d4cd4f997886d1b/s470a5/probe/p1.pl

my %h = (b=>1, a=>2);
my @src = (3,1,2);
my @x = sort { $a <=> $b } @src;
my @y = sort { $b <=> $a } @src;
my @z = sort @src;
my @w = sort { $a cmp $b } @src;
my @v = sort keys %h;
my @u = reverse sort { $a <=> $b } @src;
print join(",", sort { $a <=> $b } @src), "\n";
for my $k (sort keys %h) { print "$k " }
print "@x @y @z @w @v @u\n";
