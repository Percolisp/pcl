# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 132-local -- harvested from s470/bn-agent-a28fa0a7a0844c434/s470bn/p12-bulk.pl

use strict; use warnings;
# The cases a BLOCK-COPY array assignment could get wrong.
my @src = (1,2,3);
my @c = @src; $c[0] = 99;                      # copy, not alias
print "1:@src|@c\n";
my @a = (1,2,3); @a = @a;                      # self-assignment
print "2:@a\n";
my @b = (1,2,3); @b = (0, @b, 4);              # embedding
print "3:@b\n";
my @d = (1,2); my @e = (3,4); my @f = (@d, @e);  # two arrays flatten
print "4:@f\n";
my @g = (1,2,3); delete $g[1]; my @h = @g;     # a hole survives
print "5:", join(',', map { defined($_) ? $_ : 'u' } @h), " ", scalar(@h), "\n";
my $o = bless { v => 7 }, 'K'; my @i = ($o, 5); my @j = @i;
print "6:", ref($j[0]), " ", $j[0]{v}, "\n";
my $r = \my $x; $x = 3; my @k = ($r, 1); my @l = @k; ${$l[0]} = 8;
print "7:$x ", ${$l[0]}, "\n";
my @m = ("a","b"); my @n = @m; $n[1] .= "!";
print "8:@m|@n\n";
my @p; my @q = @p;                              # empty
print "9:", scalar(@q), "\n";
my @big = (1..5); my @small = (7); @big = @small;   # shrink
print "10:@big ", scalar(@big), "\n";
my @grow = (1); @grow = (1..6);                 # grow
print "11:@grow\n";
my %hh = (a=>1); my @flat = %hh; my @fc = @flat;
print "12:", scalar(@fc), "\n";
my @s2 = (3,1,2); my @sorted = sort { $a <=> $b } @s2; $sorted[0] = 42;
print "13:@s2|@sorted\n";
my @mixed = (1, "x", undef, 2.5); my @mc = @mixed;
print "14:", join(',', map { defined($_) ? $_ : 'u' } @mc), "\n";
# each() iterator is reset by an assignment
my @it = (1,2,3); my ($idx) = each @it; my @it2 = (9,8); @it = @it2;
my ($idx2) = each @it; print "15:$idx2\n";
# a dualvar-ish value: $! stringifies and numifies
my @dv; { local $! = 2; @dv = ($!); }
my @dvc = @dv; printf "16:%d %s\n", ($dvc[0]+0 == 2 ? 1 : 0), ($dvc[0] ne '' ? 'y' : 'n');
# a code ref and a glob survive the copy
my @cr = (sub { 11 }, \*STDOUT); my @crc = @cr;
print "17:", $crc[0]->(), " ", ref($crc[1]), "\n";
