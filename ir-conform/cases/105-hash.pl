# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 105-hash -- harvested from s470/bp-agent-a26094bc450615b09/s470bp/shapes.pl

my @a = (1..50);
my %h = map { $_ => $_ } 1..50;
my @k = (1..10);
my $s = 0;
my @v = @a[1..5];
my @w = @h{@k};
my @u = @h{'a','b'};
@a[1..3] = (7,8,9);
@a[0,2] = (1,2);
@h{'a','b'} = ($s, 2);
@h{qw(c d)} = (3, 4);
my @f;
@f = (1..20, $s);
@f = (1..$s);
print "$s\n";
