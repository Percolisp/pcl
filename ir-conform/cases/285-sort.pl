# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 285-sort -- harvested from s470/bq-agent-a972ef7a99864b78a/s470bq/t3.pl

my @a;
for my $i (1..5) { push @a, $i * 2 }
my @s = sort { $a <=> $b } (3,1,2);
my %h; $h{k} = 1;
my $n = 0; $n += 2; $n++;
sub ins { my $x = shift; return $x + 1 }
print ins(1), "\n";
my @b = (7,8); my $t = 0;
for my $x (@b) { $t += $x }
print "$t @s $h{k}\n";
print ${"main::n"}, "\n";
