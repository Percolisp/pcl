# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 230-refs -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1056/pr03.pl

# P3  kv-slices: %h{...}, %$r{...}, %a[...], and an array slice @$r[...]
my %h = (a => 1, b => 2, c => 3);
my $r = { %h };
my @a = (10, 20, 30, 40);
my $ar = [@a];
my $s = "";
for my $n (1..3) {
  my $k = chr(96 + $n);
  my %kv  = %h{$k};
  my %kv2 = %$r{$k};
  my $i = $n;
  my %ka  = %a[$i];
  my @sl  = @$ar[$i, 0];
  $s .= "[$k=$kv{$k}/$kv2{$k};$i=$ka{$i};$sl[0],$sl[1]]";
}
print "P3 $s\n";
