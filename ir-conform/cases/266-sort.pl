# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 266-sort -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1056/pr04.pl

# P4  the key slot is a NUMBER, then read back through ->{} and {}
my $r = {};
my %h;
for my $n (1..3) {
  my $k = $n * 2;          # numeric write family
  $r->{$k} = "r$k";
  $h{$k}   = "h$k";
}
print "P4 ", join(",", map { "$_=$r->{$_}/$h{$_}" } sort { $a <=> $b } keys %$r), "\n";
