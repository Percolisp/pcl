# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 268-sort -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1056/pr10.pl

# P10  the key slot is ALSO used numerically (mixed use classes must stay
#      opaque-safe) and a nested chain $r->{$k}{$k2}
my $r = {};
my $tot = 0;
for my $n (1..3) {
  my $k  = $n . "";        # string write family
  my $k2 = "sub" . $n;
  $r->{$k}{$k2} = $n;
  $tot += $k + 0;          # numeric USE of the same slot
}
print "P10 $tot ", join(",", map { my $o = $r->{$_}; "$_:" . join("+", map { "$_=$o->{$_}" } sort keys %$o) } sort keys %$r), "\n";
