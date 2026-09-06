# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 229-refs -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1056/pr02.pl

# P2  a hash SLICE through a reference: @$r{...} and @{$r}{...}
my $r = { a => 1, b => 2, c => 3 };
my $s = "";
for my $n (1..3) {
  my $k1 = chr(96 + $n);
  my $k2 = chr(96 + ($n % 3) + 1);
  my @v = @$r{$k1, $k2};
  my @w = @{$r}{$k1, $k2};
  $s .= "[$k1$k2:$v[0]$v[1]/$w[0]$w[1]]";
}
print "P2 $s\n";
