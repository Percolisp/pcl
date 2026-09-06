# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 242-regex -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1056/pr06.pl

# P6  the key slot holds a REFERENCE (perl stringifies it: HASH(0x...))
my $r = {};
my %h;
my $ref = { z => 1 };
for my $n (1..2) {
  my $k = $ref;
  $r->{$k} = "r$n";
  $h{$k}   = "h$n";
}
my @rk = keys %$r;
my @hk = keys %h;
print "P6 ", scalar(@rk), " ", scalar(@hk), " ",
      ($rk[0] =~ /^HASH\(0x[0-9a-f]+\)$/ ? "ok" : "BAD:$rk[0]"), " ",
      ($hk[0] =~ /^HASH\(0x[0-9a-f]+\)$/ ? "ok" : "BAD:$hk[0]"), " ",
      ($rk[0] eq $hk[0] ? "same" : "differ"), "\n";
