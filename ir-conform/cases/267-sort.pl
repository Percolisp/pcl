# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 267-sort -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1056/pr05.pl

# P5  the key slot is UNDEF (perl: the "" key, with a warning we do not emit)
my $r = {};
my %h;
for my $n (1..2) {
  my $k;
  $k = "x" if $n == 2;
  $r->{$k} = "r$n";
  $h{$k}   = "h$n";
}
print "P5 ", join("|", map { "<$_>=$r->{$_}/$h{$_}" } sort keys %$r), "\n";
