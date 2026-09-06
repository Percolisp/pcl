# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 220-overload -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1056/pr07.pl

# P7  the key slot holds an OBJECT with an overloaded ""
package Str;
use overload '""' => sub { "S" . $_[0]{v} }, fallback => 1;
sub new { bless { v => $_[1] }, $_[0] }
package main;
my $r = {};
my %h;
for my $n (1..3) {
  my $k = Str->new($n);
  $r->{$k} = "r$n";
  $h{$k}   = "h$n";
}
print "P7 ", join(",", map { "$_=$r->{$_}/$h{$_}" } sort keys %$r), "\n";
