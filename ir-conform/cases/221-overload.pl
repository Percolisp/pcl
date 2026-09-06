# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 221-overload -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1056/pr12.pl

# P12  the B-regime freeze reached through an UNPROVEN write shape, with a
#      key value that is a REFERENCE / an overloaded object / undef / a
#      number.  The plain %h spelling has taken this path since the B regime
#      existed; the ->{} spelling only since #1056, so the two must agree.
package Ovl;
use overload '""' => sub { "O" . $_[0]{v} }, '0+' => sub { $_[0]{v} }, fallback => 1;
sub new { bless { v => $_[1] }, $_[0] }
package main;
sub g { $_[0] }
my $r = {};
my %h;
my $ref = { z => 1 };
my @vals = ($ref, Ovl->new(7), undef, 42, "plain");
for my $n (0 .. $#vals) {
  my $k1 = g($vals[$n]);
  $r->{$k1} = "d$n";
}
for my $n (0 .. $#vals) {
  my $k2 = g($vals[$n]);
  $h{$k2} = "p$n";
}
my @rk = sort keys %$r;
my @hk = sort keys %h;
print "P12 ", scalar(@rk), "/", scalar(@hk), " ",
      ((join("\0", @rk) eq join("\0", @hk)) ? "keysmatch" : "KEYSDIFFER"), "\n";
print "P12b ", join(",", map { my $x = $_; $x =~ s/0x[0-9a-f]+/0xADDR/; "$x" } @rk), "\n";
