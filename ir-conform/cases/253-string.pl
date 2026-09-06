# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 253-string -- harvested from s470/bo-agent-a4964c8ec026d7882/s470bo/d3.pl

use strict;
my $s = "abc";
my @c = (
  ['substr($s,10,2)',  sub { substr($s, 10, 2) }],
  ['substr($s,10)',    sub { substr($s, 10) }],
  ['substr($s,3)',     sub { substr($s, 3) }],
  ['substr($s,3,2)',   sub { substr($s, 3, 2) }],
  ['substr($s,-10,2)', sub { substr($s, -10, 2) }],
  ['substr($s,1,-1)',  sub { substr($s, 1, -1) }],
  ['substr($s,1,-5)',  sub { substr($s, 1, -5) }],
);
for my $c (@c) {
  my ($n, $f) = @$c;
  my $v = eval { $f->() };
  my $e = $@ || ''; $e =~ s/ at .*//s;
  printf "%-18s => %s err=[%s]\n", $n, (defined $v ? "[$v]" : 'undef'), $e;
}
my $ns = "plain";
my $u;
printf "exists on non-ref  => %s\n", (defined(exists $ns->{k}) ? "[".(exists $ns->{k})."]" : 'undef');
printf "exists on undef    => %s\n", (defined(exists $u->{k})  ? "[".(exists $u->{k})."]"  : 'undef');
my %h = (a=>1);
printf "exists present     => %s\n", (defined(exists $h{a}) ? "[".(exists $h{a})."]" : 'undef');
printf "exists absent      => %s\n", (defined(exists $h{z}) ? "[".(exists $h{z})."]" : 'undef');
