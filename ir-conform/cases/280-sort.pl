# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 280-sort -- harvested from s470/a5-agent-a9d4cd4f997886d1b/s470a5/probe/p2.pl

package Foo;
sub f {
  my @src = @_;
  my $r = [ sort { $a <=> $b } @src ];
  return sort { $a cmp $b } @src;
}
package main;
my @src = (3,1,2);
my $n = () = sort @src;
my %h;
my @k = sort values %h;
print sort { $a <=> $b } @src;
printf("%s", join("", sort @src));
my ($p, $q) = sort @src;
foreach my $e (sort { $a <=> $b } @src) { print $e }
my @m = sort { $a <=> $b } map { $_ * 2 } @src;
my @g = sort { $a <=> $b } grep { $_ > 1 } @src;
my $cr = sub { 1 };
my @cc = sort $cr, 1, 2;
my @sc = scalar(sort @src);
push @src, sort { $a <=> $b } @src;
my @rv = reverse sort { $a <=> $b } @src;
