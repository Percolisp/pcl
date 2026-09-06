# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 226-overload -- harvested from s470/bn-agent-a28fa0a7a0844c434/s470bn/p13-l4.pl

package P;
use overload
  '+'  => sub { P->new($_[0]{v} + (ref $_[1] ? $_[1]{v} : $_[1])) },
  '*'  => sub { P->new($_[0]{v} * (ref $_[1] ? $_[1]{v} : $_[1])) },
  '%'  => sub { P->new($_[0]{v} % (ref $_[1] ? $_[1]{v} : $_[1])) },
  '""' => sub { "P(" . $_[0]{v} . ")" };
sub new { bless { v => $_[1] }, $_[0] }
package main;
my $o = P->new(5);

# (1) a raw slot POISONED by a non-literal compound write, then a LITERAL one
my $s = 0;
$s += $o;          # perl: calls +, $s is an object
$s *= 3;           # perl: calls *, still an object
print "1:$s\n";

# (2) the same with %=
my $t = 0;
$t += $o;
$t %= 7;
print "2:$t\n";

# (3) a plain literal-only loop (the #1153 shape)
my $n = 2;
for my $i (1..4) { $n *= 3; $n %= 1000003 }
print "3:$n\n";

# (4) an object assigned directly into a slot that then takes a literal delta
my $u = $o;
$u *= 2;
print "4:$u\n";

# (5) ++ on a poisoned slot
my $w = 0;
$w += $o;
$w += 1;
print "5:$w\n";
