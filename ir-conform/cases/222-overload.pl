# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 222-overload -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1056/pr13.pl

# P13  the freeze ACTUALLY firing (b-str / b-num verified with PCL_B_DEBUG)
#      on hostile values: a reference, an overloaded object, undef, a number,
#      and a numeric index given the string "3abc".
package Ovl;
use overload '""' => sub { "O" . $_[0]{v} }, '0+' => sub { $_[0]{v} }, fallback => 1;
sub new { bless { v => $_[1] }, $_[0] }
package main;
our @VALS = ({ z => 1 }, Ovl->new(7), undef, 42, "plain");
sub g { $VALS[$_[0]] }
sub h { $_[0] }
my $r = { };
$r->{$_} = "seed" for map { defined($_) ? "$_" : "" } @VALS;
my $out = "";
for my $n (0 .. 4) { my $k1 = g($n); $out .= "<" . (defined $r->{$k1} ? $r->{$k1} : "U") . ">"; }
print "P13a $out\n";
my $ar = [10, 20, 30, 40];
my $s = 0;
for my $n (2 .. 3) { my $i1 = h($n . "abc"); $s += $ar->[$i1]; }
print "P13b $s\n";
