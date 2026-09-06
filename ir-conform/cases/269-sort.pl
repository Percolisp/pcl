# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 269-sort -- harvested from s469/bh-agent-ab83d94ab96620fab/s469bh/p1056/pr15.pl

# P15  the freeze VERIFIED firing (PCL_B_DEBUG says b-str / b-num) on values
#      that are not plain strings: a reference and undef as a KEY, the string
#      "3abc" as an INDEX.  Minimal on purpose — a ternary in the reading
#      loop puts the name in a fallback text and the freeze then declines.
our @K = ({ z => 1 }, undef, 42, "plain");
sub g { $K[$_[0]] }
sub h { $_[0] }
my $r = {};
my %p;
my $out = "";
for my $n (0 .. 3) { my $k1 = g($n); $r->{$k1} = "R$n"; }
for my $n (0 .. 3) { my $k2 = g($n); $p{$k2} = "P$n"; }
for my $n (0 .. 3) { my $k3 = g($n); $out .= $r->{$k3} . $p{$k3} . ";"; }
print "P15a $out\n";
my $ar = [10, 20, 30, 40];
my @aa = (10, 20, 30, 40);
my $s = 0;
for my $n (2 .. 3) { my $i1 = h($n . "abc"); $s += $ar->[$i1]; }
for my $n (2 .. 3) { my $i2 = h($n . "abc"); $s += $aa[$i2]; }
print "P15b $s\n";
my @rk = sort keys %$r;
my @pk = sort keys %p;
print "P15c ", ((join("\0", @rk) eq join("\0", @pk)) ? "keysmatch" : "KEYSDIFFER"),
      " ", scalar(@rk), "\n";
