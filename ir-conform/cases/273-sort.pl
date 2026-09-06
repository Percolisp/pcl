# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 273-sort -- harvested from s470/a5-agent-a9d4cd4f997886d1b/s470a5/probe/al-alias3b.pl

my $str = "b,a";
for my $x (sort split(/,/, $str)) { $x .= "!" }
print "split-direct: $str\n";
my %h2 = (b=>1, a=>2);
for my $k (sort { $a cmp $b } keys %h2) { $k .= "!" }
print "keys2: ", join(",", sort keys %h2), "\n";
my @g = (3,1,2);
for my $x (sort { $a <=> $b } grep { 1 } @g) { $x .= "!" }
print "grep: @g\n";
my @rv = (3,1,2);
for my $x (reverse sort { $a <=> $b } @rv) { $x .= "!" }
print "reverse: @rv\n";
my @j = (3,1,2);
my @copy = sort { $a <=> $b } @j;
$copy[0] = 99;
print "assign-copy: @j\n";
my @k2 = (3,1,2);
print "join: ", join(",", sort { $a <=> $b } @k2), " src=@k2\n";
