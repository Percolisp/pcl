# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 129-local -- harvested from s470/a5-agent-a9d4cd4f997886d1b/s470a5/probe/al-alias2.pl

my @store = ("bb","aa");
sub f { return @store }
for my $y (sort f()) { $y .= "!" }
print "usersub: @store\n";
sub g { my @l = ("bb","aa"); return @l }
for my $y (sort g()) { $y .= "!" }
print "usersub-local: ok\n";
my %h = (b=>1, a=>2);
for my $k (sort keys %h) { $k .= "!" }
print "keys: ", join(",", sort keys %h), "\n";
for my $v (sort { $a <=> $b } values %h) { $v .= "!" }
print "values: ", join(",", map { $h{$_} } sort keys %h), "\n";
my @m = (3,1,2);
for my $x (sort { $a <=> $b } map { $_ } @m) { $x .= "!" }
print "map: @m\n";
my @sp = split /,/, "b,a";
for my $x (sort @sp) { $x .= "!" }
print "split-var: @sp\n";
