# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 274-sort -- harvested from s470/a5-agent-a9d4cd4f997886d1b/s470a5/probe/al-alias4.pl

my %h = (b=>1, a=>2);
sub fv { return values %h }
for my $x (sort { $a <=> $b } fv()) { $x .= "!" }
print "sub-values: ", join(",", map { $h{$_} } sort keys %h), "\n";
my @arr = (3,1,2);
sub fa { return @arr }
for my $x (sort { $a <=> $b } fa()) { $x .= "!" }
print "sub-array: @arr\n";
my @src = (3,1,2);
sub retsort { return sort { $a <=> $b } @src }
for my $x (retsort()) { $x .= "!" }
print "return-sort: @src\n";
my @p = (3,1,2);
my @dst;
push @dst, sort { $a <=> $b } @p;
$dst[0] = "Z";
print "push: @p\n";
my @r2 = (3,1,2);
my $ar = [ sort { $a <=> $b } @r2 ];
$ar->[0] = "Z";
print "anon: @r2\n";
