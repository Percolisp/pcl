# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 277-sort -- harvested from s470/a5-agent-a9d4cd4f997886d1b/s470a5/probe/g1.pl

no warnings;
my @src = (3, 1, 2);
my @a = sort { $a <=> $b } @src;
my @b = sort { $b <=> $a } @src;
my @c = sort { $a cmp $b } @src;
my @d = sort { $b cmp $a } @src;
my @e = sort @src;
print "asc:@a desc:@b cmpa:@c cmpd:@d def:@e\n";
my %h = (bb => 2, aa => 1, cc => 3);
my @k;
for my $key (sort keys %h) { push @k, $key }
print "keys:@k\n";
print "join:", join("|", sort { $a <=> $b } @src), "\n";
my $ar = [ sort { $b <=> $a } @src ];
print "anon:@$ar\n";
sub ret { return sort { $a <=> $b } @src }
print "ret:", join(",", ret()), "\n";
my @mixed = ("10", 9, "abc", undef, "3x");
print "mixnum:", join(",", map { defined $_ ? $_ : "U" } sort { $a <=> $b } @mixed), "\n";
print "mixstr:", join(",", map { defined $_ ? $_ : "U" } sort @mixed), "\n";
my @nan = (3, 9**9**9/9**9**9, 1, 2);
print "nan:", join(",", sort { $a <=> $b } @nan), " nand:", join(",", sort { $b <=> $a } @nan), "\n";
my @pairs = ([2,'a'],[1,'b'],[2,'c'],[1,'d']);
my @st = map { $_->[1] } sort { $a->[0] <=> $b->[0] } @pairs;
print "stable:@st\n";
my @ties = (2, 1, 2, 1);
print "eqkeys:", join(",", sort { $a <=> $b } @ties), "\n";
print "ab:", (defined($a) ? "D" : "U"), (defined($b) ? "D" : "U"), "\n";
print "srcsame:@src\n";
