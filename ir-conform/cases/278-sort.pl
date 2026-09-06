# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 278-sort -- harvested from s470/a5-agent-a9d4cd4f997886d1b/s470a5/probe/g2.pl

no warnings;
my @a = (3,1,2);
$_++ for sort { $a <=> $b } @a;
print "topic:@a\n";
my @b = ("b","a");
for my $x (sort { $a cmp $b } @b) { $x .= "!" }
print "namedwrite:@b\n";
my @c = ("b","a");
my @r = map { \$_ } sort @c;
print "ident:", (($r[0] == \$c[1]) ? "same" : "copy"), "\n";
my %h = (b=>1, a=>2);
for my $v (sort { $a <=> $b } values %h) { $v .= "!" }
print "values:", join(",", map { $h{$_} } sort keys %h), "\n";
my @g = (3,1,2);
for my $x (sort { $a <=> $b } grep { 1 } @g) { $x .= "!" }
print "grep:@g\n";
my @rv = (3,1,2);
for my $x (reverse sort { $a <=> $b } @rv) { $x .= "!" }
print "reverse:@rv\n";
my @ro = (3,1,2);
my @out;
for my $x (sort { $a <=> $b } @ro) { push @out, $x }
print "readonly:@ro|@out\n";
my %kh = (b=>1, a=>2);
my @kk;
for my $k (sort keys %kh) { push @kk, uc $k }
print "keysread:@kk\n";
