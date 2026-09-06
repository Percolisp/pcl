# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 276-sort -- harvested from s470/a5-agent-a9d4cd4f997886d1b/s470a5/probe/fast1.pl

no warnings;
my @n = (3, "nan", 1, "10abc", undef, 2);
my @a1 = sort { $a <=> $b } @n;
print "num: ", join(",", map { defined $_ ? $_ : "U" } @a1), "\n";
my @a2 = sort { $b <=> $a } @n;
print "numd: ", join(",", map { defined $_ ? $_ : "U" } @a2), "\n";
my @s = ("b", "B", "a", "10", "9", "");
my @a3 = sort @s;
my @a4 = sort { $a cmp $b } @s;
my @a5 = sort { $b cmp $a } @s;
print "str: ", join(",", @a3), "|", join(",", @a4), "|", join(",", @a5), "\n";
my @mixed = (10, "9", 2, "abc");
my @a6 = sort @mixed;
print "mixstr: ", join(",", @a6), "\n";
my @a7 = sort { $a <=> $b } @mixed;
print "mixnum: ", join(",", @a7), "\n";
my @nan = (3, 9**9**9/9**9**9, 1, 2);
my @a8 = sort { $a <=> $b } @nan;
my @a9 = sort { $b <=> $a } @nan;
print "nan: ", join(",", @a8), "|", join(",", @a9), "\n";
my @inf = (3, 9**9**9, 1, -9**9**9);
my @a10 = sort { $a <=> $b } @inf;
print "inf: ", join(",", @a10), "\n";
my @u = (undef, undef, 1);
my @a11 = sort { $a <=> $b } @u;
print "undef: ", scalar(@a11), " ", join(",", map { defined $_ ? $_ : "U" } @a11), "\n";
my @e = ();
my @a12 = sort { $a <=> $b } @e;
print "empty: ", scalar(@a12), "\n";
my @big = (1e20, 3, -1.5, 2**63, 0.1);
my @a13 = sort { $a <=> $b } @big;
print "big: ", join(",", @a13), "\n";
my @f = (1.5, 1.25, 1.75);
my @a14 = sort { $a <=> $b } @f;
print "float: ", join(",", @a14), "\n";
print "src-untouched: @n | @s | @mixed\n";
print "ab: ", defined($a) ? "def" : "undef", " ", defined($b) ? "def" : "undef", "\n";
