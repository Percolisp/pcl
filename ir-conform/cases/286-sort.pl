# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 286-sort -- harvested from s470/fable/a5/alias.pl

my @a = (3,1,2);
$_++ for sort { $a <=> $b } @a;
print "aliased-plain: @a\n";
my @b = (3,1,2);
for my $x (sort { $a <=> $b } @b) { $x .= "!" }
print "aliased-named: @b\n";
my @c = ("b","a");
my @r = map { \$_ } sort @c;
print "ident: ", (($r[0] == \$c[1]) ? "same" : "copy"), "\n";
my @n = (3, "nan", 1, "10abc", undef, 2);
{ no warnings; print "num: ", join(",", map { defined $_ ? $_ : "U" } sort { $a <=> $b } @n), "\n"; }
my @nan = (3, 9**9**9/9**9**9, 1, 2);
print "nan: ", join(",", sort { $a <=> $b } @nan), "\n";
print "nan-desc: ", join(",", sort { $b <=> $a } @nan), "\n";
my @s = ("b", "B", "a", "10", "9", "");
print "str: ", join(",", sort @s), "|", join(",", sort { $b cmp $a } @s), "\n";
my @e = ([3,"x"],[1,"y"],[3,"z"],[1,"w"]);
print "stable: ", join(",", map { $_->[1] } sort { $a->[0] <=> $b->[0] } @e), "\n";
