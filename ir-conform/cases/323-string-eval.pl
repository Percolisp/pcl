# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 323-string-eval -- harvested from s470/bi-agent-a4b9fedc2175a6513/s469bi/p1083/a.pl

my @s;
push @s, sub { eval "1" } for 1,2;
print "eval:  ", ($s[0] == $s[1] ? "same" : "different"), "\n";
my @t;
push @t, sub { use re "eval"; my $x; s/$x/1/ } for 1,2;
print "reev:  ", ($t[0] == $t[1] ? "same" : "different"), "\n";
my @u;
push @u, sub { s/1/1/ee } for 1,2;
print "ee:    ", ($u[0] == $u[1] ? "same" : "different"), "\n";
my @v;
push @v, sub { 1 } for 1,2;
print "plain: ", ($v[0] == $v[1] ? "same" : "different"), "\n";
