# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 330-string-eval -- harvested from s470/bm-agent-ac55b94993add67d0/s470bm/m1.pl

my $x = 1; local $/ = "\n"; my @a=(3,1,2); my @s = sort { $a <=> $b } @a;
for my $v (@s) { print "$v\n" }
eval "1+1"; eval { die "x" };
if ("abc" =~ /b/) { print "m\n" }
push @a, 9;
