# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 256-regex -- harvested from s470/bq-agent-a972ef7a99864b78a/s470bq/c4.pl

my $x = "abc"; my @a = (1, 2, 3);
my @m1 = ($x =~ /(\w)(\w)/);
my @m2 = ($x !~ /z/);
my @m3 = (/foo/);
my @m4 = (1 + ($x =~ /y/));
my @m5 = (($a[0] =~ /1/), $a[1]);
my @m6 = ($x);
my @m7 = ($x, $x);
my @m8 = (10 .. 12);
print "@m1";
