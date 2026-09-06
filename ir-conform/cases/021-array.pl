# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 021-array -- harvested from s470/bm-agent-ac55b94993add67d0/s470bm/verify.pl

my $a = 5 & 3; my $b = 5 | 3; my $c = 5 ^ 3; my $d = ~5;
my $s = "ab" x 2; my @l = (1,2) x 2;
my $e = ("a" eq "b"); my $f = ("a" cmp "b");
my $i = 0; $i++; ++$i; $i--; --$i;
print "$a $b $c $d $s @l $e $f $i\n";
