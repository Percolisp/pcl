# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-40-chop-chomp-lc.pl
no warnings;
my $l = "line\n"; chomp(my $c = $l); my $n = chomp($l); my $s = "abc"; chop $s; my ($x, $y) = ("a\n", "b\n"); chomp($x, $y); print "$c $n $s $x$y ", join(",", map { lc } qw(A B)), " ", lcfirst("ABC"), "\n";
