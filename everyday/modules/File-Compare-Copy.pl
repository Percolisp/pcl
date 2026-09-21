# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-09-File-Compare-Copy.pl
use strict; use warnings; use File::Compare; use File::Copy qw(copy move);
my $a = "/tmp/pcl-mw-fc-$$.a"; my $b = "$a.b"; open(my $o, ">", $a) or die; print $o "same\n" x 100; close $o;
my $c1 = copy($a, $b); my $cmp1 = compare($a, $b); open($o, ">>", $b) or die; print $o "x"; close $o; my $cmp2 = compare($a, $b); my $m = move($b, "$a.c"); print "$c1 $cmp1 $cmp2 $m ", (-e $b ? "src-left" : "moved"), " ", (-s "$a.c"), "\n"; unlink $a, "$a.c";
