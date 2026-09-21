# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-03-filetests-stat.pl
use strict; use warnings;
my $f = "/tmp/pcl-b2-ft-$$"; open(my $o, ">", $f) or die; print $o "12345"; close $o; chmod 0644, $f;
my @st = stat($f);
print join(" ", (-e $f ? "e" : "-"), (-f $f ? "f" : "-"), (-d $f ? "d" : "-"), (-r $f ? "r" : "-"), (-x $f ? "x" : "-"), (-s $f), (-z $f ? "z" : "-"), (-d "/tmp" ? "D" : "-"), (-e "/nonexistent-$$" ? "E" : "-")), "\n";
printf "%d %04o %s %s\n", $st[7], $st[2] & 07777, ($st[9] > 1e9 ? "mtime" : "bad"), (-M $f < 1 ? "fresh" : "old");
my $size = -s $f; rename $f, "$f.r" or die; print((-e "$f.r" ? "renamed" : "lost"), " ", unlink("$f.r"), " ", (-e "$f.r" ? "still" : "gone"), "\n");
