# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-04-open-modes.pl
use strict; use warnings;
my $f = "/tmp/pcl-b2-om-$$";
open(my $w, ">", $f) or die; print $w "one\ntwo\n"; close $w;
open(my $a, ">>", $f) or die; print $a "three\n"; close $a;
open(my $rw, "+<", $f) or die; my $first = <$rw>; seek($rw, 0, 0); print $rw "ONE"; close $rw;
open(my $r, "<", $f) or die; my @l = <$r>; close $r; chomp @l; print "@l\n";
my $buf = ""; open(my $m, ">", \$buf) or die; print $m "in-memory ", 42; close $m; print "$buf\n";
open(my $mi, "<", \"l1\nl2\nl3\n") or die; my $cnt = 0; $cnt++ while <$mi>; close $mi; print "$cnt\n";
open(my $dup, ">&", \*STDOUT) or die; print $dup "dup ok\n"; close $dup;
print((open(my $bad, "<", "/nonexistent/x") ? "opened" : "failed: " . ($! ? "errno" : "noerr")), "\n");
unlink $f;
