# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-05-read-seek-tell.pl
use strict; use warnings;
my $f = "/tmp/pcl-b2-rs-$$"; open(my $o, ">", $f) or die; binmode $o; print $o join("", map { chr(65 + $_ % 26) } 0..99); close $o;
open(my $i, "<", $f) or die; binmode $i;
my $n = read($i, my $buf, 10); my $pos = tell($i); seek($i, -5, 2); my $n2 = read($i, my $tail, 100); my $eof = eof($i) ? "eof" : "more";
seek($i, 20, 0); read($i, my $mid, 3, 0); my $c = getc($i);
print "$n $buf $pos $n2 $tail $eof $mid $c\n"; close $i;
open($i, "<", $f) or die; local $/ = \7; my @chunks = <$i>; close $i; print scalar(@chunks), " $chunks[0] ", length($chunks[-1]), "\n";
unlink $f;
