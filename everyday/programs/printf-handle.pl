# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-08-printf-handle.pl
use strict; use warnings;
printf STDOUT "%s-%s\n", "to", "stdout"; printf STDERR "%s\n", "to-stderr";
my $f = "/tmp/pcl-b2-ph-$$"; open(my $o, ">", $f) or die; printf $o "%03d\n", $_ for 1..3; printf {$o} "%s\n", "block"; close $o;
open(my $i, "<", $f) or die; print <$i>; close $i; unlink $f;
{ local $| = 1; print STDOUT "flushed\n"; } my $old = select(STDERR); select($old); print "selected back\n";
