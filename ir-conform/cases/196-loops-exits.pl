# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 196-loops-exits -- harvested from s470/bl-agent-a4ce305d3fd5cf10a/s470bl/p/sm1.pl

my $s=0; for my $i (1..5) { next if $i==2; last if $i==4; $s+=$i } print "s=$s\n"; my $t=0; for (my $j=0;$j<5;$j++) { next if $j==1; $t+=$j } print "t=$t\n"; my @a=(1,2,3); my $u=0; for my $x (@a) { $u+=$x } print "u=$u\n"; my $k=0; while ($k<3) { $k++ } print "k=$k\n";
