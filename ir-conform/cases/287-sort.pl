# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 287-sort -- harvested from s470/fable/a5/frhaz.pl

my @fa = (2,1); for my $x (@fa) { $fa[0] = 99; print "fr-plain-w: $x\n"; last }
my @fb = (2,1); for my $x (@fb) { @fb = (7,8); print "fr-plain-aassign: $x\n"; last }
my @fk = (2,1); for my $x (sort { $a <=> $b } keys %{{ 2=>1, 1=>1 }}) { print "fr-keys: $x\n"; last }
my @fc = (2,1); my $i = 0; for my $x (@fc) { $fc[1] = 99 if $i++ == 0; print "fr-plain-next: $x\n" }
