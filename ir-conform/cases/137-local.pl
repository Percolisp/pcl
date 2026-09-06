# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 137-local -- harvested from s470/bo-agent-a4964c8ec026d7882/s470bo/m3.pl

use strict; use warnings;
our $g = "outer";
sub show { print "g=$g\n" }
sub wrap { my ($v, $d) = @_; print "wrap[$d]=$v\n"; show(); }
sub inner { wrap((local $g = "inner"), "local-arg"); }
inner();
show();
my @l = (my $z = 7);
print "z=$z l=@l\n";
sub two { my @a = @_; print "two=@a\n" }
two((my $q = 3), 4);
print "q=$q\n";
