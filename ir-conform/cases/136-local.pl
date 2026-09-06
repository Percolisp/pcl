# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 136-local -- harvested from s470/bo-agent-a4964c8ec026d7882/s470bo/m17.pl

our $g = "outer";
sub show { print "in=$g\n" }
sub wrap { my ($v) = @_; show(); return $v }
sub inner { my $r = wrap((1, local $g = "inner")); return $r }
my $r = inner();
print "r=$r after=$g\n";
