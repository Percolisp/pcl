# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 346-wantarray -- harvested from s470/bi-agent-a4b9fedc2175a6513/s469bi/p1045/b.pl

sub t { return wantarray ? "L" : "S" }
sub g { goto &t }
my @r = g(); my $s = g();
print "@r $s\n";
