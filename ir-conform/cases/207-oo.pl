# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 207-oo -- harvested from s469/bg-agent-af8ba53a7b37a7a58/s469bg/t12.pl

my $shared = 7;
package Later;
print "later=$shared\n";
package main;
{ my $shared = 99; print "inner=$shared\n"; }
print "outer=$shared\n";
