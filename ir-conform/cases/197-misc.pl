# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 197-misc -- harvested from s469/bg-agent-af8ba53a7b37a7a58/s469bg/rev-p2.pl

my $x = 1;
sub f { $x + 1 }
{ my $x = 20; print "inner=$x\n"; }
print "f=", f(), "\n";
my $y = 5; sub g { $y }  # file-unique: identity promotion
print "g=", g(), "\n";
