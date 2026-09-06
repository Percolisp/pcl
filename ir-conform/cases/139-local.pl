# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 139-local -- harvested from s470/bo-agent-a4964c8ec026d7882/s470bo/m9.pl

my @x;
eval { @{1, local $x[0][0]} = 1; };
printf "err=%d viv=%d err_text=[%s]\n", ($@ =~ /undefined value as an ARRAY reference/ ? 1 : 0),
                          (defined $x[0][0] ? 1 : 0), ($@ =~ s/\n.*//sr);
