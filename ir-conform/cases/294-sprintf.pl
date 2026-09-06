# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 294-sprintf -- harvested from s470/bs-agent-abb21d9ade6a1c1c3/s470bs/p15.pl

sub a { my $v = shift; return $v }
printf "01 [%-8s]\n", a(1/3);
printf "02 [%s]\n", a(1/3);
printf "03 [%-8s]\n", a("x");
printf "04 [%-8s]\n", a(7);
printf "05 [%-8s]\n", 1/3;
