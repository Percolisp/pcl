# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 340-string-eval -- harvested from s470/bs-agent-abb21d9ade6a1c1c3/s470bs/p17.pl

sub a { my $v = shift; return $v }
my $q = eval "1/3";
printf "01 [%-8s]\n", a($q);
