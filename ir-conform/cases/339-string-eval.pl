# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 339-string-eval -- harvested from s470/bs-agent-abb21d9ade6a1c1c3/s470bs/p16.pl

my $q = eval "1/3";
printf "01 [%-8s]\n", $q;
my $w = $q;
printf "02 [%-8s]\n", $w;
printf "03 [%-8s]\n", "$q";
printf "04 [%-8s]\n", $q + 0;
sub id { return $_[0] }
printf "05 [%-8s]\n", id($q);
