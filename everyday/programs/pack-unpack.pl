# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-06-pack-unpack.pl
use strict; use warnings;
my $b = pack("NnC", 1, 2, 3); print length($b), " ", join(",", unpack("NnC", $b)), "\n";
print join(",", unpack("A3 A3", "foobar")), " ", unpack("H*", pack("n", 258)), " ", pack("A5", "hi"), "|\n";
my @f = unpack("(A2)*", "aabbcc"); print "@f ", unpack("%32C*", "abc"), "\n";
print join(".", unpack("C4", pack("N", 3232235777))), " ", length(pack("w", 300)), "\n";
