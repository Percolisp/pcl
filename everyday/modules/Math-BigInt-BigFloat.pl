# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-22-Math-BigInt-BigFloat.pl
use strict; use warnings; use Math::BigInt; use Math::BigFloat;
my $f = Math::BigInt->new(1); $f *= $_ for 1..30; my $p = Math::BigInt->new(2)->bpow(100); my $g = Math::BigInt->new("123456789012345678901234567890"); my $q = $g->copy->bdiv(1000); my $bf = Math::BigFloat->new("1")->bdiv(3, 20);
print join(" ", "$f", "$p", $g + 1, "$q", $g->bcmp($p) < 0 ? "lt" : "ge", length("$f"), "$bf", Math::BigInt->new("0x1F")->bstr, ($g->is_odd ? "odd" : "even"), Math::BigInt->new(17)->bmodpow(3, 5)), "\n";
