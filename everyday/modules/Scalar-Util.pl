# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-05-Scalar-Util.pl
use strict; use warnings; use Scalar::Util qw(blessed reftype looks_like_number refaddr weaken isweak dualvar readonly openhandle);
my $o = bless [], "K"; my $r = { a => 1 }; my $w = $r; weaken($w);
print join(" ", blessed($o), reftype($o), (blessed($r) // "undef"), (looks_like_number("1e5") ? 1 : 0), (looks_like_number("x") ? 1 : 0), (refaddr($r) == refaddr($w) ? "same" : "diff"), (isweak($w) ? "weak" : "strong"), (openhandle(\*STDOUT) ? "open" : "closed"), (openhandle(\*NOPE) ? "open" : "closed"), (readonly("lit") ? "ro" : "rw")), "\n";
