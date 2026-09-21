# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-14-JSON-PP.pl
use strict; use warnings; use JSON::PP;
my $j = JSON::PP->new->canonical->pretty; my $t = $j->encode({ name => "x", n => 3, f => 1.5, list => [1, "2", JSON::PP::true, undef], nested => { k => [] } }); print $t;
my $d = decode_json('{"a":[1,2,{"b":"c"}],"t":true,"f":false,"n":null,"s":"ué\n","num":-1.5e2}'); print join(" ", $d->{a}[2]{b}, ($d->{t} ? "T" : "F"), ($d->{f} ? "T" : "F"), (defined $d->{n} ? "def" : "undef"), length($d->{s}), $d->{num} + 0, ref($d->{t}), encode_json([map { $_ * 2 } 1..3])), "\n";
