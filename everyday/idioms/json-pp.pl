# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-29-json-pp.pl
no warnings;
use JSON::PP; my $j = JSON::PP->new->canonical; my $t = $j->encode({ a => [1, 2.5, "s"], b => JSON::PP::true, n => undef }); my $d = $j->decode(q({"x":[1,{"y":"z"}],"t":true})); print "$t $d->{x}[1]{y} ", ($d->{t} ? "T" : "F"), "\n";
