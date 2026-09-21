# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-01-Data-Dumper.pl
use strict; use warnings; use Data::Dumper;
local $Data::Dumper::Sortkeys = 1; local $Data::Dumper::Indent = 1; my $d = { n => 1, s => "two", l => [1, [2, 3]], h => { k => undef }, c => \"ref" };
print Dumper($d); { local $Data::Dumper::Terse = 1; local $Data::Dumper::Indent = 0; print Dumper([1, "a", { x => 1.5 }]), "\n"; }
my $copy = do { no strict; my $VAR1; eval Dumper($d); $VAR1 }; print $copy->{l}[1][1], " ", Data::Dumper->new([$d->{l}], ["list"])->Indent(0)->Dump, "\n";
