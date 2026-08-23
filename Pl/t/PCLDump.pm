# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package PCLDump;
# A core-only dump() for the gate's debugging aids.
#
# A few Pl/t files print parse trees with Data::Dump's dump() when a row
# fails or when their debug blocks are switched on.  Data::Dump is not a core
# module, and the gate must run on a stock perl where only PPI and Moo were
# installed (CI, a user's `prove -j8 Pl/t/`), so this is the same calling
# shape over core Data::Dumper: dump(LIST) returns a string.
use strict;
use warnings;
use Data::Dumper ();
use Exporter 'import';
our @EXPORT_OK = qw(dump);

sub dump {
    local $Data::Dumper::Terse    = 1;
    local $Data::Dumper::Indent   = 1;
    local $Data::Dumper::Sortkeys = 1;
    my $s = Data::Dumper::Dumper(@_);
    chomp $s;
    return $s;
}

1;
