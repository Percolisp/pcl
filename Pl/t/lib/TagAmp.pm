# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package TagAmp;
use strict; use warnings;
use Exporter 'import';
our (@EXPORT_OK, %EXPORT_TAGS);
BEGIN {
    @EXPORT_OK   = qw(h1 h2);
    %EXPORT_TAGS = (ALL => [ qw{ &h1 &h2 } ]);
}
sub h1 ($$) { "h1(" . join(",", @_) . ")" }
sub h2 (\@) { "h2(" . scalar(@{$_[0]}) . ")" }
1;
