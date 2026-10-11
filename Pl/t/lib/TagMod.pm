# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package TagMod;
use strict; use warnings;
use Exporter 'import';
our @EXPORT    = qw(f0);
our @EXPORT_OK = qw(f1 f2 g1);
our %EXPORT_TAGS = (all => [ @EXPORT, @EXPORT_OK ]);
sub f0 ($)   { "f0(" . join(",", @_) . ")" }
sub f1 ($$)  { "f1(" . join(",", @_) . ")" }
sub f2 (&@)  { my $c = shift; "f2(" . join(",", map { $c->($_) } @_) . ")" }
sub g1 (;$)  { "g1(" . join(",", @_) . ")" }
1;
