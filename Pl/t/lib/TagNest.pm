# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Fixture for Pl/t/import-tag-01.t (task #3184): a tag built from OTHER tags
# in a later statement, and an overridable builtin's name exported by a tag.
package TagNest;
use strict; use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(n1 n2 stat);
our %EXPORT_TAGS = (one => [ qw(n1) ], two => [ '&n2' ]);
$EXPORT_TAGS{all} = [ @{ $EXPORT_TAGS{one} }, @{ $EXPORT_TAGS{two} } ];
$EXPORT_TAGS{ov}  = [ qw(stat) ];
sub n1 ($)  { "n1(" . join(",", @_) . ")" }
sub n2 (;$) { "n2(" . join(",", @_) . ")" }
sub stat    { "mystat(" . join(",", @_) . ")" }
1;
