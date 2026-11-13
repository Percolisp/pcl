# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Fixture for Pl/t/facts-overlay-01.t's import-directive rows (task #4881):
# `()` constants a directive call imports.
package DirectiveConsts;
use strict;
use warnings;
require Exporter;
our @ISA       = ('Exporter');
our @EXPORT    = qw(TWO);
our @EXPORT_OK = qw(TWO THREE);
use constant TWO   => 2;
use constant THREE => 3;

1;
