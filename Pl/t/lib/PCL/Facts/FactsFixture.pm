# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# The facts overlay for Pl/t/lib/FactsFixture.pm (task #2878): declarations
# only -- what the module installs at BEGIN time, which no static parse sees.
package FactsFixture;

# ONE and ZERO are glob-installed `()` constants (a BEGIN loop over a name
# list); greet is a glob-installed plain sub in a computed @EXPORT.
sub ONE ();
sub ZERO ();
sub greet;

# blk and blk2 are built by a string eval with a (&;@) prototype.
sub blk (&;@);
sub blk2 (&;@);

# The export lists are computed (grep, keys %api), so the scan reads nothing.
our @EXPORT = qw(greet);
our @EXPORT_OK = qw(blk blk2 ONE ZERO);
our %EXPORT_TAGS = (all => [qw(blk blk2 ONE ZERO)]);

1;
