# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pcl-shim: bigrat -- ANNOUNCED, not implemented (task #2874).
#
# bigint and bignum are built on the static constant-handler shape (see
# lib/bigint.pm); bigrat would be the same three lines, but Math::BigRat
# itself does not run under PCL yet (`Math::BigRat->new("1/3") +
# Math::BigRat->new("1/6")` gives 2, and `* 2` dies), so turning literals
# into Math::BigRat objects would trade a plain-number answer for a wrong
# one.  Until it does, `use bigrat` says so ONCE on stderr (CLAUDE.md rule
# 12's effect-only boundary) and literals stay plain numbers
# (docs/not-supported.md "bigint / bignum / bigrat").

package bigrat;
use strict;
use warnings;

our $VERSION = '0.67';
my $announced;

sub import {
  print STDERR "PCL: use bigrat is not supported (task #2874): numeric literals stay plain numbers\n"
    if !$announced++;
}

sub unimport { }

1;
