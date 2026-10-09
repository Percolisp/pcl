# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Fixture for Pl/t/facts-overlay-01.t (task #2878): a module whose subs and
# export lists only RUNNING code produces, so a static parse cannot see them.
# Its overlay is Pl/t/lib/PCL/Facts/FactsFixture.pm.
package FactsFixture;
use strict;
use warnings;
require Exporter;
our @ISA = ('Exporter');

# `()` constants and a plain sub, glob-installed in BEGIN over a name list.
BEGIN {
  no strict 'refs';
  # (greet first: after the loop it is task #3020's drop, not this test's.)
  *{"FactsFixture::greet"} = sub { "hi" };
  for my $n (qw(ONE ZERO)) {
    my $v = $n eq 'ONE' ? 1 : 0;
    *{"FactsFixture::$n"} = sub () { $v };
  }
}

# Block-form subs built by a string eval over a computed name list.
my %api = (blk => 1, blk2 => 2);
for my $sub (sort keys %api) {
  eval "sub $sub(&;\@) { my \$c = shift; '<' . join('|', \$c->(), \@_) . '>' }";
}

# Export lists computed from data.
our @EXPORT      = grep { /^g/ } qw(greet);
our @EXPORT_OK   = (sort(keys %api), qw(ONE ZERO));
our %EXPORT_TAGS = (all => \@EXPORT_OK);

# The module's own use of its constant, BELOW the BEGIN that installs it.
sub own_test { return ONE + 1 }

1;
