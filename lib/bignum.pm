# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pcl-shim: bignum -- numeric literals become Math::BigInt / Math::BigFloat
# objects (task #2874).  The mechanism is bigint's (see lib/bigint.pm): the
# import sub names its constant handlers as `overload::constant KIND =>
# \&NAME`, the shape the transpiler reads statically, and each literal in the
# scope of `use bignum` is compiled as a call of that handler with the
# literal's SOURCE text.  An integer literal is a Math::BigInt, a float literal
# a Math::BigFloat, and Math::BigInt UPGRADES to Math::BigFloat (so `1/3` is
# 0.3333...) while Math::BigFloat downgrades an integral result -- perl's
# bignum 0.67 behaviour.  Not perl's: the upgrade/downgrade setting is
# GLOBAL from the first `use bignum` (perl's is too, but it is undone by `no
# bignum`); a literal in a string eval is not converted.

package bignum;
use strict;
use warnings;
use bigint ();
use Math::BigInt;
use Math::BigFloat;

our $VERSION = '0.67';
our @ISA = ('Exporter');
require Exporter;
our @EXPORT = qw(inf NaN hex oct);

sub import {
  Math::BigInt->upgrade('Math::BigFloat');
  Math::BigFloat->downgrade('Math::BigInt');
  overload::constant integer => \&bigint::_integer,
                     float   => \&_float,
                     binary  => \&bigint::_binary;
  __PACKAGE__->export_to_level(1, @_);
}

sub unimport {
  overload::remove_constant('integer', '', 'float', '', 'binary', '');
}

sub _float {
  my ($text) = @_;
  $text =~ tr/_//d;
  return Math::BigFloat->new($text);
}

sub inf () { Math::BigFloat->binf() }
sub NaN () { Math::BigFloat->bnan() }

sub hex { bigint::hex(@_ ? $_[0] : $_) }
sub oct { bigint::oct(@_ ? $_[0] : $_) }

1;
