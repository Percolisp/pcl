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
our @EXPORT_OK = qw(PI e bpi bexp hex oct);

# The import list is perl's bignum 0.67 (task #5500): bigint's options plus
# `upgrade` / `downgrade` (each with its class), read by bigint's ONE reader
# (bigint::_import_options); the int class gets `upgrade => Math::BigFloat`
# and the float class `downgrade => Math::BigInt` first, then the user's.
sub import {
  my $class = shift;
  my $o = bigint::_import_options(1, @_);
  Math::BigInt->import(upgrade => 'Math::BigFloat', @{ $o->{int} }, @{ $o->{import} });
  Math::BigFloat->import(downgrade => 'Math::BigInt', @{ $o->{float} }, @{ $o->{import} });
  bigint::_print_versions($class, 'Math::BigInt') if $o->{version};
  overload::constant integer => \&bigint::_integer,
                     float   => \&_float,
                     binary  => \&bigint::_binary;
  __PACKAGE__->export_to_level(1, $class, @{ $o->{names} });
}

sub unimport {
  overload::remove_constant('integer', '', 'float', '', 'binary', '');
}

sub _float {
  my ($text) = @_;
  $text =~ tr/_//d;
  return Math::BigFloat->new($text);
}

sub inf () { Math::BigInt->binf() }
sub NaN () { Math::BigInt->bnan() }

# The on-request exports, as perl's bignum 0.67 defines them: the constants
# to 40 digits, bpi / bexp computed with upgrading switched off.
sub PI  () { Math::BigFloat->new('3.141592653589793238462643383279502884197') }
sub e   () { Math::BigFloat->new('2.718281828459045235360287471352662497757') }

sub bpi ($) {
  my $up = Math::BigFloat->upgrade();
  Math::BigFloat->upgrade(undef);
  my $x = Math::BigFloat->bpi(@_);
  Math::BigFloat->upgrade($up);
  return $x;
}

sub bexp ($$) {
  my $up = Math::BigFloat->upgrade();
  Math::BigFloat->upgrade(undef);
  my $x = Math::BigFloat->new(shift)->bexp(@_);
  Math::BigFloat->upgrade($up);
  return $x;
}

sub hex { bigint::hex(@_ ? $_[0] : $_) }
sub oct { bigint::oct(@_ ? $_[0] : $_) }

1;
