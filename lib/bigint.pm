# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pcl-shim: bigint -- integer literals become Math::BigInt objects (task #2874).
#
# perl's bigint installs compile-time CONSTANT handlers (overload::constant):
# the tokenizer calls them for every numeric literal in the lexical scope of
# `use bigint`.  PCL cannot run a handler while it parses, so this shim holds
# the FACT in the one shape the transpiler reads statically
# (Pl::Parser::module_import_effects): `overload::constant KIND => \&NAME`
# in the import sub, with NAME a sub of this package.  In the scope of `use
# bigint` -- from the statement to the end of its enclosing block, or to a
# `no bigint` -- each literal of that KIND is compiled as `bigint::NAME('TEXT')`,
# the handler called with the literal's SOURCE TEXT exactly as perl calls it,
# so `2**100` never passes through a double.  The operators then run through
# Math::BigInt's own overloads.
#
# What is NOT perl's (docs/not-supported.md "bigint"): a literal inside a
# STRING EVAL is not converted; `hex`/`oct` are exported into the importing
# PACKAGE (perl makes them lexical); bignum / bigrat are separate shims.

package bigint;
use strict;
use warnings;
use Math::BigInt;

our $VERSION = '0.67';
our @ISA = ('Exporter');
require Exporter;
our @EXPORT = qw(inf NaN hex oct);

sub import {
  overload::constant integer => \&_integer,
                     float   => \&_float,
                     binary  => \&_binary;
  __PACKAGE__->export_to_level(1, @_);
}

sub unimport {
  overload::remove_constant('integer', '', 'float', '', 'binary', '');
}

# A decimal integer literal: its digits (underscores are separators).
sub _integer {
  my ($text) = @_;
  $text =~ tr/_//d;
  return Math::BigInt->new($text);
}

# A float literal under bigint is TRUNCATED to an integer (perl: `2.5 + 1` is
# 3, `1e3` is 1000): the decimal point is moved by the exponent and the
# fraction dropped, on the TEXT, so no double is involved.
sub _float {
  my ($text) = @_;
  $text =~ tr/_//d;
  my ($int, $frac, $exp) = $text =~ /\A(\d*)(?:\.(\d*))?(?:[eE]([-+]?\d+))?\z/
    or return Math::BigInt->bnan();
  $frac //= '';
  $exp  //= 0;
  my $digits = $int . $frac;
  my $point  = length($int) + $exp;
  return Math::BigInt->new(0) if $point <= 0 || $digits !~ /[1-9]/;
  $digits .= '0' x ($point - length $digits) if $point > length $digits;
  return Math::BigInt->new(substr($digits, 0, $point));
}

# 0x.. / 0b.. / 0o.. / 0.. (octal) literals.
sub _binary {
  my ($text) = @_;
  $text =~ tr/_//d;
  return Math::BigInt->from_hex($text) if $text =~ /\A0[xX]/;
  return Math::BigInt->from_bin($text) if $text =~ /\A0[bB]/;
  $text =~ s/\A0[oO]/0/;
  return Math::BigInt->from_oct($text);
}

sub inf () { Math::BigInt->binf() }
sub NaN () { Math::BigInt->bnan() }

sub hex {
  my $s = @_ ? $_[0] : $_;
  $s = "0x$s" if $s !~ /\A0[xX]/;
  return Math::BigInt->from_hex($s);
}

sub oct {
  my $s = @_ ? $_[0] : $_;
  $s =~ s/\A\s+//;
  return Math::BigInt->from_hex($s) if $s =~ /\A0?[xX]/;
  return Math::BigInt->from_bin($s) if $s =~ /\A0?[bB]/;
  $s =~ s/\A0?[oO]//;
  return Math::BigInt->from_oct("0$s");
}

1;
