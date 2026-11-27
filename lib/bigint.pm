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
#
# THE IMPORT LIST is perl's bigint 0.67 (task #5500, s543t): the options
# `a` / `accuracy`, `p` / `precision`, `round_mode` and `l` / `lib` / `try` /
# `only` (each with its value) are handed to Math::BigInt->import, as perl's
# bigint does -- PCL runs the pure-Perl Math::BigInt::Calc whatever library is
# named, exactly as perl does on a box without the GMP / Pari backends, and
# `lib` / `only` warn / die the way Math::BigInt's own import does; `v` /
# `version` prints the versions and exits; `t` / `trace` uses
# Math::BigInt::Trace; the names `PI e bpi bexp hex oct` are exported on
# request (@EXPORT_OK), `inf NaN hex oct` by default -- the Exporter rule, which the
# transpiler reads statically (perl overrides `hex` / `oct` lexically whatever the
# list says: under a non-empty list they stay the built-ins here); anything else dies
# "Unknown option '...'" at the `use`.  _import_options is the ONE reader of
# that list, bignum's import calls it too.

package bigint;
use strict;
use warnings;
use Math::BigInt;

our $VERSION = '0.67';
our @ISA = ('Exporter');
require Exporter;
our @EXPORT = qw(inf NaN hex oct);
our @EXPORT_OK = qw(PI e bpi bexp hex oct);

# The class the literals become: Math::BigInt, or Math::BigInt::Trace under
# the `t` option.
our $obj_class = 'Math::BigInt';

# perl's import loop (bigint.pm / bignum.pm 0.67), shared by both shims.
# Returns a hash: import => the common options for the classes' import,
# int / float => the int-class / float-class-only options (bignum's upgrade /
# downgrade), names => the requested exports, version => `v` was given,
# trace => `t` was given.  An unknown word dies at the caller's `use`.
sub _import_options {
  my ($bignum, @args) = @_;
  my %o = (import => [], int => [], float => [], names => []);
  while (@args) {
    my $param = shift @args;
    if ($bignum && ($param eq 'upgrade' || $param eq 'downgrade')) {
      push @{ $o{$param eq 'upgrade' ? 'int' : 'float'} }, $param, shift @args;
    } elsif ($param =~ /^a(ccuracy)?$/) {
      push @{ $o{import} }, 'accuracy', shift @args;
    } elsif ($param =~ /^p(recision)?$/) {
      push @{ $o{import} }, 'precision', shift @args;
    } elsif ($param eq 'round_mode') {
      push @{ $o{import} }, 'round_mode', shift @args;
    } elsif ($param =~ /^(l|lib|try|only)$/) {
      push @{ $o{import} }, $param eq 'l' ? 'lib' : $param;
      push @{ $o{import} }, shift @args if @args;
    } elsif ($param =~ /^(v|version)$/) {
      $o{version} = 1;
    } elsif (!$bignum && $param =~ /^(t|trace)$/) {
      $o{trace} = 1;
    } elsif ($param =~ /^(PI|e|bexp|bpi|hex|oct)\z/) {
      push @{ $o{names} }, $param;
    } else {
      require Carp;
      Carp::croak("Unknown option '$param'");
    }
  }
  return \%o;
}

# What perl's `v` option prints, then exit.
sub _print_versions {
  my ($class, $obj_class) = @_;
  printf "%-31s v%s\n", $class, $class->VERSION();
  printf " lib => %-23s v%s\n", $obj_class->config("lib"), $obj_class->config("lib_version");
  printf "%-31s v%s\n", $obj_class, $obj_class->VERSION();
  exit;
}

sub import {
  my $class = shift;
  my $o = _import_options(0, @_);
  if ($o->{trace}) {
    $obj_class = 'Math::BigInt::Trace';
    eval "require $obj_class";
    die $@ if $@;
  }
  $obj_class->import(@{ $o->{import} });
  _print_versions($class, $obj_class) if $o->{version};
  overload::constant integer => \&_integer,
                     float   => \&_float,
                     binary  => \&_binary;
  __PACKAGE__->export_to_level(1, $class, @{ $o->{names} });
}

sub unimport {
  overload::remove_constant('integer', '', 'float', '', 'binary', '');
}

# A decimal integer literal: its digits (underscores are separators).
sub _integer {
  my ($text) = @_;
  $text =~ tr/_//d;
  return $obj_class->new($text);
}

# A float literal under bigint is TRUNCATED to an integer (perl: `2.5 + 1` is
# 3, `1e3` is 1000): the decimal point is moved by the exponent and the
# fraction dropped, on the TEXT, so no double is involved.
sub _float {
  my ($text) = @_;
  $text =~ tr/_//d;
  my ($int, $frac, $exp) = $text =~ /\A(\d*)(?:\.(\d*))?(?:[eE]([-+]?\d+))?\z/
    or return $obj_class->bnan();
  $frac //= '';
  $exp  //= 0;
  my $digits = $int . $frac;
  my $point  = length($int) + $exp;
  return $obj_class->new(0) if $point <= 0 || $digits !~ /[1-9]/;
  $digits .= '0' x ($point - length $digits) if $point > length $digits;
  return $obj_class->new(substr($digits, 0, $point));
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

sub inf () { $obj_class->binf() }
sub NaN () { $obj_class->bnan() }

# The on-request exports, as perl's bigint 0.67 defines them (integers).
sub PI  () { $obj_class->new(3) }
sub e   () { $obj_class->new(2) }
sub bpi ($) { $obj_class->new(3) }
sub bexp ($$) { my $x = $obj_class->new(shift); $x->bexp(@_) }

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
