# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pcl-shim: must-win -- the real module is XS.  This shim is found BEFORE @INC is
# searched, so a PERL5LIB or -I copy of the real module cannot shadow it
# (task #2462, docs/ir-spec.md 9, docs/shipped-modules.md).

# Pure-Perl Clone (task #3826): the real module (0.47) is one XS function,
# rv_clone(), that walks a structure and copies it.  The same walk in Perl:
#   - a plain value is returned as a copy;
#   - an ARRAY / HASH / SCALAR / REF is copied recursively and a blessed one
#     is re-blessed into the same class;
#   - CODE, GLOB, IO, FORMAT and Regexp referents are SHARED (Clone.xs keeps
#     the same referent for them);
#   - a structure that refers to itself is copied ONCE: a referent seen
#     before maps to its copy (Clone.xs's `hseen` table), so a cycle stays a
#     cycle and two references to one referent stay two references to one;
#   - clone($ref, $depth): copy $depth levels and share below them
#     (Clone.xs decrements depth per level and returns the referent itself at
#     0); no depth = unlimited.
# HTTP::Headers (`use Clone qw(clone)`) is the consumer that made it needed.

package Clone;
use strict;
use warnings;
use Scalar::Util qw(blessed reftype refaddr);
require Exporter;
our @ISA       = qw(Exporter);
our @EXPORT;
our @EXPORT_OK = qw(clone);
our $VERSION   = '0.47';

sub clone {
  my ($data, $depth) = @_;
  return $data unless ref $data;
  return _clone($data, defined $depth ? $depth : -1, {});
}

sub _clone {
  my ($ref, $depth, $seen) = @_;
  return $ref unless ref $ref;
  return $ref if $depth == 0;
  my $addr = refaddr($ref);
  return $seen->{$addr} if exists $seen->{$addr};
  my $type  = reftype($ref);
  my $class = blessed($ref);
  return $ref if defined $class && $class eq 'Regexp';
  my $copy;
  if ($type eq 'ARRAY') {
    $copy = [];
    $seen->{$addr} = $copy;
    @$copy = map { _clone($_, $depth - 1, $seen) } @$ref;
  }
  elsif ($type eq 'HASH') {
    $copy = {};
    $seen->{$addr} = $copy;
    %$copy = map { ($_ => _clone($ref->{$_}, $depth - 1, $seen)) } keys %$ref;
  }
  elsif ($type eq 'SCALAR' || $type eq 'REF' || $type eq 'LVALUE' || $type eq 'VSTRING') {
    my $v = $$ref;
    $copy = \$v;
    $seen->{$addr} = $copy;
    $v = _clone($$ref, $depth - 1, $seen) if ref $$ref;
  }
  else {
    # CODE, GLOB, IO, FORMAT, REGEXP: Clone.xs shares the referent.
    return $ref;
  }
  bless $copy, $class if defined $class;
  return $copy;
}

1;
