# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

package Scalar::Util;
use strict;
use warnings;
use Exporter 'import';

our @EXPORT_OK = qw(
    blessed reftype weaken isweak looks_like_number
    readonly tainted dualvar isdual isvstring openhandle
    set_prototype
);

our $VERSION = '1.63';

sub blessed {
    my ($ref) = @_;
    return undef unless defined $ref && ref($ref);
    return ref($ref);
}

sub reftype {
    my ($ref) = @_;
    return undef unless ref($ref);
    return 'HASH'  if UNIVERSAL::isa($ref, 'HASH');
    return 'ARRAY' if UNIVERSAL::isa($ref, 'ARRAY');
    return 'CODE'  if UNIVERSAL::isa($ref, 'CODE');
    return ref($ref);
}

sub weaken { }
sub isweak { 0 }

sub looks_like_number {
    my ($val) = @_;
    return 0 unless defined $val;
    return $val =~ /^\s*[+-]?(?:\d+\.?\d*|\.\d+)(?:[Ee][+-]?\d+)?\s*$/;
}

sub readonly { 0 }
sub tainted  { 0 }

sub dualvar {
    my ($num, $str) = @_;
    # Return a scalar whose string value is $str and numeric value is $num.
    # PCL: not($dualvar) uses string truthiness, so returning $str is correct
    # for boolean operations. Full NV/SV dualvar semantics are in p-dualvar (CL).
    return $str;
}

sub isdual    { 0 }
sub isvstring { 0 }
sub openhandle { $_[0] }
sub set_prototype { }

1;
