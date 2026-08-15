# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package version;
use strict;
use warnings;

use overload
    '""'  => \&stringify,
    '0+'  => \&numify,
    'cmp' => \&vcmp,
    '<=>' => \&vcmp,
    fallback => 1;

our $VERSION = '0.9933';

sub new {
    my ($class, $val) = @_;
    $val //= '';
    # Store as string
    my $str = "$val";
    return bless { string => $str, original => $val }, $class;
}

# Shorthand: version->parse same as version->new
sub parse { shift->new(@_) }

sub stringify { $_[0]->{string} }

sub numify {
    my $self = shift;
    my $s = $self->{string};
    return $s + 0;
}

sub vcmp {
    my ($a, $b, $swap) = @_;
    $a = "$a";
    $b = ref($b) ? "$b" : $b;
    ($a, $b) = ($b, $a) if $swap;
    return $a cmp $b;
}

sub is_alpha { 0 }
sub is_qv    { 0 }

# ---------------------------------------------------------------------------
# is_strict / is_lax — the two version-string acceptance predicates.  Perl
# ships them in version/regex.pm, which builds each pattern by interpolating
# qr// objects into other qr// objects; the patterns below are those same
# grammars expanded into one literal each, so there is no qr-in-qr to compile.
#
#   STRICT = STRICT_DECIMAL | STRICT_DOTTED_DECIMAL
#     STRICT_DECIMAL        = (0|[1-9][0-9]*) (\.[0-9]+)?      no leading zeros
#     STRICT_DOTTED_DECIMAL = v (0|[1-9][0-9]*) (\.[0-9]{1,3}){2,}
#                                          leading v AND at least three parts
#
#   LAX = undef | LAX_DOTTED_DECIMAL | LAX_DECIMAL
#     LAX_DOTTED_DECIMAL = v [0-9]+ ((\.[0-9]+)+ (_[0-9]+)?)?
#                        | [0-9]*   (\.[0-9]+){2,} (_[0-9]+)?
#     LAX_DECIMAL        = [0-9]+ ((\.[0-9]+)|\.)? (_[0-9]+)?
#                        | (\.[0-9]+) (_[0-9]+)?
#
# VERIFIED against the real `version::` (perl 5.40.3) over all 40 strings
# t/op/packagev.t feeds these two, plus 12 more (empty string, 1.2.3.4.5,
# v1.2.3_4, 1..2, 0.0.0, v1.02_03_04, 1.0_0, v9.9.9.9.9, .5, 5., 007,
# v1.234.5): every answer agrees, both predicates.
our $STRICT = qr/\A(?:(?:0|[1-9][0-9]*)(?:\.[0-9]+)?|v(?:0|[1-9][0-9]*)(?:\.[0-9]{1,3}){2,})\z/;
our $LAX    = qr/\A(?:undef|v[0-9]+(?:(?:\.[0-9]+)+(?:_[0-9]+)?)?|(?:[0-9]+)?(?:\.[0-9]+){2,}(?:_[0-9]+)?|[0-9]+(?:(?:\.[0-9]+)|\.)?(?:_[0-9]+)?|(?:\.[0-9]+)(?:_[0-9]+)?)\z/;

sub is_strict { defined $_[0] && $_[0] =~ $STRICT ? 1 : 0 }
sub is_lax    { defined $_[0] && $_[0] =~ $LAX    ? 1 : 0 }

1;
