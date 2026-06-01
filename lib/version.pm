# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

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

1;
