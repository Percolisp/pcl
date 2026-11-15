# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pcl-shim: must-win -- the real module is XS.  This shim is found BEFORE @INC is
# searched, so a PERL5LIB or -I copy of the real module cannot shadow it
# (task #2462, docs/ir-spec.md 9, docs/shipped-modules.md).
#
# Digest::MD5 without XS (task #2947, batch s514a).  The real module is XS, so
# `use Digest::MD5` died "Can't locate ... (the module is XS and has no PCL
# build)" -- and an MD5 checksum is in every script that verifies a download,
# keys a cache on content or talks to an API that signs its requests.
#
# The digest itself is the runtime's (s540e, #4988): `builtin::md5_digest` for
# a whole string and `builtin::md5_update` (whole 64-byte blocks into the four
# chaining words) for the incremental object -- both over the sb-md5 contrib
# the runtime already loads, ~50x the pure-Perl RFC 1321 compression this file
# carried until then (s514a).  What stays here is the module: the API, the
# object as plain data (the chaining words, the pending partial block, the
# byte count -- so `clone` is a copy), RFC 1321's padding and length block,
# and perl's message for a character above 0xFF (the runtime answers
# "nothing" for one and this file croaks).
#
# The oracle is perl's own XS Digest::MD5: byte-identical digests, the same
# functional and OO API, `digest` resets the object, base64 without padding,
# and a character above 0xFF dies "Wide character in subroutine entry".
#
# NOT IMPLEMENTED, and loud about it: `context` (the XS state get/set) dies
# naming itself.

package Digest::MD5;
use strict;
use warnings;
use Exporter 'import';

our $VERSION = '2.58_01';
our @EXPORT_OK = qw(md5 md5_hex md5_base64);
our @ISA;
eval { require Digest::base; @ISA = qw(Digest::base); 1 };

sub _wide {
    require Carp;
    Carp::croak('Wide character in subroutine entry');
}

sub _fresh { { h => [0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476], buf => '', len => 0 } }

sub _add {
    my ($st, $data) = @_;
    my @r = builtin::md5_update(@{ $st->{h} }, $st->{buf}, $data);
    _wide() if !@r;
    $st->{buf} = pop @r;
    $st->{h} = \@r;
    $st->{len} += length $data;
}

sub _final {
    my ($st) = @_;
    my $bits = $st->{len} * 8;
    my $buf = $st->{buf} . "\x80";
    $buf .= "\0" x ((56 - length($buf) % 64) % 64);
    $buf .= pack('VV', $bits % 4294967296, int($bits / 4294967296) % 4294967296);
    my @r = builtin::md5_update(@{ $st->{h} }, '', $buf);
    return pack('V4', @r[0 .. 3]);
}

sub _b64 {
    require MIME::Base64;
    my $s = MIME::Base64::encode_base64($_[0], '');
    $s =~ s/=+\z//;
    return $s;
}

sub md5 {
    my $d = builtin::md5_digest(join('', @_));
    _wide() if !defined $d;
    return $d;
}
# The hex form through a byte table: `unpack('H*', ...)` of 16 bytes costs
# ~19 us under PCL (pack runs as transpiled Perl), the table ~2 us (s540e).
my @HEX = map { sprintf('%02x', $_) } 0 .. 255;
sub _hex { my ($d) = @_; join('', map { $HEX[ord(substr($d, $_, 1))] } 0 .. length($d) - 1) }

sub md5_hex    { _hex(md5(@_)) }
sub md5_base64 { _b64(md5(@_)) }

sub new {
    my $class = shift;
    if (ref $class) {
        %$class = %{ _fresh() };
        return $class;
    }
    return bless _fresh(), $class;
}

sub reset { $_[0]->new }

sub clone {
    my $self = shift;
    return bless { h => [@{ $self->{h} }], buf => $self->{buf}, len => $self->{len} }, ref $self;
}

sub add {
    my $self = shift;
    _add($self, $_) for @_;
    return $self;
}

sub addfile {
    my ($self, $fh) = @_;
    my ($n, $buf);
    while (($n = read($fh, $buf, 65536))) {
        _add($self, $buf);
    }
    if (!defined $n) {
        require Carp;
        Carp::croak("Reading from filehandle failed");
    }
    return $self;
}

sub digest {
    my $self = shift;
    my $D = _final($self);
    $self->new;
    return $D;
}
sub hexdigest { _hex($_[0]->digest) }
sub b64digest { _b64($_[0]->digest) }

sub context {
    require Carp;
    Carp::croak('Digest::MD5::context is not implemented in PCL (docs/not-supported.md)');
}

1;
