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
# RFC 1321 in plain Perl.  Every 32-bit intermediate is masked with 0xffffffff
# BEFORE it can leave the range (PCL's integers are unbounded -- see
# "Integers are unbounded" in docs/not-supported.md -- and an explicit mask is
# what makes the arithmetic identical under both perl and PCL), and `~` is
# never used (it would be a 64-bit complement): `~x & m` is written `x ^ m`.
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

my $M32 = 0xffffffff;

# Per-step shift amounts and the sine-derived constants (RFC 1321 3.4).
my @S = (7, 12, 17, 22) x 4;
push @S, (5, 9, 14, 20) x 4;
push @S, (4, 11, 16, 23) x 4;
push @S, (6, 10, 15, 21) x 4;

my @K = (
    0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee, 0xf57c0faf, 0x4787c62a,
    0xa8304613, 0xfd469501, 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
    0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821, 0xf61e2562, 0xc040b340,
    0x265e5a51, 0xe9b6c7aa, 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
    0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed, 0xa9e3e905, 0xfcefa3f8,
    0x676f02d9, 0x8d2a4c8a, 0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
    0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70, 0x289b7ec6, 0xeaa127fa,
    0xd4ef3085, 0x04881d05, 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
    0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039, 0x655b59c3, 0x8f0ccc92,
    0xffeff47d, 0x85845dd1, 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
    0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391,
);

# The message-word index each step reads.
my @G = (0 .. 15,
         map({ (5 * $_ + 1) % 16 } 0 .. 15),
         map({ (3 * $_ + 5) % 16 } 0 .. 15),
         map({ (7 * $_) % 16 } 0 .. 15));

# Fold the 64-byte blocks of $data (from $off, a multiple of 64 bytes) into $h.
sub _blocks {
    my ($h, $data, $nblocks) = @_;
    my ($h0, $h1, $h2, $h3) = @$h;
    my @x = unpack('V*', substr($data, 0, $nblocks * 64));
    for (my $base = 0; $base < @x; $base += 16) {
        my ($A, $B, $C, $D) = ($h0, $h1, $h2, $h3);
        for my $i (0 .. 63) {
            my $f;
            if ($i < 16)    { $f = $D ^ ($B & ($C ^ $D)) }
            elsif ($i < 32) { $f = $C ^ ($D & ($B ^ $C)) }
            elsif ($i < 48) { $f = $B ^ $C ^ $D }
            else            { $f = $C ^ ($B | ($D ^ $M32)) }
            my $t = ($A + $f + $K[$i] + $x[$base + $G[$i]]) & $M32;
            my $s = $S[$i];
            $t = (($t << $s) & $M32) | ($t >> (32 - $s));
            ($A, $D, $C) = ($D, $C, $B);
            $B = ($B + $t) & $M32;
        }
        $h0 = ($h0 + $A) & $M32;
        $h1 = ($h1 + $B) & $M32;
        $h2 = ($h2 + $C) & $M32;
        $h3 = ($h3 + $D) & $M32;
    }
    @$h = ($h0, $h1, $h2, $h3);
}

sub _wide_check {
    if ($_[0] =~ /[^\x00-\xff]/) {
        require Carp;
        Carp::croak('Wide character in subroutine entry');
    }
}

sub _fresh { { h => [0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476], buf => '', len => 0 } }

sub _add {
    my ($st, $data) = @_;
    _wide_check($data);
    $st->{len} += length $data;
    my $buf = $st->{buf} . $data;
    my $n = int(length($buf) / 64);
    if ($n) {
        _blocks($st->{h}, $buf, $n);
        $buf = substr($buf, $n * 64);
    }
    $st->{buf} = $buf;
}

sub _final {
    my ($st) = @_;
    my $bits = $st->{len} * 8;
    my $buf = $st->{buf} . "\x80";
    $buf .= "\0" x ((56 - length($buf) % 64) % 64);
    $buf .= pack('VV', $bits % 4294967296, int($bits / 4294967296) % 4294967296);
    my @h = @{ $st->{h} };
    _blocks(\@h, $buf, length($buf) / 64);
    return pack('V4', @h);
}

sub _b64 {
    require MIME::Base64;
    my $s = MIME::Base64::encode_base64($_[0], '');
    $s =~ s/=+\z//;
    return $s;
}

sub md5 {
    my $st = _fresh();
    _add($st, join('', @_));
    return _final($st);
}
sub md5_hex    { unpack('H*', md5(@_)) }
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
sub hexdigest { unpack('H*', $_[0]->digest) }
sub b64digest { _b64($_[0]->digest) }

sub context {
    require Carp;
    Carp::croak('Digest::MD5::context is not implemented in PCL (docs/not-supported.md)');
}

1;
