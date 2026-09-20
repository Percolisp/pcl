# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later
#
# MIME::Base64 without XS (task #2079-family, s492c).  The real module is XS,
# so `use MIME::Base64;` died "Can't locate MIME/Base64.pm in @INC (the module
# is XS and has no PCL build)" -- and base64 is in every script that talks to
# an HTTP API, stores a binary blob in a text field or reads a Basic-Auth
# header.
#
# The encoder/decoder here are the plain 3-byte <-> 4-character transform, not
# perl's `pack "u"` trick: the trick is faster but depends on uuencode's exact
# padding, and a straight loop is what can be read against RFC 4648.

package MIME::Base64;
use strict;
use warnings;
use Exporter 'import';

our $VERSION = '3.16';
our @EXPORT    = qw(encode_base64 decode_base64);
our @EXPORT_OK = qw(encode_base64url decode_base64url
                    encoded_base64_length decoded_base64_length);

my @B64 = ('A' .. 'Z', 'a' .. 'z', '0' .. '9', '+', '/');
my %B64R;
@B64R{@B64} = (0 .. 63);

sub encode_base64 ($;$) {
    my ($data, $eol) = @_;
    $eol = "\n" if !defined $eol;
    return '' if !defined $data || $data eq '';
    require Carp;
    Carp::croak("The Base64 encoder requires that no data contains characters "
                . "with code above 255")
      if $data =~ /[^\x00-\xff]/;
    my $res = '';
    my $len = length $data;
    for (my $i = 0; $i < $len; $i += 3) {
        my $n = $len - $i; $n = 3 if $n > 3;
        my $b0 = ord(substr($data, $i, 1));
        my $b1 = $n > 1 ? ord(substr($data, $i + 1, 1)) : 0;
        my $b2 = $n > 2 ? ord(substr($data, $i + 2, 1)) : 0;
        my $v = ($b0 << 16) | ($b1 << 8) | $b2;
        $res .= $B64[($v >> 18) & 63] . $B64[($v >> 12) & 63]
              . ($n > 1 ? $B64[($v >> 6) & 63] : '=')
              . ($n > 2 ? $B64[$v & 63]        : '=');
    }
    $res =~ s/(.{1,76})/$1$eol/g if length $eol;
    return $res;
}

# perl's decoder IGNORES every character outside the alphabet (that is what
# lets it read a wrapped MIME body) and STOPS AT THE FIRST `=`, wherever it
# sits: `decode_base64("aa=aa")` is one byte, not three (probed 5.40.3, and
# MIME-Base64's own t/length.t rows 22-25 are exactly this).
sub _b64_significant {
    my $str = shift;
    $str =~ tr{A-Za-z0-9+/=}{}cd;
    $str =~ s/=.*\z//s;
    return $str;
}

sub decode_base64 ($) {
    my $str = shift;
    return '' if !defined $str;
    $str = _b64_significant($str);
    my $res = '';
    my $len = length $str;
    for (my $i = 0; $i < $len; $i += 4) {
        my $n = $len - $i; $n = 4 if $n > 4;
        last if $n < 2;                      # a lone trailing character is noise
        my $c0 = $B64R{substr($str, $i, 1)};
        my $c1 = $B64R{substr($str, $i + 1, 1)};
        my $c2 = $n > 2 ? $B64R{substr($str, $i + 2, 1)} : 0;
        my $c3 = $n > 3 ? $B64R{substr($str, $i + 3, 1)} : 0;
        my $v = ($c0 << 18) | ($c1 << 12) | ($c2 << 6) | $c3;
        $res .= chr(($v >> 16) & 255);
        $res .= chr(($v >> 8) & 255) if $n > 2;
        $res .= chr($v & 255)        if $n > 3;
    }
    return $res;
}

sub encoded_base64_length ($;$) {
    my ($data, $eol) = @_;
    $eol = "\n" if !defined $eol;
    return 0 if !defined $data || $data eq '';
    my $groups = int((length($data) + 2) / 3);
    my $chars  = $groups * 4;
    my $lines  = int(($chars + 75) / 76);
    return $chars + $lines * length($eol);
}

sub decoded_base64_length ($) {
    my $str = shift;
    return 0 if !defined $str;
    $str = _b64_significant($str);
    my $len = length $str;
    my $full = int($len / 4);
    my $rest = $len % 4;
    return $full * 3 + ($rest > 1 ? $rest - 1 : 0);
}

# RFC 4648 §5: the URL/filename-safe alphabet, and NO padding.
sub encode_base64url {
    my $e = encode_base64($_[0], '');
    $e =~ tr{+/}{-_};
    $e =~ s/=+\z//;
    return $e;
}

sub decode_base64url {
    my $s = shift;
    return '' if !defined $s;
    $s =~ tr{-_}{+/};
    return decode_base64($s);
}

1;
