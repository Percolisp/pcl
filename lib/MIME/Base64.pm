# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pcl-shim: must-win -- the real module is XS.  This shim is found BEFORE @INC is
# searched, so a PERL5LIB or -I copy of the real module cannot shadow it
# (task #2462, docs/ir-spec.md 9, docs/shipped-modules.md).
#
# MIME::Base64 without XS (task #2079-family, s492c).  The real module is XS,
# so `use MIME::Base64;` died "Can't locate MIME/Base64.pm in @INC (the module
# is XS and has no PCL build)" -- and base64 is in every script that talks to
# an HTTP API, stores a binary blob in a text field or reads a Basic-Auth
# header.
#
# The 3-byte <-> 4-character transform itself is the runtime's (s540e, #4990:
# `builtin::base64_encode(BYTES, EOL)` and `builtin::base64_decode(STRING)`,
# ~200x the Perl loop this file carried until then); what stays here is the
# module -- the prototypes, the `$eol` default, the empty / undef answers,
# the wide-character croak, the length functions and the url variants.

package MIME::Base64;
use strict;
use warnings;
use Exporter 'import';

our $VERSION = '3.16';
our @EXPORT    = qw(encode_base64 decode_base64);
our @EXPORT_OK = qw(encode_base64url decode_base64url
                    encoded_base64_length decoded_base64_length);

sub encode_base64 ($;$) {
    my ($data, $eol) = @_;
    $eol = "\n" if !defined $eol;
    return '' if !defined $data || $data eq '';
    my $res = builtin::base64_encode($data, $eol);
    if (!defined $res) {
        require Carp;
        Carp::croak("The Base64 encoder requires that no data contains characters "
                    . "with code above 255");
    }
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
    return builtin::base64_decode($str);
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
# perl's MIME::Base64 aliases (`MIME::Base64::encode($s)` is documented; #2066,
# Data::Dump's base64 branch calls it).
*encode = \&encode_base64;
*decode = \&decode_base64;

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
