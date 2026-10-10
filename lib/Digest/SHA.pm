# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pcl-shim: must-win -- the real module is XS.  This shim is found BEFORE @INC is
# searched, so a PERL5LIB or -I copy of the real module cannot shadow it
# (task #2462, docs/ir-spec.md 9, docs/shipped-modules.md).
#
# Digest::SHA without XS (task #2947, batch s514a).  FIPS 180-4 in plain Perl:
# SHA-1, SHA-224, SHA-256, SHA-384, SHA-512, SHA-512/224 and SHA-512/256, the
# functional `shaN` / `shaN_hex` / `shaN_base64`, the HMAC family, and the OO
# interface (new / reset / add / add_bits / addfile / digest / hexdigest /
# b64digest / clone / algorithm / hashsize).
#
# Every 32-bit intermediate is masked with 0xffffffff before it can leave the
# range (PCL's integers are unbounded -- "Integers are unbounded" in
# docs/not-supported.md), and the 64-bit words of the SHA-512 family are kept
# as (high, low) 32-bit HALVES, so no intermediate ever needs more than 35
# bits: the arithmetic is the same under perl and PCL, and every value stays
# a machine integer.
#
# The oracle is perl's XS Digest::SHA: byte-identical digests, base64 without
# padding, `digest` resets the object, a character above 0xFF dies "Wide
# character in subroutine entry", `new` with an unknown algorithm returns undef.
#
# NOT IMPLEMENTED, and loud about it: a bit count that is not a multiple of 8
# (`add_bits` with a partial byte, `addfile` mode "0" of such a file) dies
# naming itself; `getstate` / `putstate` / `dump` / `load` die naming
# themselves (docs/not-supported.md).

package Digest::SHA;
use strict;
use warnings;
use Exporter 'import';

our $VERSION = '6.04';
our $errmsg;
our @EXPORT_OK = qw(
    $errmsg
    hmac_sha1      hmac_sha1_base64      hmac_sha1_hex
    hmac_sha224    hmac_sha224_base64    hmac_sha224_hex
    hmac_sha256    hmac_sha256_base64    hmac_sha256_hex
    hmac_sha384    hmac_sha384_base64    hmac_sha384_hex
    hmac_sha512    hmac_sha512_base64    hmac_sha512_hex
    hmac_sha512224 hmac_sha512224_base64 hmac_sha512224_hex
    hmac_sha512256 hmac_sha512256_base64 hmac_sha512256_hex
    sha1      sha1_base64      sha1_hex
    sha224    sha224_base64    sha224_hex
    sha256    sha256_base64    sha256_hex
    sha384    sha384_base64    sha384_hex
    sha512    sha512_base64    sha512_hex
    sha512224 sha512224_base64 sha512224_hex
    sha512256 sha512256_base64 sha512256_hex);
our @ISA;
eval { require Digest::base; push @ISA, 'Digest::base'; 1 };

my $M32 = 0xffffffff;
my $T32 = 4294967296;

my @K256 = (
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1,
    0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786,
    0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147,
    0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
    0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a,
    0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
);

# The SHA-512 round constants as 16-hex-digit words, split into halves below.
my @K512 = map { (hex(substr($_, 0, 8)), hex(substr($_, 8, 8))) } qw(
    428a2f98d728ae22 7137449123ef65cd b5c0fbcfec4d3b2f e9b5dba58189dbbc
    3956c25bf348b538 59f111f1b605d019 923f82a4af194f9b ab1c5ed5da6d8118
    d807aa98a3030242 12835b0145706fbe 243185be4ee4b28c 550c7dc3d5ffb4e2
    72be5d74f27b896f 80deb1fe3b1696b1 9bdc06a725c71235 c19bf174cf692694
    e49b69c19ef14ad2 efbe4786384f25e3 0fc19dc68b8cd5b5 240ca1cc77ac9c65
    2de92c6f592b0275 4a7484aa6ea6e483 5cb0a9dcbd41fbd4 76f988da831153b5
    983e5152ee66dfab a831c66d2db43210 b00327c898fb213f bf597fc7beef0ee4
    c6e00bf33da88fc2 d5a79147930aa725 06ca6351e003826f 142929670a0e6e70
    27b70a8546d22ffc 2e1b21385c26c926 4d2c6dfc5ac42aed 53380d139d95b3df
    650a73548baf63de 766a0abb3c77b2a8 81c2c92e47edaee6 92722c851482353b
    a2bfe8a14cf10364 a81a664bbc423001 c24b8b70d0f89791 c76c51a30654be30
    d192e819d6ef5218 d69906245565a910 f40e35855771202a 106aa07032bbd1b8
    19a4c116b8d2d0c8 1e376c085141ab53 2748774cdf8eeb99 34b0bcb5e19b48a8
    391c0cb3c5c95a63 4ed8aa4ae3418acb 5b9cca4f7763e373 682e6ff3d6b2b8a3
    748f82ee5defb2fc 78a5636f43172f60 84c87814a1f0ab72 8cc702081a6439ec
    90befffa23631e28 a4506cebde82bde9 bef9a3f7b2c67915 c67178f2e372532b
    ca273eceea26619c d186b8c721c0c207 eada7dd6cde0eb1e f57d4f7fee6ed178
    06f067aa72176fba 0a637dc5a2c898a6 113f9804bef90dae 1b710b35131c471b
    28db77f523047d84 32caab7b40c72493 3c9ebe0a15c9bebc 431d67c49c100d4c
    4cc5d4becb3e42b6 597f299cfc657e2a 5fcb6fab3ad6faec 6c44198c4a475817
);

sub _halves { map { (hex(substr($_, 0, 8)), hex(substr($_, 8, 8))) } @_ }

# alg => [initial state, block bytes, digest bytes, hash bits]
my %ALG = (
    1   => [[0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476, 0xc3d2e1f0], 64, 20, 160],
    224 => [[0xc1059ed8, 0x367cd507, 0x3070dd17, 0xf70e5939,
             0xffc00b31, 0x68581511, 0x64f98fa7, 0xbefa4fa4], 64, 28, 224],
    256 => [[0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
             0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19], 64, 32, 256],
    384 => [[_halves(qw(cbbb9d5dc1059ed8 629a292a367cd507 9159015a3070dd17
                        152fecd8f70e5939 67332667ffc00b31 8eb44a8768581511
                        db0c2e0d64f98fa7 47b5481dbefa4fa4))], 128, 48, 384],
    512 => [[_halves(qw(6a09e667f3bcc908 bb67ae8584caa73b 3c6ef372fe94f82b
                        a54ff53a5f1d36f1 510e527fade682d1 9b05688c2b3e6c1f
                        1f83d9abfb41bd6b 5be0cd19137e2179))], 128, 64, 512],
    512224 => [[_halves(qw(8c3d37c819544da2 73e1996689dcd4d6 1dfab7ae32ff9c82
                           679dd514582f9fcf 0f6d2b697bd44da8 77e36f7304c48942
                           3f9d85a86a1d36c8 1112e6ad91d692a1))], 128, 28, 224],
    512256 => [[_halves(qw(22312194fc2bf72c 9f555fa3c84c64c2 2393b86b6f53b151
                           963877195940eabd 96283ee2a88effe3 be5e1e2553863992
                           2b0199fc2c85b8aa 0eb72ddc81c52ca2))], 128, 32, 256],
);

# ---- the compression functions: fold $n blocks of $data into the state $h ----

sub _rotl32 { (($_[0] << $_[1]) & $M32) | ($_[0] >> (32 - $_[1])) }

sub _sha1_blocks {
    my ($h, $data, $n) = @_;
    my ($h0, $h1, $h2, $h3, $h4) = @$h;
    for my $blk (0 .. $n - 1) {
        my @w = unpack('N16', substr($data, $blk * 64, 64));
        for my $t (16 .. 79) {
            my $x = $w[$t - 3] ^ $w[$t - 8] ^ $w[$t - 14] ^ $w[$t - 16];
            $w[$t] = (($x << 1) & $M32) | ($x >> 31);
        }
        my ($A, $B, $C, $D, $E) = ($h0, $h1, $h2, $h3, $h4);
        for my $t (0 .. 79) {
            my ($f, $k);
            if ($t < 20)    { $f = $D ^ ($B & ($C ^ $D)); $k = 0x5a827999 }
            elsif ($t < 40) { $f = $B ^ $C ^ $D;          $k = 0x6ed9eba1 }
            elsif ($t < 60) { $f = ($B & $C) | ($D & ($B | $C)); $k = 0x8f1bbcdc }
            else            { $f = $B ^ $C ^ $D;          $k = 0xca62c1d6 }
            my $tmp = ((((($A << 5) & $M32) | ($A >> 27)) + $f + $E + $k + $w[$t]) & $M32);
            ($E, $D, $C, $B, $A) = ($D, $C, ((($B << 30) & $M32) | ($B >> 2)), $A, $tmp);
        }
        $h0 = ($h0 + $A) & $M32;
        $h1 = ($h1 + $B) & $M32;
        $h2 = ($h2 + $C) & $M32;
        $h3 = ($h3 + $D) & $M32;
        $h4 = ($h4 + $E) & $M32;
    }
    @$h = ($h0, $h1, $h2, $h3, $h4);
}

sub _rotr32 { ($_[0] >> $_[1]) | (($_[0] << (32 - $_[1])) & $M32) }

sub _sha256_blocks {
    my ($h, $data, $n) = @_;
    my @H = @$h;
    for my $blk (0 .. $n - 1) {
        my @w = unpack('N16', substr($data, $blk * 64, 64));
        for my $t (16 .. 63) {
            my $x = $w[$t - 15];
            my $y = $w[$t - 2];
            my $s0 = _rotr32($x, 7) ^ _rotr32($x, 18) ^ ($x >> 3);
            my $s1 = _rotr32($y, 17) ^ _rotr32($y, 19) ^ ($y >> 10);
            $w[$t] = ($w[$t - 16] + $s0 + $w[$t - 7] + $s1) & $M32;
        }
        my ($A, $B, $C, $D, $E, $F, $G, $HH) = @H;
        for my $t (0 .. 63) {
            my $S1 = _rotr32($E, 6) ^ _rotr32($E, 11) ^ _rotr32($E, 25);
            my $ch = $G ^ ($E & ($F ^ $G));
            my $t1 = ($HH + $S1 + $ch + $K256[$t] + $w[$t]) & $M32;
            my $S0 = _rotr32($A, 2) ^ _rotr32($A, 13) ^ _rotr32($A, 22);
            my $maj = ($A & $B) | ($C & ($A | $B));
            ($HH, $G, $F, $E, $D, $C, $B) = ($G, $F, $E, ($D + $t1) & $M32, $C, $B, $A);
            $A = ($t1 + $S0 + $maj) & $M32;
        }
        my @v = ($A, $B, $C, $D, $E, $F, $G, $HH);
        $H[$_] = ($H[$_] + $v[$_]) & $M32 for 0 .. 7;
    }
    @$h = @H;
}

# 64-bit rotate right of the halves ($hi, $lo) by $n (0 < $n < 64, $n != 32).
sub _rotr64 {
    my ($hi, $lo, $n) = @_;
    ($hi, $lo, $n) = ($lo, $hi, $n - 32) if $n > 32;
    return ((($hi >> $n) | (($lo << (32 - $n)) & $M32)),
            (($lo >> $n) | (($hi << (32 - $n)) & $M32)));
}

sub _sha512_blocks {
    my ($h, $data, $n) = @_;
    my @H = @$h;
    for my $blk (0 .. $n - 1) {
        my @in = unpack('N32', substr($data, $blk * 128, 128));
        my (@wh, @wl);
        for my $t (0 .. 15) { $wh[$t] = $in[2 * $t]; $wl[$t] = $in[2 * $t + 1] }
        for my $t (16 .. 79) {
            my ($xh, $xl) = ($wh[$t - 15], $wl[$t - 15]);
            my ($a1h, $a1l) = _rotr64($xh, $xl, 1);
            my ($a2h, $a2l) = _rotr64($xh, $xl, 8);
            my $s0h = $a1h ^ $a2h ^ ($xh >> 7);
            my $s0l = $a1l ^ $a2l ^ ((($xl >> 7) | (($xh << 25) & $M32)));
            my ($yh, $yl) = ($wh[$t - 2], $wl[$t - 2]);
            my ($b1h, $b1l) = _rotr64($yh, $yl, 19);
            my ($b2h, $b2l) = _rotr64($yh, $yl, 61);
            my $s1h = $b1h ^ $b2h ^ ($yh >> 6);
            my $s1l = $b1l ^ $b2l ^ ((($yl >> 6) | (($yh << 26) & $M32)));
            my $lo = $wl[$t - 16] + $s0l + $wl[$t - 7] + $s1l;
            $wl[$t] = $lo & $M32;
            $wh[$t] = ($wh[$t - 16] + $s0h + $wh[$t - 7] + $s1h + int($lo / $T32)) & $M32;
        }
        my ($ah, $al, $bh, $bl, $ch, $cl, $dh, $dl,
            $eh, $el, $fh, $fl, $gh, $gl, $hh, $hl) = @H;
        for my $t (0 .. 79) {
            my ($r1h, $r1l) = _rotr64($eh, $el, 14);
            my ($r2h, $r2l) = _rotr64($eh, $el, 18);
            my ($r3h, $r3l) = _rotr64($eh, $el, 41);
            my $S1h = $r1h ^ $r2h ^ $r3h;
            my $S1l = $r1l ^ $r2l ^ $r3l;
            my $chh = $gh ^ ($eh & ($fh ^ $gh));
            my $chl = $gl ^ ($el & ($fl ^ $gl));
            my $t1l = $hl + $S1l + $chl + $K512[2 * $t + 1] + $wl[$t];
            my $t1h = ($hh + $S1h + $chh + $K512[2 * $t] + $wh[$t] + int($t1l / $T32)) & $M32;
            $t1l &= $M32;
            my ($q1h, $q1l) = _rotr64($ah, $al, 28);
            my ($q2h, $q2l) = _rotr64($ah, $al, 34);
            my ($q3h, $q3l) = _rotr64($ah, $al, 39);
            my $S0h = $q1h ^ $q2h ^ $q3h;
            my $S0l = $q1l ^ $q2l ^ $q3l;
            my $mjh = ($ah & $bh) | ($ch & ($ah | $bh));
            my $mjl = ($al & $bl) | ($cl & ($al | $bl));
            ($hh, $hl, $gh, $gl, $fh, $fl) = ($gh, $gl, $fh, $fl, $eh, $el);
            my $el2 = $dl + $t1l;
            ($eh, $el) = (($dh + $t1h + int($el2 / $T32)) & $M32, $el2 & $M32);
            ($dh, $dl, $ch, $cl, $bh, $bl) = ($ch, $cl, $bh, $bl, $ah, $al);
            my $al2 = $t1l + $S0l + $mjl;
            ($ah, $al) = (($t1h + $S0h + $mjh + int($al2 / $T32)) & $M32, $al2 & $M32);
        }
        my @v = ($ah, $al, $bh, $bl, $ch, $cl, $dh, $dl,
                 $eh, $el, $fh, $fl, $gh, $gl, $hh, $hl);
        for (my $i = 14; $i >= 0; $i -= 2) {
            my $lo = $H[$i + 1] + $v[$i + 1];
            $H[$i + 1] = $lo & $M32;
            $H[$i] = ($H[$i] + $v[$i] + int($lo / $T32)) & $M32;
        }
    }
    @$h = @H;
}

# ---- the state object: {alg, h, buf, len} -----------------------------------

sub _wide_check {
    if ($_[0] =~ /[^\x00-\xff]/) {
        require Carp;
        Carp::croak('Wide character in subroutine entry');
    }
}

sub _canon_alg {
    my ($alg) = @_;
    return 1 if !defined $alg;
    $alg =~ s/\D+//g;
    return (length $alg && exists $ALG{$alg}) ? $alg + 0 : undef;
}

sub _init {
    my ($st, $alg) = @_;
    $st->{alg} = $alg;
    $st->{h}   = [@{ $ALG{$alg}[0] }];
    $st->{buf} = '';
    $st->{len} = 0;
    return $st;
}

sub _compress {
    my ($st, $data, $n) = @_;
    my $alg = $st->{alg};
    if ($alg == 1)      { _sha1_blocks($st->{h}, $data, $n) }
    elsif ($alg <= 256) { _sha256_blocks($st->{h}, $data, $n) }
    else                { _sha512_blocks($st->{h}, $data, $n) }
}

sub _write {
    my ($st, $data) = @_;
    _wide_check($data);
    $st->{len} += length $data;
    my $bs = $ALG{ $st->{alg} }[1];
    my $buf = $st->{buf} . $data;
    my $n = int(length($buf) / $bs);
    if ($n) {
        _compress($st, $buf, $n);
        $buf = substr($buf, $n * $bs);
    }
    $st->{buf} = $buf;
}

sub _final {
    my ($st) = @_;
    my ($init, $bs, $dbytes) = @{ $ALG{ $st->{alg} } };
    my $bits = $st->{len} * 8;
    my $lenfield = pack('NN', int($bits / $T32) % $T32, $bits % $T32);
    $lenfield = ("\0" x 8) . $lenfield if $bs == 128;
    my $buf = $st->{buf} . "\x80";
    my $room = $bs - length $lenfield;
    $buf .= "\0" x (($room - length($buf) % $bs) % $bs);
    $buf .= $lenfield;
    my %copy = (%$st, h => [@{ $st->{h} }]);
    _compress(\%copy, $buf, length($buf) / $bs);
    return substr(pack('N*', @{ $copy{h} }), 0, $dbytes);
}

sub _b64 {
    require MIME::Base64;
    my $s = MIME::Base64::encode_base64($_[0], '');
    $s =~ s/=+\z//;
    return $s;
}

sub _digest_of {
    my $alg = shift;
    my $st = _init({}, $alg);
    _write($st, join('', @_));
    return _final($st);
}

sub _hmac {
    my $alg = shift;
    my $key = @_ ? pop : '';
    $key = '' if !defined $key;
    _wide_check($key);
    my $bs = $ALG{$alg}[1];
    $key = _digest_of($alg, $key) if length $key > $bs;
    $key .= "\0" x ($bs - length $key);
    # Byte by byte, not `$key ^ ("\x36" x $bs)`: the ipad string is "666...",
    # which LOOKS like a number, and PCL then takes the numeric `^` (#1040).
    my @k = unpack('C*', $key);
    my $inner = _digest_of($alg, pack('C*', map { $_ ^ 0x36 } @k), @_);
    return _digest_of($alg, pack('C*', map { $_ ^ 0x5c } @k), $inner);
}

# The functional interface: shaN, shaN_hex, shaN_base64, hmac_shaN[...].
for my $alg (1, 224, 256, 384, 512, 512224, 512256) {
    no strict 'refs';
    my $name = "sha$alg";
    *{$name}             = sub { _digest_of($alg, @_) };
    *{"${name}_hex"}     = sub { unpack('H*', _digest_of($alg, @_)) };
    *{"${name}_base64"}  = sub { _b64(_digest_of($alg, @_)) };
    *{"hmac_$name"}        = sub { _hmac($alg, @_) };
    *{"hmac_${name}_hex"}  = sub { unpack('H*', _hmac($alg, @_)) };
    *{"hmac_${name}_base64"} = sub { _b64(_hmac($alg, @_)) };
}

# ---- the OO interface -------------------------------------------------------

sub new {
    my ($class, $alg) = @_;
    # An instance with no algorithm keeps its own (Digest::SHA's `sharewind`).
    $alg = $class->{alg} if ref $class && !defined $alg;
    my $canon = _canon_alg($alg);
    return undef if !defined $canon;
    if (ref $class) {
        _init($class, $canon);
        return $class;
    }
    return bless _init({}, $canon), $class;
}

sub reset { my $self = shift; $self->new(@_) }

sub clone {
    my $self = shift;
    return bless { %$self, h => [@{ $self->{h} }] }, ref $self;
}

sub algorithm { $_[0]{alg} }
sub hashsize  { $ALG{ $_[0]{alg} }[3] }

sub add {
    my $self = shift;
    _write($self, $_) for @_;
    return $self;
}

sub add_bits {
    my ($self, $data, $nbits) = @_;
    if (!defined $nbits) {
        $nbits = length $data;
        $data = pack('B*', $data);
    }
    $nbits = length($data) * 8 if $nbits > length($data) * 8;
    if ($nbits % 8) {
        require Carp;
        Carp::croak("Digest::SHA::add_bits: a bit count that is not a multiple of 8 ($nbits) is not implemented in PCL");
    }
    _write($self, substr($data, 0, $nbits / 8));
    return $self;
}

sub _bail {
    my $msg = shift;
    $errmsg = $!;
    require Carp;
    Carp::croak("$msg: $!");
}

sub _addfh {
    my ($self, $fh) = @_;
    my ($n, $buf);
    while (($n = read($fh, $buf, 65536))) {
        _write($self, $buf);
    }
    _bail('Read failed') if !defined $n;
    return $self;
}

sub addfile {
    my ($self, $file, $mode) = @_;
    return _addfh($self, $file) if ref(\$file) ne 'SCALAR';
    $mode = '' if !defined $mode;
    my $fh;
    if ($file eq '-') {
        open($fh, '<&', \*STDIN) or _bail('Open failed');
    }
    else {
        open($fh, '<', $file) or _bail('Open failed');
    }
    if ($mode eq '0') {
        my ($n, $buf);
        while (($n = read($fh, $buf, 4096))) {
            $buf =~ tr/01//cd;
            $self->add_bits($buf);
        }
        _bail('Read failed') if !defined $n;
        close $fh;
        return $self;
    }
    binmode $fh if $mode eq 'b' || $mode eq 'U';
    if ($mode eq 'U' && -T $file) {
        local $/;
        my $all = <$fh>;
        $all = '' if !defined $all;
        $all =~ s/\015\012?/\012/g;
        _write($self, $all);
    }
    else {
        _addfh($self, $fh);
    }
    close $fh;
    return $self;
}

sub digest {
    my $self = shift;
    my $d = _final($self);
    _init($self, $self->{alg});
    return $d;
}
sub hexdigest { unpack('H*', $_[0]->digest) }
sub b64digest { _b64($_[0]->digest) }

for my $m (qw(getstate putstate dump load)) {
    no strict 'refs';
    *{$m} = sub {
        require Carp;
        Carp::croak("Digest::SHA::$m is not implemented in PCL (docs/not-supported.md)");
    };
}

1;
