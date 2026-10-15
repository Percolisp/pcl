# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pcl-shim: must-win -- the real module is XS.  This shim is found BEFORE @INC is
# searched, so a PERL5LIB or -I copy of the real module cannot shadow it
# (task #2462, docs/ir-spec.md 9, docs/shipped-modules.md).
#
# Compress::Raw::Zlib without XS (task #3161, batch s517b).
#
# TEMPORARY: libz via sb-alien until pclxs provides Compress::Raw::Zlib (USER
# s516, 2026-10-12: "Do 1 for now, note it as temporary").  The route was
# taken for speed and for zlib's own byte-identical output; nothing in this
# file's API depends on it -- when the XS module runs under pclxs, this file
# is deleted and the real one loads.
#
# The runtime holds ONLY the calls into the system zlib, one builtin::zlib_*
# primitive per zlib function (docs/ir-spec.md "Host libraries"); this file is
# the module: the constants and export lists, the parameter parsing, the
# stream objects and their methods, the output-buffer growth rule, the
# ConsumeInput / LimitOutput / AppendOutput flags and the Z_* status dualvars,
# written after the XS module's Zlib.xs (2.214) so that every return value and
# every write-back matches it.
#
# A stream is a blessed SCALAR ref (as in the XS module) to a hash holding
# the zlib HANDLE.  The z_stream is freed when the stream ENDS -- inflate's
# Z_STREAM_END, deflate's finishing flush -- and in DESTROY; a reset of an
# ended stream initialises a fresh one with the same parameters.  What leaks
# when a stream is abandoned mid-way: docs/not-supported.md "Compress::Raw::Zlib".
#
# NOT IMPLEMENTED, and loud about it: InflateScan / inflateScanStream (the
# Zip "append to an existing archive" path) die naming themselves.

package Compress::Raw::Zlib;

use strict;
use warnings;
require Exporter;
use Carp;
use Scalar::Util ();

our ($VERSION, $XS_VERSION, @ISA, @EXPORT, %EXPORT_TAGS, @EXPORT_OK,
     %DEFLATE_CONSTANTS, @DEFLATE_CONSTANTS, $gzip_os_code);

$VERSION = '2.214';
$XS_VERSION = $VERSION;

@ISA = qw(Exporter);
%EXPORT_TAGS = (
    flush    => [qw(Z_NO_FLUSH Z_PARTIAL_FLUSH Z_SYNC_FLUSH Z_FULL_FLUSH
                    Z_FINISH Z_BLOCK)],
    level    => [qw(Z_NO_COMPRESSION Z_BEST_SPEED Z_BEST_COMPRESSION
                    Z_DEFAULT_COMPRESSION)],
    strategy => [qw(Z_FILTERED Z_HUFFMAN_ONLY Z_RLE Z_FIXED
                    Z_DEFAULT_STRATEGY)],
    status   => [qw(Z_OK Z_STREAM_END Z_NEED_DICT Z_ERRNO Z_STREAM_ERROR
                    Z_DATA_ERROR Z_MEM_ERROR Z_BUF_ERROR Z_VERSION_ERROR)],
);
%DEFLATE_CONSTANTS = %EXPORT_TAGS;

@DEFLATE_CONSTANTS =
@EXPORT = qw(
    ZLIB_VERSION ZLIB_VERNUM OS_CODE MAX_MEM_LEVEL MAX_WBITS
    Z_ASCII Z_BEST_COMPRESSION Z_BEST_SPEED Z_BINARY Z_BLOCK Z_BUF_ERROR
    Z_DATA_ERROR Z_DEFAULT_COMPRESSION Z_DEFAULT_STRATEGY Z_DEFLATED Z_ERRNO
    Z_FILTERED Z_FIXED Z_FINISH Z_FULL_FLUSH Z_HUFFMAN_ONLY Z_MEM_ERROR
    Z_NEED_DICT Z_NO_COMPRESSION Z_NO_FLUSH Z_NULL Z_OK Z_PARTIAL_FLUSH Z_RLE
    Z_STREAM_END Z_STREAM_ERROR Z_SYNC_FLUSH Z_TREES Z_UNKNOWN
    Z_VERSION_ERROR
    ZLIBNG_VERSION ZLIBNG_VERNUM ZLIBNG_VER_MAJOR ZLIBNG_VER_MINOR
    ZLIBNG_VER_REVISION ZLIBNG_VER_STATUS ZLIBNG_VER_MODIFIED
    WANT_GZIP WANT_GZIP_OR_ZLIB
);
push @EXPORT, qw(crc32 adler32 DEF_WBITS);

# The library's version, asked once at load: a box without libz dies HERE,
# in one line, as a perl without the XS object dies at `use`.
my $LIBVERSION = builtin::zlib_version();

sub zlib_version ()  { $LIBVERSION }
sub ZLIB_VERSION ()  { $LIBVERSION }
sub ZLIB_VERNUM ()   { _vernum($LIBVERSION) }
sub _vernum {
    my @v = (split(/\./, $_[0]), 0, 0, 0, 0);
    return ($v[0] << 12) | ($v[1] << 8) | ($v[2] << 4) | $v[3];
}

use constant WANT_GZIP           => 16;
use constant WANT_GZIP_OR_ZLIB   => 32;

sub OS_CODE ()              { $^O eq 'MSWin32' ? 0 : $^O eq 'darwin' ? 19 : 3 }
sub MAX_MEM_LEVEL ()        { 9 }
sub MAX_WBITS ()            { 15 }
sub Z_NO_FLUSH ()           { 0 }
sub Z_PARTIAL_FLUSH ()      { 1 }
sub Z_SYNC_FLUSH ()         { 2 }
sub Z_FULL_FLUSH ()         { 3 }
sub Z_FINISH ()             { 4 }
sub Z_BLOCK ()              { 5 }
sub Z_TREES ()              { 6 }
sub Z_OK ()                 { 0 }
sub Z_STREAM_END ()         { 1 }
sub Z_NEED_DICT ()          { 2 }
sub Z_ERRNO ()              { -1 }
sub Z_STREAM_ERROR ()       { -2 }
sub Z_DATA_ERROR ()         { -3 }
sub Z_MEM_ERROR ()          { -4 }
sub Z_BUF_ERROR ()          { -5 }
sub Z_VERSION_ERROR ()      { -6 }
sub Z_NO_COMPRESSION ()     { 0 }
sub Z_BEST_SPEED ()         { 1 }
sub Z_BEST_COMPRESSION ()   { 9 }
sub Z_DEFAULT_COMPRESSION () { -1 }
sub Z_FILTERED ()           { 1 }
sub Z_HUFFMAN_ONLY ()       { 2 }
sub Z_RLE ()                { 3 }
sub Z_FIXED ()              { 4 }
sub Z_DEFAULT_STRATEGY ()   { 0 }
sub Z_BINARY ()             { 0 }
sub Z_ASCII ()              { 1 }
sub Z_UNKNOWN ()            { 2 }
sub Z_DEFLATED ()           { 8 }
sub Z_NULL ()               { 0 }
# zlib, not zlib-ng: the zlib-ng version macros read as perl's do there.
sub ZLIBNG_VERSION ()       { "" }
sub ZLIBNG_VERNUM ()        { 0 }
sub ZLIBNG_VER_MAJOR ()     { 0 }
sub ZLIBNG_VER_MINOR ()     { 0 }
sub ZLIBNG_VER_REVISION ()  { 0 }
sub ZLIBNG_VER_STATUS ()    { 0 }
sub ZLIBNG_VER_MODIFIED ()  { 0 }
sub DEF_WBITS {
    croak "Your vendor has not defined Zlib macro DEF_WBITS, used";
}

$gzip_os_code = OS_CODE;

sub is_zlib_native ()   { 1 }
sub is_zlibng_native () { 0 }
sub is_zlibng_compat () { 0 }
sub is_zlibng ()        { 0 }
sub zlibCompileFlags () { 169 }

use constant FLAG_APPEND             => 1 ;
use constant FLAG_CRC                => 2 ;
use constant FLAG_ADLER              => 4 ;
use constant FLAG_CONSUME_INPUT      => 8 ;
use constant FLAG_LIMIT_OUTPUT       => 16 ;

# ---- the status dualvars (Zlib.xs my_z_errmsg) ---------------------------
my %STATUS_TEXT = (
    2 => "need dictionary", 1 => "stream end", 0 => "",
    -2 => "stream error", -3 => "data error", -4 => "insufficient memory",
    -5 => "buffer error", -6 => "incompatible version",
);

sub _status {
    my $code = shift;
    return Scalar::Util::dualvar($code, "$!") if $code == -1;
    exists $STATUS_TEXT{$code}
        or croak "Compress::Raw::Zlib: zlib returned the unknown status $code";
    return Scalar::Util::dualvar($code, $STATUS_TEXT{$code});
}

# ---- checksums -------------------------------------------------------------
sub _bytes {
    my ($v, $who) = @_;
    $v = $$v if ref $v;
    return "" if !defined $v;
    utf8::downgrade($v, 1) or croak "Wide character in $who";
    return $v;
}

sub crc32 {
    my $buf = _bytes($_[0], "Compress::Raw::Zlib::crc32");
    my $crc = (@_ > 1 && defined $_[1]) ? $_[1] : 0;
    return builtin::zlib_crc32($buf, $crc, $_[2] || 0);
}

sub adler32 {
    my $buf = _bytes($_[0], "Compress::Raw::Zlib::adler32");
    my $adler = (@_ > 1 && defined $_[1]) ? $_[1] : 1;
    return builtin::zlib_adler32($buf, $adler, $_[2] || 0);
}

sub crc32_combine   { builtin::zlib_crc32_combine($_[0], $_[1], $_[2]) }
sub adler32_combine { builtin::zlib_adler32_combine($_[0], $_[1], $_[2]) }

# ---- parameters (the XS module's ParseParameters, its messages) -------------
use constant Parse_any      => 0x01;
use constant Parse_unsigned => 0x02;
use constant Parse_signed   => 0x04;
use constant Parse_boolean  => 0x08;

sub ParseParameters {
    my $level = shift || 0;
    my $spec = shift;
    my $sub = (caller($level + 1))[3];
    my %got;
    my %seen;
    my @entered;
    if (@_ == 1) {
        my $h = $_[0];
        croak "$sub: Expected even number of parameters, got 1"
            if !defined $h || ref $h ne 'HASH';
        @entered = %$h;
    }
    else {
        croak "$sub: Expected even number of parameters, got " . scalar(@_)
            if @_ % 2;
        @entered = @_;
    }
    for my $k (keys %$spec) {
        $got{lc $k} = $spec->{$k}[3];
    }
    my %type = map { (lc $_ => $spec->{$_}[2]) } keys %$spec;
    my @bad;
    while (@entered) {
        my ($key, $value) = splice @entered, 0, 2;
        (my $name = $key) =~ s/^-//;
        my $canon = lc $name;
        if (!exists $type{$canon}) { push @bad, $name; next }
        $got{$canon} = _checkType($sub, $name, $value, $type{$canon});
        $seen{$canon} = 1;
    }
    croak "$sub: unknown key value(s) @bad" if @bad;
    return bless { Got => \%got, Seen => \%seen },
        'Compress::Raw::Zlib::Parameters';
}

sub _checkType {
    my ($sub, $key, $value, $type) = @_;
    return $value if $type & Parse_any;
    if ($type & Parse_unsigned) {
        croak "$sub: Parameter '$key' must be an unsigned int, got 'undef'"
            if !defined $value;
        croak "$sub: Parameter '$key' must be an unsigned int, got '$value'"
            if $value !~ /^\d+$/;
        return $value;
    }
    if ($type & Parse_signed) {
        croak "$sub: Parameter '$key' must be a signed int, got 'undef'"
            if !defined $value;
        croak "$sub: Parameter '$key' must be a signed int, got '$value'"
            if $value !~ /^-?\d+$/;
        return $value;
    }
    if ($type & Parse_boolean) {
        croak "$sub: Parameter '$key' must be an int, got '$value'"
            if defined $value && $value !~ /^\d*$/;
        return defined $value ? ($value != 0 ? 1 : 0) : 0;
    }
    return $value;
}

sub Compress::Raw::Zlib::Parameters::value  { $_[0]{Got}{lc $_[1]} }
sub Compress::Raw::Zlib::Parameters::parsed { $_[0]{Seen}{lc $_[1]} ? 1 : 0 }

# ---- constructors -------------------------------------------------------------
our $OPTIONS_deflate = {
    'AppendOutput'  => [1, 1, Parse_boolean,  0],
    'CRC32'         => [1, 1, Parse_boolean,  0],
    'ADLER32'       => [1, 1, Parse_boolean,  0],
    'Bufsize'       => [1, 1, Parse_unsigned, 4096],
    'Level'         => [1, 1, Parse_signed,   Z_DEFAULT_COMPRESSION()],
    'Method'        => [1, 1, Parse_unsigned, Z_DEFLATED()],
    'WindowBits'    => [1, 1, Parse_signed,   MAX_WBITS()],
    'MemLevel'      => [1, 1, Parse_unsigned, MAX_MEM_LEVEL()],
    'Strategy'      => [1, 1, Parse_unsigned, Z_DEFAULT_STRATEGY()],
    'Dictionary'    => [1, 1, Parse_any,      ""],
};

sub Compress::Raw::Zlib::Deflate::new {
    my $pkg = shift;
    my $got = ParseParameters(0, $OPTIONS_deflate, @_);
    croak "Compress::Raw::Zlib::Deflate::new: Bufsize must be >= 1, you specified "
        . $got->value('Bufsize')
        unless $got->value('Bufsize') >= 1;
    my $flags = 0;
    $flags |= FLAG_APPEND if $got->value('AppendOutput');
    $flags |= FLAG_CRC    if $got->value('CRC32');
    $flags |= FLAG_ADLER  if $got->value('ADLER32');
    my $windowBits = $got->value('WindowBits');
    $windowBits += MAX_WBITS() if ($windowBits & MAX_WBITS()) == 0;
    _deflateInit($flags, $got->value('Level'), $got->value('Method'),
                 $windowBits, $got->value('MemLevel'), $got->value('Strategy'),
                 $got->value('Bufsize'), $got->value('Dictionary'));
}

our $OPTIONS_inflate = {
    'AppendOutput'  => [1, 1, Parse_boolean,  0],
    'LimitOutput'   => [1, 1, Parse_boolean,  0],
    'CRC32'         => [1, 1, Parse_boolean,  0],
    'ADLER32'       => [1, 1, Parse_boolean,  0],
    'ConsumeInput'  => [1, 1, Parse_boolean,  1],
    'Bufsize'       => [1, 1, Parse_unsigned, 4096],
    'WindowBits'    => [1, 1, Parse_signed,   MAX_WBITS()],
    'Dictionary'    => [1, 1, Parse_any,      ""],
};

sub Compress::Raw::Zlib::Inflate::new {
    my $pkg = shift;
    my $got = ParseParameters(0, $OPTIONS_inflate, @_);
    croak "Compress::Raw::Zlib::Inflate::new: Bufsize must be >= 1, you specified "
        . $got->value('Bufsize')
        unless $got->value('Bufsize') >= 1;
    my $flags = 0;
    $flags |= FLAG_APPEND        if $got->value('AppendOutput');
    $flags |= FLAG_CRC           if $got->value('CRC32');
    $flags |= FLAG_ADLER         if $got->value('ADLER32');
    $flags |= FLAG_CONSUME_INPUT if $got->value('ConsumeInput');
    $flags |= FLAG_LIMIT_OUTPUT  if $got->value('LimitOutput');
    my $windowBits = $got->value('WindowBits');
    $windowBits += MAX_WBITS() if ($windowBits & MAX_WBITS()) == 0;
    _inflateInit($flags, $windowBits, $got->value('Bufsize'),
                 $got->value('Dictionary'));
}

sub Compress::Raw::Zlib::InflateScan::new {
    croak "Compress::Raw::Zlib::InflateScan is not implemented in PCL (task #3161)";
}

sub _new_state {
    my ($kind, $flags, $bufsize, %more) = @_;
    return { kind => $kind, flags => $flags, bufsize => $bufsize,
             crc32 => 0, adler32 => 1, dict_adler => 0, last_error => 0,
             compressed => 0, uncompressed => 0, inflated => 0,
             pending => "", %more };
}

sub _deflateInit {
    my ($flags, $level, $method, $wbits, $memlevel, $strategy, $bufsize,
        $dictionary) = @_;
    my ($st, $h) = builtin::zlib_deflate_init2($level, $method, $wbits,
                                                $memlevel, $strategy);
    my $obj;
    if ($st == Z_OK) {
        my $s = _new_state('deflate', $flags, $bufsize, h => $h,
                           level => $level, method => $method,
                           wbits => $wbits, memlevel => $memlevel,
                           strategy => $strategy);
        if (defined $dictionary && length $dictionary) {
            $st = builtin::zlib_deflate_set_dictionary(
                $h, _bytes($dictionary, "Compress::Raw::Zlib::Deflate::new dictionary"));
            $s->{dict_adler} = (builtin::zlib_stream($h))[2];
        }
        $obj = bless \$s, 'Compress::Raw::Zlib::deflateStream';
    }
    return wantarray ? ($obj, _status($st)) : $obj;
}

sub _inflateInit {
    my ($flags, $wbits, $bufsize, $dictionary) = @_;
    my ($st, $h) = builtin::zlib_inflate_init2($wbits);
    my $obj;
    if ($st == Z_OK) {
        my $s = _new_state('inflate', $flags, $bufsize, h => $h,
                           wbits => $wbits);
        if (defined $dictionary && length $dictionary) {
            $s->{dictionary} = $dictionary;
            # a raw stream has no header to ask for it: set it now
            $st = builtin::zlib_inflate_set_dictionary($h, $dictionary)
                if $wbits < 0;
        }
        $obj = bless \$s, 'Compress::Raw::Zlib::inflateStream';
    }
    return wantarray ? ($obj, _status($st)) : $obj;
}

sub _inflateScanInit {
    croak "Compress::Raw::Zlib::InflateScan is not implemented in PCL (task #3161)";
}

# ---- shared stream plumbing -------------------------------------------------
# The output parameter: a scalar or a reference to one (Zlib.xs deRef_l).
sub _out_ref { ref $_[0] ? $_[0] : \$_[0] }

# Free the z_stream of an ENDED stream, keeping what its accessors report.
sub _end {
    my $s = shift;
    return if !defined $s->{h};
    my @info = builtin::zlib_stream($s->{h});
    $s->{info} = \@info;
    if ($s->{kind} eq 'deflate') { builtin::zlib_deflate_end($s->{h}) }
    else                         { builtin::zlib_inflate_end($s->{h}) }
    $s->{h} = undef;
}

sub _info { my $s = shift; defined $s->{h} ? [builtin::zlib_stream($s->{h})] : ($s->{info} || [0, 0, 0, undef, 0]) }

sub _reset_counters {
    my $s = shift;
    @$s{qw(crc32 adler32 compressed uncompressed inflated pending)} = (0, 1, 0, 0, 0, "");
    delete $s->{ended};
    delete $s->{info};
}

package Compress::Raw::Zlib::deflateStream;
use Carp;

sub deflate {
    my $s = ${$_[0]};
    my $in = Compress::Raw::Zlib::_bytes($_[1], "Compress::Raw::Zlib::Deflate::deflate input parameter");
    my $out = Compress::Raw::Zlib::_out_ref($_[2]);
    $$out = '' if !($s->{flags} & Compress::Raw::Zlib::FLAG_APPEND) || !defined $$out;
    $s->{crc32} = builtin::zlib_crc32($in, $s->{crc32}, 0)
        if $s->{flags} & Compress::Raw::Zlib::FLAG_CRC;
    $s->{adler32} = builtin::zlib_adler32($in, $s->{adler32}, 0)
        if $s->{flags} & Compress::Raw::Zlib::FLAG_ADLER;
    my $buf = $s->{pending};
    $s->{pending} = "";
    my $st = 0;
    my $off = 0;
    if ($s->{ended}) {
        $st = length($in) ? -5 : 0;
    }
    else {
        my $outmax = $s->{bufsize};
        while ($off < length $in) {
            my ($r, $o, $used) = builtin::zlib_deflate(
                $s->{h}, ($off ? substr($in, $off) : $in), 0, $outmax);
            $buf .= $o;
            $off += $used;
            $st = $r;
            last if $r != 0;
            $outmax *= 2 if length($o) == $outmax;
        }
    }
    $s->{compressed} += length $buf;
    $s->{uncompressed} += $off;
    $s->{last_error} = $st;
    if ($st == 0) {
        $$out .= $buf;
    }
    return Compress::Raw::Zlib::_status($st);
}

sub flush {
    my $s = ${$_[0]};
    my $out = Compress::Raw::Zlib::_out_ref($_[1]);
    $$out = '' if !($s->{flags} & Compress::Raw::Zlib::FLAG_APPEND) || !defined $$out;
    my $f = @_ > 2 && defined $_[2] ? $_[2] : Compress::Raw::Zlib::Z_FINISH();
    my $buf = $s->{pending};
    $s->{pending} = "";
    my $st = 0;
    if (!$s->{ended}) {
        my $outmax = $s->{bufsize};
        for (;;) {
            my ($r, $o) = builtin::zlib_deflate($s->{h}, "", $f, $outmax);
            $buf .= $o;
            $st = $r;
            # Ignore the second of two consecutive flushes (Zlib.xs)
            $st = 0 if $r == -5 && length($o) == 0;
            last if length($o) < $outmax || $st != 0;
            $outmax *= 2;
        }
        if ($st == 1) {
            $st = 0;
            $s->{ended} = 1;
            Compress::Raw::Zlib::_end($s);
        }
    }
    $s->{compressed} += length $buf;
    $s->{last_error} = $st;
    if ($st == 0) {
        $$out .= $buf;
    }
    return Compress::Raw::Zlib::_status($st);
}

sub deflateReset {
    my $s = ${$_[0]};
    my $st;
    if (defined $s->{h}) {
        $st = builtin::zlib_deflate_reset($s->{h});
    }
    else {
        my $h;
        ($st, $h) = builtin::zlib_deflate_init2(@$s{qw(level method wbits memlevel strategy)});
        $s->{h} = $h if $st == 0;
    }
    Compress::Raw::Zlib::_reset_counters($s) if $st == 0;
    return Compress::Raw::Zlib::_status($st);
}

sub deflateParams {
    my $self = shift;
    my $got = Compress::Raw::Zlib::ParseParameters(0, {
        'Level'    => [1, 1, Compress::Raw::Zlib::Parse_signed,   undef],
        'Strategy' => [1, 1, Compress::Raw::Zlib::Parse_unsigned, undef],
        'Bufsize'  => [1, 1, Compress::Raw::Zlib::Parse_unsigned, undef],
    }, @_);
    croak "Compress::Raw::Zlib::deflateParams needs Level and/or Strategy"
        unless $got->parsed('Level') + $got->parsed('Strategy')
             + $got->parsed('Bufsize');
    croak "Compress::Raw::Zlib::Inflate::deflateParams: Bufsize must be >= 1, you specified "
        . $got->value('Bufsize')
        if $got->parsed('Bufsize') && $got->value('Bufsize') <= 1;
    my $flags = 0;
    $flags |= 1 if $got->parsed('Level');
    $flags |= 2 if $got->parsed('Strategy');
    $flags |= 4 if $got->parsed('Bufsize');
    $self->_deflateParams($flags, $got->value('Level'), $got->value('Strategy'),
                          $got->value('Bufsize'));
}

# Zlib.xs flushParams: the block zlib emits when the parameters change is
# kept and handed out in front of the next deflate / flush output.
sub _deflateParams {
    my ($self, $flags, $level, $strategy, $bufsize) = @_;
    my $s = $$self;
    $s->{level} = $level       if $flags & 1;
    $s->{strategy} = $strategy if $flags & 2;
    $s->{bufsize} = $bufsize   if $flags & 4;
    my $st = 0;
    if (!$s->{ended}) {
        for (;;) {
            my ($r, $o) = builtin::zlib_deflate_params($s->{h}, $s->{level},
                                                       $s->{strategy}, $s->{bufsize});
            $s->{pending} .= $o;
            $st = $r;
            last if $r != -5 || !length $o;
        }
        $st = 0 if $st == -5;
    }
    $s->{last_error} = $st;
    return Compress::Raw::Zlib::_status($st);
}

sub deflateTune {
    my $s = ${$_[0]};
    return Compress::Raw::Zlib::_status(
        builtin::zlib_deflate_tune($s->{h}, @_[1 .. 4]));
}

sub get_Level    { ${$_[0]}->{level} }
sub get_Strategy { ${$_[0]}->{strategy} }
sub get_Bufsize  { ${$_[0]}->{bufsize} }
sub status       { Compress::Raw::Zlib::_status(${$_[0]}->{last_error}) }
sub crc32        { ${$_[0]}->{crc32} }
sub adler32      { ${$_[0]}->{adler32} }
sub dict_adler   { ${$_[0]}->{dict_adler} }
sub msg          { Compress::Raw::Zlib::_info(${$_[0]})->[3] }
sub total_in     { Compress::Raw::Zlib::_info(${$_[0]})->[0] }
sub total_out    { Compress::Raw::Zlib::_info(${$_[0]})->[1] }
sub uncompressedBytes { ${$_[0]}->{uncompressed} }
sub compressedBytes   { ${$_[0]}->{compressed} }

sub STORABLE_freeze { my $type = ref shift; croak "Cannot freeze $type object\n" }
sub STORABLE_thaw   { my $type = ref shift; croak "Cannot thaw $type object\n" }

sub DESTROY { my $s = ${$_[0]}; Compress::Raw::Zlib::_end($s) if ref $s }

package Compress::Raw::Zlib::inflateStream;
use Carp;

sub inflate {
    my $s = ${$_[0]};
    my $in = Compress::Raw::Zlib::_bytes($_[1], "Compress::Raw::Zlib::Inflate::inflate input parameter");
    my $out = Compress::Raw::Zlib::_out_ref($_[2]);
    $$out = '' if !($s->{flags} & Compress::Raw::Zlib::FLAG_APPEND) || !defined $$out;
    my $limit = $s->{flags} & Compress::Raw::Zlib::FLAG_LIMIT_OUTPUT;
    my $buf = "";
    my $off = 0;
    my $st = 0;
    if ($s->{ended}) {
        $st = 1;
    }
    else {
        my $outmax = $s->{bufsize};
        while ($st == 0) {
            my ($r, $o, $used) = builtin::zlib_inflate(
                $s->{h}, ($off ? substr($in, $off) : $in), 2, $outmax);
            $buf .= $o;
            $off += $used;
            $st = $r;
            my $full = length($o) == $outmax;
            if ($r == 2 && defined $s->{dictionary}) {
                $s->{dict_adler} = (builtin::zlib_stream($s->{h}))[2];
                $st = builtin::zlib_inflate_set_dictionary($s->{h}, $s->{dictionary});
                next if $st == 0;
            }
            if ($limit && ($st == 0 || $st == -5)) {
                $st = $full ? -5 : 0;
                last;
            }
            last if $st == -2 || $st == -4 || $st == -3 || $st == 1;
            if ($st == -5) {
                if ($full) { $st = 0; $outmax *= 2; next }
                if ($off >= length $in) { $st = 0; last }
            }
            $outmax *= 2 if $full;
        }
    }
    $s->{last_error} = $st;
    if ($st == 0 || $st == 1 || $st == -5 || $st == -3) {
        $s->{inflated} = length $buf;
        $s->{uncompressed} += length $buf;
        $s->{compressed} += $off;
        $$out .= $buf;
        $s->{crc32} = builtin::zlib_crc32($buf, $s->{crc32}, 0)
            if $s->{flags} & Compress::Raw::Zlib::FLAG_CRC;
        $s->{adler32} = builtin::zlib_adler32($buf, $s->{adler32}, 0)
            if $s->{flags} & Compress::Raw::Zlib::FLAG_ADLER;
        if ($s->{flags} & (Compress::Raw::Zlib::FLAG_CONSUME_INPUT
                           | Compress::Raw::Zlib::FLAG_LIMIT_OUTPUT)) {
            my $rest = $off < length($in) ? substr($in, $off) : "";
            if (ref $_[1]) { ${$_[1]} = $rest } else { $_[1] = $rest }
        }
    }
    if ($st == 1 && !$s->{ended}) {
        $s->{ended} = 1;
        Compress::Raw::Zlib::_end($s);
    }
    return Compress::Raw::Zlib::_status($st);
}

sub inflateSync {
    my $s = ${$_[0]};
    my $in = Compress::Raw::Zlib::_bytes($_[1], "Compress::Raw::Zlib::Inflate::inflateSync");
    return Compress::Raw::Zlib::_status(-2) if !defined $s->{h};
    my ($st, $used) = builtin::zlib_inflate_sync($s->{h}, $in);
    $s->{last_error} = $st;
    if ($st == 0 || $st == -3) {
        my $rest = $used < length($in) ? substr($in, $used) : "";
        if (ref $_[1]) { ${$_[1]} = $rest } else { $_[1] = $rest }
    }
    return Compress::Raw::Zlib::_status($st);
}

sub inflateReset {
    my $s = ${$_[0]};
    my $st;
    if (defined $s->{h}) {
        $st = builtin::zlib_inflate_reset($s->{h});
    }
    else {
        my $h;
        ($st, $h) = builtin::zlib_inflate_init2($s->{wbits});
        if ($st == 0) {
            $s->{h} = $h;
            builtin::zlib_inflate_set_dictionary($h, $s->{dictionary})
                if defined $s->{dictionary} && $s->{wbits} < 0;
        }
    }
    Compress::Raw::Zlib::_reset_counters($s) if $st == 0;
    return Compress::Raw::Zlib::_status($st);
}

sub set_Append {
    my $s = ${$_[0]};
    my $was = ($s->{flags} & Compress::Raw::Zlib::FLAG_APPEND) ? 1 : 0;
    if ($_[1]) { $s->{flags} |= Compress::Raw::Zlib::FLAG_APPEND }
    else       { $s->{flags} &= ~Compress::Raw::Zlib::FLAG_APPEND }
    return $was;
}

sub get_Bufsize  { ${$_[0]}->{bufsize} }
sub status       { Compress::Raw::Zlib::_status(${$_[0]}->{last_error}) }
sub crc32        { ${$_[0]}->{crc32} }
sub adler32      { ${$_[0]}->{adler32} }
sub dict_adler   { ${$_[0]}->{dict_adler} }
sub inflateCount { ${$_[0]}->{inflated} }
sub msg          { Compress::Raw::Zlib::_info(${$_[0]})->[3] }
sub total_in     { Compress::Raw::Zlib::_info(${$_[0]})->[0] }
sub total_out    { Compress::Raw::Zlib::_info(${$_[0]})->[1] }
sub uncompressedBytes { ${$_[0]}->{uncompressed} }
sub compressedBytes   { ${$_[0]}->{compressed} }

sub STORABLE_freeze { my $type = ref shift; croak "Cannot freeze $type object\n" }
sub STORABLE_thaw   { my $type = ref shift; croak "Cannot thaw $type object\n" }

sub DESTROY { my $s = ${$_[0]}; Compress::Raw::Zlib::_end($s) if ref $s }

1;
