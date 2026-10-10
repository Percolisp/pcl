# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pcl-shim: must-win -- the real module is XS.  This shim is found BEFORE @INC is
# searched, so a PERL5LIB or -I copy of the real module cannot shadow it
# (task #2462, docs/ir-spec.md 9, docs/shipped-modules.md).
#
# Encode without XS (task #2946, s513i).  `use Encode` died "the module is XS
# and has no PCL build" -- and it is in most programs that read or write
# non-ASCII text.  The CODEC is the runtime's (the ONE table `:encoding(NAME)`
# layers use, reached through three builtin:: primitives that stop at the
# first failure and say where it is); everything perl's Encode decides --
# names and aliases, the CHECK modes, the in-place remainder, strict vs lax
# UTF-8, the UTF-16/32 byte-order marks, the Encoding objects -- is plain Perl
# here.  Perl's own pure-Perl Encode::Encoding and Encode::MIME::Name are
# reused unchanged.
#
# THE FORM RULE (docs/ir-spec.md, "Encode's codec primitives"): decode returns
# characters, encode returns characters 0-255 (octets).  PCL has no per-scalar
# UTF-8 flag (#1389), so is_utf8 is utf8::is_utf8 and says 1 for every string
# -- after encode too, where perl says 0.

package Encode;
use strict;
use warnings;

our $VERSION = '3.21';

use Exporter 'import';

our @EXPORT = qw(decode decode_utf8 encode encode_utf8 str2bytes bytes2str
                 encodings find_encoding find_mime_encoding clone_encoding);
our @FB_FLAGS = qw(DIE_ON_ERR WARN_ON_ERR RETURN_ON_ERR LEAVE_SRC PERLQQ
                   HTMLCREF XMLCREF STOP_AT_PARTIAL);
our @FB_CONSTS = qw(FB_DEFAULT FB_CROAK FB_QUIET FB_WARN FB_PERLQQ
                    FB_HTMLCREF FB_XMLCREF);
our @EXPORT_OK = (qw(_utf8_off _utf8_on define_encoding from_to is_16bit
                     is_8bit is_utf8 perlio_ok resolve_alias utf8_downgrade
                     utf8_upgrade), @FB_FLAGS, @FB_CONSTS);
our %EXPORT_TAGS = (
    all          => [ @EXPORT, @EXPORT_OK ],
    default      => [ @EXPORT ],
    fallbacks    => [ @FB_CONSTS ],
    fallback_all => [ @FB_CONSTS, @FB_FLAGS ],
);

our @CARP_NOT = qw(Encode::Encoding Encode::XS Encode::utf8 Encode::Unicode);

# perl's values (probed, Encode 3.21).  The FB_ composites that escape
# INCLUDE LEAVE_SRC.
sub DIE_ON_ERR ()           { 0x0001 }
sub WARN_ON_ERR ()          { 0x0002 }
sub RETURN_ON_ERR ()        { 0x0004 }
sub LEAVE_SRC ()            { 0x0008 }
sub ONLY_PRAGMA_WARNINGS () { 0x0010 }
sub PERLQQ ()               { 0x0100 }
sub HTMLCREF ()             { 0x0200 }
sub XMLCREF ()              { 0x0400 }
sub STOP_AT_PARTIAL ()      { 0x0800 }
sub FB_DEFAULT ()           { 0x0000 }
sub FB_CROAK ()             { 0x0001 }
sub FB_QUIET ()             { 0x0004 }
sub FB_WARN ()              { 0x0006 }
sub FB_PERLQQ ()            { 0x0108 }
sub FB_HTMLCREF ()          { 0x0208 }
sub FB_XMLCREF ()           { 0x0408 }

# ---------------------------------------------------------------------------
# Names.  %INFO: perl's canonical name -> [runtime codec, kind, class].
# kind: utf8 (lax), strict (utf-8-strict), byte (one byte per char),
# mb (a multi-byte legacy codec), u16 / u32 (fixed units, no BOM),
# bom16 / bom32 (the endianness-less UTF-16 / UTF-32: a BOM on both sides).
# ---------------------------------------------------------------------------
our %INFO = (
    'utf8'         => [ 'utf8',  'utf8',   'Encode::utf8' ],
    'utf-8-strict' => [ 'utf8',  'strict', 'Encode::utf8' ],
    'ascii'        => [ 'ascii', 'byte',   'Encode::XS' ],
    'iso-8859-1'   => [ 'latin1', 'byte',  'Encode::XS' ],
    'UTF-16'       => [ 'utf-16be', 'bom16', 'Encode::Unicode' ],
    'UTF-16BE'     => [ 'utf-16be', 'u16',   'Encode::Unicode' ],
    'UTF-16LE'     => [ 'utf-16le', 'u16',   'Encode::Unicode' ],
    'UTF-32'       => [ 'utf-32be', 'bom32', 'Encode::Unicode' ],
    'UTF-32BE'     => [ 'utf-32be', 'u32',   'Encode::Unicode' ],
    'UTF-32LE'     => [ 'utf-32le', 'u32',   'Encode::Unicode' ],
    'UCS-2BE'      => [ 'ucs-2be',  'u16',   'Encode::Unicode' ],
    'UCS-2LE'      => [ 'ucs-2le',  'u16',   'Encode::Unicode' ],
    'koi8-r'       => [ 'koi8-r', 'byte', 'Encode::XS' ],
    'koi8-u'       => [ 'koi8-u', 'byte', 'Encode::XS' ],
    'MacRoman'     => [ 'mac-roman', 'byte', 'Encode::XS' ],
    'MacCyrillic'  => [ 'x-mac-cyrillic', 'byte', 'Encode::XS' ],
    'shiftjis'     => [ 'shift_jis', 'mb', 'Encode::XS' ],
    'euc-jp'       => [ 'euc-jp', 'mb', 'Encode::XS' ],
    'cp936'        => [ 'gbk', 'mb', 'Encode::XS' ],
);
for my $n (2 .. 11, 13 .. 16) { $INFO{"iso-8859-$n"} = [ "iso-8859-$n", 'byte', 'Encode::XS' ] }
for my $n (437, 850, 852, 855, 857, 860 .. 866, 869, 874, 1250 .. 1258) {
    $INFO{"cp$n"} = [ "cp$n", 'byte', 'Encode::XS' ];
}

# lower-cased alias -> canonical name (perl's Encode::Alias answers, probed).
our %ALIAS = (
    'utf8' => 'utf8', 'utf-8' => 'utf-8-strict', 'utf-8-strict' => 'utf-8-strict',
    'ascii' => 'ascii', 'us-ascii' => 'ascii', 'ansi_x3.4-1968' => 'ascii',
    'iso-646-us' => 'ascii',
    'latin1' => 'iso-8859-1', 'latin-1' => 'iso-8859-1',
    'utf16' => 'UTF-16', 'utf-16' => 'UTF-16', 'utf32' => 'UTF-32', 'utf-32' => 'UTF-32',
    'ucs2' => 'UCS-2BE', 'ucs-2' => 'UCS-2BE', 'iso-10646-1' => 'UCS-2BE',
    'macroman' => 'MacRoman', 'maccyrillic' => 'MacCyrillic',
    'shiftjis' => 'shiftjis', 'shift_jis' => 'shiftjis', 'sjis' => 'shiftjis',
    'shift-jis' => 'shiftjis',
    'euc-jp' => 'euc-jp', 'eucjp' => 'euc-jp', 'ujis' => 'euc-jp',
    'gbk' => 'cp936', 'cp936' => 'cp936',
    'winlatin1' => 'cp1252', 'winlatin2' => 'cp1250', 'wincyrillic' => 'cp1251',
    'wingreek' => 'cp1253', 'winturkish' => 'cp1254', 'winhebrew' => 'cp1255',
    'winarabic' => 'cp1256', 'winbaltic' => 'cp1257', 'winvietnamese' => 'cp1258',
);
our %NOCODEC = (
    "big5" => "big5-eten", "big5-eten" => "big5-eten", "big5-hkscs" => "big5-hkscs",
    "euc-kr" => "euc-kr", "cp949" => "cp949", "cp950" => "cp950",
    "euc-cn" => "euc-cn", "gb2312" => "euc-cn", "hz" => "hz", "johab" => "johab",
    "iso-2022-jp" => "iso-2022-jp", "iso-2022-kr" => "iso-2022-kr",
    "cp932" => "cp932", "7bit-jis" => "7bit-jis",
);
my %LATIN = (1 => 1, 2 => 2, 3 => 3, 4 => 4, 5 => 9, 6 => 10, 7 => 13,
             8 => 14, 9 => 15, 10 => 16);

our %Encoding;      # canonical name -> object, for every encoding asked for

sub resolve_alias {
    my $name = shift;
    $name = shift if defined $name && $name eq 'Encode' && @_;
    return undef if !defined $name || ref $name;
    my $canon = _canonical($name);
    return $canon if defined $canon;
    if (defined &Encode::Alias::find_alias) {
        my $e = Encode::Alias->find_alias($name);
        return $e->name if ref $e;
    }
    # perl knows these names; this host has no codec for them, so
    # find_encoding answers undef (docs/not-supported.md, Encode).
    return $NOCODEC{ lc $name };
}

sub _canonical {
    my $name = shift;
    return $name if exists $Encoding{$name};
    my $lc = lc $name;
    $lc =~ s/^\s+|\s+$//g;
    for my $k (keys %Encoding) { return $k if lc $k eq $lc }
    my $c = $ALIAS{$lc};
    if (!defined $c) {
        for my $k (keys %INFO) { if (lc $k eq $lc) { $c = $k; last } }
    }
    if (!defined $c) {
        if ($lc =~ /^(?:iso[-_ ]?)?8859[-_ ]?(\d+)$/) { $c = "iso-8859-$1" }
        elsif ($lc =~ /^latin[-_ ]?(\d+)$/ && $LATIN{$1}) { $c = "iso-8859-$LATIN{$1}" }
        elsif ($lc =~ /^(?:x[-_])?(?:windows|win|cp|ms|ibm)[-_ ]?(\d{3,4})$/) { $c = "cp$1" }
        elsif ($lc =~ /^utf[-_]?(16|32)[-_]?([bl]e)$/) { $c = "UTF-$1" . uc $2 }
        elsif ($lc =~ /^ucs[-_]?2[-_]?([bl]e)$/) { $c = 'UCS-2' . uc $1 }
        elsif ($lc =~ /^koi8[-_]?([ru])$/) { $c = "koi8-$1" }
    }
    return undef if !defined $c || !exists $INFO{$c};
    return undef if !builtin::encoding_known($INFO{$c}[0]);
    return $c;
}

sub find_encoding {
    my ($name, $skip_external) = @_;
    $name = $skip_external, $skip_external = $_[2]
        if defined $name && $name eq 'Encode' && @_ > 1;
    return undef if !defined $name;
    return $name if ref $name && eval { $name->isa('Encode::Encoding') };
    my $canon = _canonical("$name");
    if (!defined $canon) {
        # A program that loaded perl's own Encode::Alias may have defined
        # aliases there (a string, a regex or a CODE ref -- ExtUtils::MakeMaker::
        # Locale's "locale"); real Encode asks it last, and so does this.
        if (defined &Encode::Alias::find_alias) {
            my $e = Encode::Alias->find_alias("$name");
            return $e if ref $e;
        }
        return undef;
    }
    return $Encoding{$canon} if $Encoding{$canon};
    my ($codec, $kind, $class) = @{ $INFO{$canon} };
    my $obj = bless { Name => $canon, codec => $codec, kind => $kind }, $class;
    $obj->{strict_utf8} = 1 if $kind eq 'strict';
    $Encoding{$canon} = $obj;
    return $obj;
}

sub find_mime_encoding {
    my $mime = shift;
    $mime = shift if defined $mime && $mime eq 'Encode' && @_;
    return undef if !defined $mime;
    require Encode::MIME::Name;
    for my $c (sort keys %INFO) {
        my $m = Encode::MIME::Name::get_mime_name($c);
        return find_encoding($c) if defined $m && lc $m eq lc $mime;
    }
    return undef;
}

sub clone_encoding {
    my $obj = find_encoding(@_);
    return undef if !$obj;
    return bless { %$obj }, ref $obj;
}

sub define_encoding {
    my $obj  = shift;
    my $name = shift;
    $Encoding{$name} = $obj;
    my $lc = lc $name;
    $ALIAS{$lc} = $name if !exists $INFO{$name};
    $INFO{$name} ||= [ undef, 'object', ref $obj ];
    for my $alias (@_) { $ALIAS{lc $alias} = $name }
    return $obj;
}

sub encodings {
    # Every codec this host has counts as LOADED (perl loads its sets on
    # demand, so its plain list grows as a program asks; here it is whole from
    # the start), so `:all` and the plain list agree.
    my %enc = map { $_ => 1 } keys %Encoding;
    for my $c (keys %INFO) {
        $enc{$c} = 1 if defined $INFO{$c}[0] && builtin::encoding_known($INFO{$c}[0]);
    }
    return sort { lc $a cmp lc $b } keys %enc;
}

sub perlio_ok {
    my $obj = find_encoding($_[0]);
    return $obj ? $obj->perlio_ok : 0;
}

# perl's Carp shape: the message at the first caller outside Encode.
# (PCL's Carp shim does not add a location, #233.)
sub _where {
    for (my $i = 1; ; $i++) {
        my ($pkg, $file, $line) = caller($i);
        return "\n" if !defined $pkg;
        next if $pkg =~ /^Encode(?:::|\z)/;
        # A frame PCL cannot place (caller fidelity, #233) names the
        # runtime or a transpiled .lisp file: say no location rather than a
        # wrong one.
        return "\n" if $file =~ /\.lisp\z/;
        return " at $file line $line.\n";
    }
}
sub _croak { die join("", @_) . _where() }
sub _carp  { warn join("", @_) . _where() }

sub _get {
    my $name = shift;
    _croak("Encoding name should not be undef") if !defined $name;
    my $enc = find_encoding($name);
    _croak("Unknown encoding '$name'") if !defined $enc;
    return $enc;
}

sub encode($$;$) {
    my ($name, $string, $check) = @_;
    return undef if !defined $string;
    $string .= '';
    $check ||= 0;
    my $enc = _get($name);
    my $octets = $enc->encode($string, $check);
    $_[1] = $string if $check && !ref $check && !($check & LEAVE_SRC);
    return $octets;
}

sub decode($$;$) {
    my ($name, $octets, $check) = @_;
    return undef if !defined $octets;
    $octets .= '';
    $check ||= 0;
    my $enc = _get($name);
    my $string = $enc->decode($octets, $check);
    $_[1] = $octets if $check && !ref $check && !($check & LEAVE_SRC);
    return $string;
}

*str2bytes = \&encode;
*bytes2str = \&decode;

sub from_to($$$;$) {
    my ($string, $from, $to, $check) = @_;
    return undef if !defined $string;
    $check ||= 0;
    my $f = find_encoding($from);
    _croak("Unknown encoding '$from'") if !defined $f;
    my $t = find_encoding($to);
    _croak("Unknown encoding '$to'") if !defined $t;
    my $uni = $f->decode($string);
    $_[0] = $string = $t->encode($uni, $check);
    return undef if $check && length $uni;
    return defined $_[0] ? length $string : undef;
}

sub encode_utf8($) {
    my $str = shift;
    return undef if !defined $str;
    utf8::encode($str);
    return $str;
}

my $utf8enc;
sub decode_utf8($;$) {
    my ($octets, $check) = @_;
    return undef if !defined $octets;
    $octets .= '';
    $check ||= 0;
    $utf8enc ||= find_encoding('utf8');
    my $string = $utf8enc->decode($octets, $check);
    $_[0] = $octets if $check && !ref $check && !($check & LEAVE_SRC);
    return $string;
}

sub is_utf8 { return utf8::is_utf8($_[0]) }

# The flag flips.  PCL has no flag (#1389): _utf8_on READS the octets as
# (lax) UTF-8 when they are valid -- what perl's reinterpretation shows --
# and leaves them otherwise; _utf8_off turns characters into their UTF-8
# octets.  The previous-flag answer is perl's for the ordinary use (octets
# in, characters out and back).
sub _utf8_on {
    return undef if !defined $_[0] || ref $_[0];
    return 1 if $_[0] =~ /[^\x00-\xFF]/;
    my ($chars, $fail) = builtin::decode_octets("utf8", $_[0], 0);
    $_[0] = $chars if !defined $fail;
    return "";
}

sub _utf8_off {
    return undef if !defined $_[0] || ref $_[0];
    my $was = $_[0] =~ /[^\x00-\x7F]/ ? 1 : '';
    utf8::encode($_[0]);
    return $was;
}

# ---------------------------------------------------------------------------
# The CHECK loops.  The primitive converts up to the first failure; each
# failure is answered here by the CHECK mode, and the loop continues at the
# byte / character after it.
# ---------------------------------------------------------------------------

my $NONCHAR = do {
    my $cls = '\x{FDD0}-\x{FDEF}';
    for my $p (0 .. 16) {
        $cls .= sprintf '\x{%X}\x{%X}', $p * 0x10000 + 0xFFFE, $p * 0x10000 + 0xFFFF;
    }
    qr/[$cls]/;
};
my $NONCHAR_OR_SURROGATE = qr/$NONCHAR|[\x{D800}-\x{DFFF}]/;

# The bytes of one malformed sequence starting at $at: perl consumes the
# start byte and the continuation bytes its length announces.  Returns
# (length, partial, code point perl's LAX utf8 accepts or undef).
sub _utf8_bad {
    my ($kind, $oct, $at) = @_;
    my $b = ord substr($oct, $at, 1);
    my $want = $b < 0xC0 ? 1 : $b < 0xE0 ? 2 : $b < 0xF0 ? 3 : $b < 0xF8 ? 4
             : $b < 0xFC ? 5 : $b < 0xFE ? 6 : $b == 0xFE ? 7 : 13;
    my $n = 1;
    my $len = length $oct;
    $n++ while $n < $want && $at + $n < $len
        && (ord(substr($oct, $at + $n, 1)) & 0xC0) == 0x80;
    my $partial = ($n < $want && $at + $n == $len) ? 1 : 0;
    my $cp;
    if ($kind eq 'utf8' && $n == $want && $want >= 2 && $want <= 4) {
        my $v = $b & (0x7F >> $want);
        $v = ($v << 6) | (ord(substr($oct, $at + $_, 1)) & 0x3F) for 1 .. $want - 1;
        my $min = (0, 0, 0x80, 0x800, 0x10000)[$want];
        $cp = $v if $v >= $min && $v <= 0x10FFFF;
    }
    return ($n, $partial, $cp);
}

sub _bad_len {
    my ($enc, $oct, $at) = @_;
    my $kind = $enc->{kind};
    return _utf8_bad($kind, $oct, $at) if $kind eq 'utf8' || $kind eq 'strict';
    my $unit = $kind eq 'u16' ? 2 : $kind eq 'u32' ? 4 : 1;
    my $left = length($oct) - $at;
    return ($left, 1, undef) if $left < $unit;
    return ($unit, 0, undef);
}

sub _err_name {
    my $enc = shift;
    return $enc->{Name} eq 'utf-8-strict' ? 'UTF-8' : $enc->{Name};
}

# Decode $oct with $chk; returns (string, remainder).
sub _run_decode {
    my ($enc, $oct, $chk) = @_;
    _croak("Wide character") if $oct =~ /[^\x00-\xFF]/;
    my $codec = $enc->{codec};
    my $code = ref $chk eq 'CODE' ? $chk : undef;
    my $flags = $code ? PERLQQ | LEAVE_SRC : $chk;
    my ($out, $pos, $len) = ('', 0, length $oct);
    while ($pos < $len) {
        my ($chunk, $fail) = builtin::decode_octets($codec, $oct, $pos);
        if ($enc->{kind} eq 'strict' && $chunk =~ $NONCHAR) {
            my $k = $-[0];
            my $prefix = substr($chunk, 0, $k);
            $fail = $pos + length(encode_utf8($prefix));
            $chunk = $prefix;
        }
        $out .= $chunk;
        last if !defined $fail;
        my ($n, $partial, $cp) = _bad_len($enc, $oct, $fail);
        if (defined $cp) {
            $out .= chr $cp;
            $pos = $fail + $n;
            next;
        }
        # A trailing part-unit of UTF-16/32 is left in the source silently.
        return ($out, substr($oct, $fail)) if $partial && $enc->{kind} =~ /^u(?:16|32)\z/;
        if ($partial && ($flags & STOP_AT_PARTIAL)) {
            return ($out, substr($oct, $fail));
        }
        my $bad = substr($oct, $fail, $n);
        my $msg = sprintf '%s "%s" does not map to Unicode', _err_name($enc),
            join('', map { sprintf '\x%02X', ord } split //, $bad);
        if ($code) {
            $out .= join '', map { $code->(ord $_) } split //, $bad;
        }
        else {
            _croak($msg) if $flags & DIE_ON_ERR;
            _carp($msg) if $flags & WARN_ON_ERR;
            return ($out, substr($oct, $fail)) if $flags & RETURN_ON_ERR;
            if ($flags & PERLQQ) {
                $out .= join '', map { sprintf '\x%02X', ord } split //, $bad;
            }
            elsif ($flags & HTMLCREF) {
                $out .= join '', map { sprintf '&#%u;', ord } split //, $bad;
            }
            elsif ($flags & XMLCREF) {
                $out .= join '', map { sprintf '&#x%X;', ord } split //, $bad;
            }
            else {
                $out .= "\x{FFFD}";
            }
        }
        $pos = $fail + $n;
    }
    return ($out, '');
}

# The octets perl's LAX utf8 writes for a code point SBCL will not encode
# (a surrogate).
sub _lax_utf8_bytes {
    my $c = shift;
    return chr(0xE0 | ($c >> 12)) . chr(0x80 | (($c >> 6) & 0x3F)) . chr(0x80 | ($c & 0x3F))
        if $c < 0x10000;
    return chr(0xF0 | ($c >> 18)) . chr(0x80 | (($c >> 12) & 0x3F))
         . chr(0x80 | (($c >> 6) & 0x3F)) . chr(0x80 | ($c & 0x3F));
}

# Encode $str with $chk; returns (octets, remainder).
sub _run_encode {
    my ($enc, $str, $chk) = @_;
    my $codec = $enc->{codec};
    my $kind = $enc->{kind};
    my $code = ref $chk eq 'CODE' ? $chk : undef;
    my $flags = $code ? PERLQQ | LEAVE_SRC : $chk;
    my ($out, $pos, $len) = ('', 0, length $str);
    while ($pos < $len) {
        my ($chunk, $fail) = builtin::encode_octets($codec, $str, $pos);
        if ($kind eq 'strict') {
            pos($str) = $pos;
            if ($str =~ /\G.*?($NONCHAR_OR_SURROGATE)/gs && (!defined $fail || $-[1] < $fail)) {
                $fail = $-[1];
                ($chunk) = builtin::encode_octets($codec, substr($str, $pos, $fail - $pos), 0);
            }
        }
        $out .= $chunk;
        last if !defined $fail;
        my $o = ord substr($str, $fail, 1);
        if ($kind eq 'utf8') {
            $out .= _lax_utf8_bytes($o);
            $pos = $fail + 1;
            next;
        }
        my $msg = sprintf '"\x{%04x}" does not map to %s', $o, _err_name($enc);
        if ($code) {
            $out .= $code->($o);
        }
        else {
            _croak($msg) if $flags & DIE_ON_ERR;
            _carp($msg) if $flags & WARN_ON_ERR;
            return ($out, substr($str, $fail)) if $flags & RETURN_ON_ERR;
            if ($flags & PERLQQ)      { $out .= sprintf '\x{%04x}', $o }
            elsif ($flags & HTMLCREF) { $out .= sprintf '&#%d;', $o }
            elsif ($flags & XMLCREF)  { $out .= sprintf '&#x%x;', $o }
            elsif ($kind eq 'strict' || $kind =~ /^(?:u|bom)/) {
                ($chunk) = builtin::encode_octets($codec, "\x{FFFD}", 0);
                $out .= $chunk;
            }
            else { $out .= '?' }
        }
        $pos = $fail + 1;
    }
    return ($out, '');
}

package Encode::XS;
our @ISA = ('Encode::Encoding');
our @CARP_NOT = ('Encode');

sub decode {
    my ($self, $octets, $chk) = @_;
    return undef if !defined $octets;
    $chk ||= 0;
    my ($str, $rest) = Encode::_run_decode($self, "$octets", $chk);
    $_[1] = $rest if $chk && !ref $chk && !($chk & Encode::LEAVE_SRC());
    return $str;
}

sub encode {
    my ($self, $string, $chk) = @_;
    return undef if !defined $string;
    $chk ||= 0;
    my ($oct, $rest) = Encode::_run_encode($self, "$string", $chk);
    $_[1] = $rest if $chk && !ref $chk && !($chk & Encode::LEAVE_SRC());
    return $oct;
}

sub perlio_ok { return 1 }
sub needs_lines { return 0 }

package Encode::utf8;
our @ISA = ('Encode::XS');
our @CARP_NOT = ('Encode');

package Encode::Unicode;
our @ISA = ('Encode::XS');
our @CARP_NOT = ('Encode');

my %BOM = (bom16 => [ 2, "\xFE\xFF", "\xFF\xFE", 'utf-16be', 'utf-16le' ],
           bom32 => [ 4, "\x00\x00\xFE\xFF", "\xFF\xFE\x00\x00", 'utf-32be', 'utf-32le' ]);

# The endianness-less names: encode writes a big-endian BOM and big-endian
# units; decode reads the BOM and assumes big-endian without one (perl 3.21).
sub decode {
    my ($self, $octets, $chk) = @_;
    my $b = $BOM{ $self->{kind} };
    return Encode::XS::decode(@_) if !$b || !defined $octets;
    my ($w, $be, $le, $cbe, $cle) = @$b;
    my $head = substr($octets, 0, $w);
    my $codec = $cbe;
    if ($head eq $be) { $octets = substr($octets, $w) }
    elsif ($head eq $le) { $octets = substr($octets, $w); $codec = $cle }
    my $unit = bless { %$self, codec => $codec, kind => ($w == 2 ? 'u16' : 'u32') }, 'Encode::XS';
    my $r = $unit->decode($octets, $chk);
    $_[1] = $octets if $chk && !ref $chk && !($chk & Encode::LEAVE_SRC());
    return $r;
}

sub encode {
    my ($self, $string, $chk) = @_;
    my $b = $BOM{ $self->{kind} };
    return Encode::XS::encode(@_) if !$b || !defined $string;
    my $unit = bless { %$self, kind => ($b->[0] == 2 ? 'u16' : 'u32') }, 'Encode::XS';
    my $r = $b->[1] . $unit->encode($string, $chk);
    $_[1] = $string if $chk && !ref $chk && !($chk & Encode::LEAVE_SRC());
    return $r;
}

package Encode;
require Encode::Encoding;

1;
