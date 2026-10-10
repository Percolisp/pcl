# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pcl-shim: must-win -- the real module is XS.  This shim is found BEFORE @INC is
# searched, so a PERL5LIB or -I copy of the real module cannot shadow it
# (task #2462, docs/ir-spec.md 9, docs/shipped-modules.md).
#
# Storable without XS (task #2948, batch s514a), in PERL'S OWN BINARY FORMAT:
# what `freeze` / `nfreeze` / `store` / `nstore` write here, real perl's
# Storable thaws and retrieves, and the reverse -- a frozen string or a
# Storable file is a thing programs keep (caches, IPC, fixtures), so a
# private format would be a silent incompatibility.
#
# The format (Storable.xs's "FORMAT" notes, verified against hexdumps of
# perl 5.40.3 / Storable 3.32): a header (major 2, write-minor 11, the native
# byte order and sizes, or the network flag), then one item per stored SV,
# each SV taking the next TAG, and `SX_OBJECT` + a tag for a referent stored
# before -- which is how sharing and cycles survive.  The details, and what
# PCL cannot reproduce byte for byte, are in docs/shipped-modules.md and
# docs/not-supported.md ("Storable").
#
# Dies (CLAUDE.md rule 12), with perl's texts where perl has one: CODE, GLOB,
# IO, FORMAT, LVALUE and REGEXP items, tied containers, STORABLE_freeze /
# STORABLE_thaw hooks (SX_HOOK), an unknown tag in a stream, a newer major
# version.  A truncated stream thaws to undef, as in perl.

package Storable;
use strict;
use warnings;
use Exporter 'import';
use Carp ();
use Scalar::Util ();

our $VERSION = '3.32';
our @EXPORT = qw(store retrieve);
our @EXPORT_OK = qw(
    nstore store_fd nstore_fd fd_retrieve
    freeze nfreeze thaw
    dclone
    retrieve_fd
    lock_store lock_nstore lock_retrieve
    file_magic read_magic
    BLESS_OK TIE_OK FLAGS_COMPAT
    stack_depth stack_depth_hash
);

our ($canonical, $forgive_me, $Deparse, $Eval);
our ($recursion_limit, $recursion_limit_hash);
$recursion_limit      = 512 if !defined $recursion_limit;
$recursion_limit_hash = 256 if !defined $recursion_limit_hash;

sub BLESS_OK     () { 2 }
sub TIE_OK       () { 4 }
sub FLAGS_COMPAT () { BLESS_OK | TIE_OK }
sub CAN_FLOCK    () { 1 }
$Storable::flags = FLAGS_COMPAT;
$Storable::downgrade_restricted = 1;
$Storable::accept_future_minor  = 1;

our ($LAST_NETORDER, $STORING, $RETRIEVING) = (0, 0, 0);
sub last_op_in_netorder { $LAST_NETORDER }
sub is_storing          { $STORING }
sub is_retrieving       { $RETRIEVING }
sub stack_depth         { $Storable::recursion_limit }
sub stack_depth_hash    { $Storable::recursion_limit_hash }

# The type tags (Storable.xs SX_*).
use constant {
    SX_OBJECT => 0, SX_LSCALAR => 1, SX_ARRAY => 2, SX_HASH => 3, SX_REF => 4,
    SX_UNDEF => 5, SX_INTEGER => 6, SX_DOUBLE => 7, SX_BYTE => 8, SX_NETINT => 9,
    SX_SCALAR => 10, SX_TIED_ARRAY => 11, SX_TIED_HASH => 12, SX_TIED_SCALAR => 13,
    SX_SV_UNDEF => 14, SX_SV_YES => 15, SX_SV_NO => 16, SX_BLESS => 17,
    SX_IX_BLESS => 18, SX_HOOK => 19, SX_OVERLOAD => 20, SX_TIED_KEY => 21,
    SX_TIED_IDX => 22, SX_UTF8STR => 23, SX_LUTF8STR => 24, SX_FLAG_HASH => 25,
    SX_CODE => 26, SX_WEAKREF => 27, SX_WEAKOVERLOAD => 28, SX_VSTRING => 29,
    SX_LVSTRING => 30, SX_SVUNDEF_ELEM => 31, SX_REGEXP => 32, SX_LOBJECT => 33,
    SX_BOOLEAN_TRUE => 34, SX_BOOLEAN_FALSE => 35,
};

my $BIN_MAJOR       = 2;
my $BIN_MINOR       = 12;
my $BIN_WRITE_MINOR = 11;
my $BYTEORDER       = '12345678';
my $IV_MIN          = -9223372036854775808;

# The native header is written in THIS machine's order; PCL runs on
# little-endian 64-bit hosts only, and a big-endian one must not write
# plausible-looking bytes in the wrong order.
die "Storable (PCL): only little-endian hosts are supported\n"
    if unpack('H*', pack('L', 1)) ne '01000000';

# ---------------------------------------------------------------- writing --

sub _croak { Carp::croak(@_) }

sub _header {
    my ($net) = @_;
    return pack('CC', ($BIN_MAJOR << 1) | 1, $BIN_WRITE_MINOR) if $net;
    return pack('CC', $BIN_MAJOR << 1, $BIN_WRITE_MINOR)
         . pack('C', length $BYTEORDER) . $BYTEORDER . pack('C4', 4, 8, 8, 8);
}

sub _len32 { $_[0]{net} ? pack('N', $_[1]) : pack('V', $_[1]) }

sub _w_string {
    my ($cx, $s) = @_;
    if ($s =~ /[^\x00-\xff]/) {
        utf8::encode($s);
        $cx->{out} .= length($s) <= 255
            ? pack('CC', SX_UTF8STR, length $s) . $s
            : pack('C', SX_LUTF8STR) . _len32($cx, length $s) . $s;
        return;
    }
    $cx->{out} .= length($s) <= 255
        ? pack('CC', SX_SCALAR, length $s) . $s
        : pack('C', SX_LSCALAR) . _len32($cx, length $s) . $s;
}

sub _w_integer {
    my ($cx, $iv) = @_;
    if ($iv >= -128 && $iv <= 127) {
        $cx->{out} .= pack('CC', SX_BYTE, $iv + 128);
    }
    elsif ($cx->{net}) {
        return _w_string($cx, "$iv") if $iv > 2147483647 || $iv < -2147483648;
        $cx->{out} .= pack('C', SX_NETINT) . pack('N', $iv & 0xffffffff);
    }
    else {
        $cx->{out} .= pack('C', SX_INTEGER) . pack('q<', $iv);
    }
}

# A plain (non-reference) scalar value.
sub _w_scalar {
    my ($cx, $v) = @_;
    if (!defined $v) {
        $cx->{out} .= pack('C', SX_UNDEF);
        return;
    }
    if (builtin::is_bool($v)) {
        $cx->{out} .= pack('C', $v ? SX_BOOLEAN_TRUE : SX_BOOLEAN_FALSE);
        return;
    }
    if (!builtin::created_as_number($v)) {
        _w_string($cx, $v);
        return;
    }
    # perl's question is "does the SV carry an IV": an integer does, and so does
    # an integral NV whose magnitude is below 2**53 (SvIV_please).  An integer
    # prints as digits; an NV of 2**53 or more prints with an exponent.
    my $digits = "$v" =~ /\A-?[0-9]+\z/;
    if ($v == $v && $v == int($v)
        && ($digits ? ($v >= $IV_MIN && $v < 9223372036854775808) : abs($v) < 9007199254740992)) {
        return _w_integer($cx, int $v);
    }
    if ($digits && $v > 0) {
        return _w_string($cx, "$v");      # a UV above IV_MAX: perl writes the digits
    }
    return _w_string($cx, "$v") if $cx->{net};
    $cx->{out} .= pack('C', SX_DOUBLE) . pack('d<', $v);
}

# An SV that holds $v: an array element, a hash value, the target of a ref
# to a ref.  Takes a tag of its own.
sub _w_sv {
    my ($cx, $v) = @_;
    $cx->{tag}++;
    if (ref $v) {
        _w_ref($cx, $v);
    }
    else {
        _w_scalar($cx, $v);
    }
}

# The reference itself (the SV holding $r has its tag already): SX_REF or
# SX_OVERLOAD, then the referent.
sub _w_ref {
    my ($cx, $r) = @_;
    my $class = Scalar::Util::blessed($r);
    my $ov = defined $class && _overloaded($class);
    $cx->{out} .= pack('C', $ov ? SX_OVERLOAD : SX_REF);
    _w_referent($cx, $r);
}

# Is CLASS overloaded?  `overload::Overloaded` may be supplied by the runtime
# rather than defined as a Perl sub, so it is called, not tested with `defined &`.
sub _overloaded {
    my ($class) = @_;
    my $ov = eval { overload::Overloaded($class) };
    return $ov ? 1 : 0;
}

sub _w_class {
    my ($cx, $class) = @_;
    my $ix = $cx->{classes}{$class};
    if (defined $ix) {
        $cx->{out} .= $ix <= 127 ? pack('CC', SX_IX_BLESS, $ix)
                                 : pack('CC', SX_IX_BLESS, 0x80) . _len32($cx, $ix);
        return;
    }
    $cx->{classes}{$class} = $cx->{nclass}++;
    my $len = length $class;
    $cx->{out} .= $len <= 127 ? pack('CC', SX_BLESS, $len) . $class
                              : pack('CC', SX_BLESS, 0x80) . _len32($cx, $len) . $class;
}

sub _w_cannot {
    my ($cx, $type, $r) = @_;
    _croak("Can't store $type items") if !$Storable::forgive_me;
    Carp::carp("Can't store item $type(" . sprintf('0x%x', Scalar::Util::refaddr($r)) . ")");
    my $s = "You lost $type(" . sprintf('0x%x', Scalar::Util::refaddr($r)) . ")";
    _w_string($cx, $s);
}

sub _w_referent {
    my ($cx, $r) = @_;
    my $addr = Scalar::Util::refaddr($r);
    my $seen = $cx->{seen}{$addr};
    if (defined $seen) {
        $cx->{out} .= pack('C', SX_OBJECT) . pack('N', $seen);
        return;
    }
    $cx->{seen}{$addr} = $cx->{tag}++;
    $cx->{keep}{$addr} = $r;          # keep the referent alive: its address is a key
    my $type = Scalar::Util::reftype($r);
    if (++$cx->{depth} > 10000) {
        _croak('Max. recursion depth with nested structures exceeded');
    }
    my $class = Scalar::Util::blessed($r);
    if (defined $class && $type ne 'CODE' && $type ne 'GLOB') {
        if ($class->can('STORABLE_freeze')) {
            _croak("Storable (PCL): the STORABLE_freeze hook of class $class is not implemented (docs/not-supported.md)");
        }
        _w_class($cx, $class);
    }
    if ($type eq 'ARRAY') {
        return _w_tied($cx, SX_TIED_ARRAY, tied(@$r)) if tied(@$r);
        my $n = @$r;
        $cx->{out} .= pack('C', SX_ARRAY) . _len32($cx, $n);
        for my $i (0 .. $n - 1) {
            if (!exists $r->[$i]) {
                $cx->{tag}++;
                $cx->{out} .= pack('C', SX_SV_UNDEF);
                next;
            }
            _w_sv($cx, $r->[$i]);
        }
    }
    elsif ($type eq 'HASH') {
        return _w_tied($cx, SX_TIED_HASH, tied(%$r)) if tied(%$r);
        _w_hash($cx, $r);
    }
    elsif ($type eq 'SCALAR' || $type eq 'VSTRING') {
        return _w_tied($cx, SX_TIED_SCALAR, tied($$r)) if tied($$r);
        _w_scalar($cx, $$r);
    }
    elsif ($type eq 'REF') {
        _w_ref($cx, $$r);
    }
    elsif ($type eq 'CODE') {
        _croak("Storable (PCL): \$Storable::Deparse is not implemented (no B::Deparse)")
            if $Storable::Deparse && !$Storable::forgive_me;
        _w_cannot($cx, 'CODE', $r);
    }
    elsif ($type eq 'REGEXP') {
        my ($pat, $fl) = re::regexp_pattern($r);
        _croak("Storable (PCL): a regexp whose pattern holds a character above 0xFF is not implemented")
            if $pat =~ /[^\x00-\xff]/;
        $cx->{out} .= length($pat) <= 255
            ? pack('CCC', SX_REGEXP, 0, length $pat)
            : pack('CC', SX_REGEXP, 1) . _len32($cx, length $pat);   # SHR_U32_RE_LEN
        $cx->{out} .= $pat . pack('C', length $fl) . $fl;
    }
    else {
        # GLOB, IO, FORMAT, LVALUE: perl's own "Can't store X items".
        _w_cannot($cx, $type, $r);
    }
    $cx->{depth}--;
}

# A tied container is its tag + the tie object (perl's store_tied).
sub _w_tied {
    my ($cx, $tag, $obj) = @_;
    $cx->{out} .= pack('C', $tag);
    _w_sv($cx, $obj);
    $cx->{depth}--;
}

sub _w_hash {
    my ($cx, $h) = @_;
    my @keys = keys %$h;
    @keys = sort @keys if $Storable::canonical;
    my $flagged = grep { /[^\x00-\xff]/ } @keys;
    if ($flagged) {
        $cx->{out} .= pack('CC', SX_FLAG_HASH, 0) . _len32($cx, scalar @keys);
    }
    else {
        $cx->{out} .= pack('C', SX_HASH) . _len32($cx, scalar @keys);
    }
    for my $k (@keys) {
        _w_sv($cx, $h->{$k});
        my $kb = $k;
        my $kflags = 0;
        if ($kb =~ /[^\x00-\xff]/) {
            utf8::encode($kb);
            $kflags = 1;              # SHV_K_UTF8
        }
        $cx->{out} .= pack('C', $kflags) if $flagged;
        $cx->{out} .= _len32($cx, length $kb) . $kb;
    }
}

# The body of freeze / nfreeze / store / nstore: header (without the file
# magic) + the root referent.
sub _do_store {
    my ($root, $net) = @_;
    _croak('not a reference') if !ref $root;
    my $cx = { net => $net, out => _header($net), tag => 0, seen => {}, keep => {},
               classes => {}, nclass => 0, depth => 0 };
    local $STORING = 1;
    _w_referent($cx, $root);
    $LAST_NETORDER = $net ? 1 : 0;
    return $cx->{out};
}

# ---------------------------------------------------------------- reading --

# The reader works over a buffer that, for a filehandle, is refilled on demand,
# so `fd_retrieve` consumes exactly one stored object, as perl does.
sub _bytes {
    my ($cx, $n) = @_;
    my $have = length($cx->{buf}) - $cx->{pos};
    if ($have < $n && $cx->{fh}) {
        my $more = '';
        my $got = read($cx->{fh}, $more, $n - $have);
        $cx->{buf} .= $more if $got;
        $have = length($cx->{buf}) - $cx->{pos};
    }
    die \"truncated" if $have < $n;
    my $s = substr($cx->{buf}, $cx->{pos}, $n);
    $cx->{pos} += $n;
    return $s;
}

sub _byte  { unpack('C', _bytes($_[0], 1)) }
sub _r_len { $_[0]{net} ? unpack('N', _bytes($_[0], 4)) : unpack('V', _bytes($_[0], 4)) }

sub _r_header {
    my ($cx, $file) = @_;
    my $what = $file ? 'file' : 'string';
    if ($file) {
        my $magic = eval { _bytes($cx, 4) };
        _croak("Magic number checking on storable $what failed")
            if !defined $magic || $magic ne 'pst0';
    }
    my $vb = eval { _byte($cx) };
    _croak("Magic number checking on storable $what failed") if !defined $vb;
    my $major = $vb >> 1;
    my $net = $vb & 1;
    my $minor = $major >= 2 ? _byte($cx) : 0;
    if ($major > $BIN_MAJOR
        || ($major == $BIN_MAJOR && $minor > $BIN_MINOR && !$Storable::accept_future_minor)) {
        _croak("Storable binary image v$major.$minor more recent than I am (v$BIN_MAJOR.$BIN_MINOR)");
    }
    _croak("Storable (PCL): binary image v$major.$minor predates the 2.x format; not supported")
        if $major < 2;
    $cx->{net} = $net;
    $cx->{minor} = $minor;
    if (!$net) {
        my $bo = _bytes($cx, _byte($cx));
        _croak('Byte order is not compatible') if $bo ne $BYTEORDER;
        my ($int, $long, $ptr, $nv) = unpack('C4', _bytes($cx, 4));
        _croak('Integer size is not compatible') if $int != 4;
        _croak('Long integer size is not compatible') if $long != 8;
        _croak('Pointer size is not compatible') if $ptr != 8;
        _croak('Double size is not compatible') if $nv != 8;
    }
}

sub _corrupt {
    my ($cx) = @_;
    my $kind = $cx->{fh} ? 'file' : 'string';
    _croak("Corrupted storable $kind (binary v2.$cx->{minor})");
}

# Retrieve one SV; returns a REFERENCE to it (so a scalar, an array and a
# hash are handled alike, and an SX_OBJECT back-reference returns the very
# same reference -- sharing and cycles).
sub _r_item {
    my ($cx, $class) = @_;
    my $t = _byte($cx);
    my $seen = $cx->{seen};
    if ($t == SX_OBJECT) {
        my $tag = unpack('N', _bytes($cx, 4));
        _corrupt($cx) if $tag >= @$seen;
        return $seen->[$tag];
    }
    my $item;
    if ($t == SX_BLESS || $t == SX_IX_BLESS) {
        my $name;
        my $len = _byte($cx);
        if ($t == SX_BLESS) {
            $len = _r_len($cx) if $len & 0x80;
            $name = _bytes($cx, $len);
            push @{ $cx->{classes} }, $name;
        }
        else {
            $len = _r_len($cx) if $len & 0x80;
            $name = $cx->{classes}[$len];
            _corrupt($cx) if !defined $name;
        }
        # without BLESS_OK perl retrieves the object UNblessed (its BLESS macro)
        $name = undef if !($cx->{flags} & BLESS_OK);
        return _r_item($cx, $name);
    }
    if ($t == SX_ARRAY) {
        my @a;
        $item = \@a;
        push @$seen, $item;
        bless $item, $class if defined $class;
        my $n = _r_len($cx);
        my $hole = Scalar::Util::refaddr($cx->{hole});
        for my $i (0 .. $n - 1) {
            my $e = _r_item($cx);
            # a nonexistent slot stays nonexistent (perl: SV_UNDEF / SVUNDEF_ELEM)
            $a[$i] = $$e if Scalar::Util::refaddr($e) != $hole;
        }
        return $item;
    }
    if ($t == SX_HASH || $t == SX_FLAG_HASH) {
        my %h;
        $item = \%h;
        push @$seen, $item;
        bless $item, $class if defined $class;
        my $flagged = $t == SX_FLAG_HASH;
        _byte($cx) if $flagged;                # hash flags (restricted: not modelled)
        my $n = _r_len($cx);
        for (1 .. $n) {
            my $v = _r_item($cx);
            my $kf = $flagged ? _byte($cx) : 0;
            _croak('Storable (PCL): a hash key stored as an SV (tied) is not implemented')
                if $kf & 0x08;
            my $k = _bytes($cx, _r_len($cx));
            utf8::decode($k) if $kf & 0x01;
            $h{$k} = $$v if !($kf & 0x10);     # SHV_K_PLACEHOLDER: a restricted slot
        }
        return $item;
    }
    if ($t == SX_REF || $t == SX_WEAKREF || $t == SX_OVERLOAD || $t == SX_WEAKOVERLOAD) {
        my $rv;
        $item = \$rv;
        push @$seen, $item;
        bless $item, $class if defined $class;
        $rv = _r_item($cx);
        _check_overload($rv) if $t == SX_OVERLOAD || $t == SX_WEAKOVERLOAD;
        return $item;
    }
    my $v;
    if ($t == SX_SV_UNDEF || $t == SX_SVUNDEF_ELEM) {
        push @$seen, $cx->{hole};
        return $cx->{hole};
    }
    if    ($t == SX_UNDEF) { $v = undef }
    elsif ($t == SX_SV_YES || $t == SX_BOOLEAN_TRUE)  { $v = !!1 }
    elsif ($t == SX_SV_NO  || $t == SX_BOOLEAN_FALSE) { $v = !!0 }
    elsif ($t == SX_BYTE)    { $v = _byte($cx) - 128 }
    elsif ($t == SX_INTEGER) { $v = unpack('q<', _bytes($cx, 8)) }
    elsif ($t == SX_NETINT)  { $v = unpack('l>', _bytes($cx, 4)) }
    elsif ($t == SX_DOUBLE)  { $v = unpack('d<', _bytes($cx, 8)) }
    elsif ($t == SX_SCALAR || $t == SX_UTF8STR || $t == SX_VSTRING) {
        $v = _bytes($cx, _byte($cx));
        utf8::decode($v) if $t == SX_UTF8STR;
    }
    elsif ($t == SX_LSCALAR || $t == SX_LUTF8STR || $t == SX_LVSTRING) {
        $v = _bytes($cx, _r_len($cx));
        utf8::decode($v) if $t == SX_LUTF8STR;
    }
    elsif ($t == SX_HOOK) {
        _croak("Storable (PCL): STORABLE_freeze/STORABLE_thaw hook data (SX_HOOK) is not implemented (docs/not-supported.md)");
    }
    elsif ($t == SX_TIED_ARRAY || $t == SX_TIED_HASH || $t == SX_TIED_SCALAR) {
        _croak("Tying is disabled.") if !($cx->{flags} & TIE_OK);
        my (@a, %h, $s);
        $item = $t == SX_TIED_ARRAY ? \@a : $t == SX_TIED_HASH ? \%h : \$s;
        push @$seen, $item;
        my $obj = _r_item($cx);
        # The stored object IS the tie object: a TIE* that hands it back.
        if    ($t == SX_TIED_ARRAY) { tie @a, 'Storable::_TieWith', $$obj }
        elsif ($t == SX_TIED_HASH)  { tie %h, 'Storable::_TieWith', $$obj }
        else                        { tie $s, 'Storable::_TieWith', $$obj }
        bless $item, $class if defined $class;
        return $item;
    }
    elsif ($t == SX_TIED_KEY || $t == SX_TIED_IDX) {
        _croak("Storable (PCL): tied items (tag $t) are not implemented (docs/not-supported.md)");
    }
    elsif ($t == SX_CODE) {
        _croak("Can't eval, please set \$Storable::Eval to a true value");
    }
    elsif ($t == SX_REGEXP) {
        my $op = _byte($cx);
        my $pat = _bytes($cx, ($op & 1) ? _r_len($cx) : _byte($cx));
        my $fl = _bytes($cx, _byte($cx));
        _corrupt($cx) if $fl !~ /\A[msixpdualn]*\z/;
        my $qr = eval "qr/\$pat/$fl";
        die $@ if !defined $qr;
        bless $qr, $class if defined $class && $class ne 'Regexp';
        push @$seen, $qr;
        return $qr;
    }
    elsif ($t == SX_LOBJECT) {
        _croak("Storable (PCL): stream tag $t (an object above 2 GB) is not implemented (docs/not-supported.md)");
    }
    else {
        _corrupt($cx);
    }
    # vstring magic: the stored vstring text precedes the scalar it decorates.
    return _r_item($cx, $class) if $t == SX_VSTRING || $t == SX_LVSTRING;
    $item = \$v;
    push @$seen, $item;
    bless $item, $class if defined $class;
    return $item;
}

sub _check_overload {
    my ($rv) = @_;
    my $class = Scalar::Util::blessed($rv);
    if (!defined $class) {
        _croak('Cannot restore overloading on ' . Scalar::Util::reftype($rv)
               . sprintf('(0x%x)', Scalar::Util::refaddr($rv)) . ' (package <unknown>)');
    }
    my $ok = _overloaded($class);
    if (!$ok) {
        (my $file = "$class.pm") =~ s{::}{/}g;
        eval { require $file };
        $ok = _overloaded($class);
    }
    _croak('Cannot restore overloading on ' . Scalar::Util::reftype($rv)
           . sprintf('(0x%x)', Scalar::Util::refaddr($rv)) . " (package $class)") if !$ok;
}

sub _do_retrieve {
    my ($src, $fh, $file, $flg) = @_;
    my $cx = { buf => defined $src ? $src : '', pos => 0, fh => $fh, seen => [],
               classes => [], minor => 0, hole => \my $hole,
               # `$flg`, not `$flags`: a lexical $flags shadows $Storable::flags in PCL (#3080).
               flags => defined $flg ? $flg : $Storable::flags };
    local $RETRIEVING = 1;
    _r_header($cx, $file);
    my $root = eval { _r_item($cx) };
    if (my $e = $@) {
        return undef if ref $e eq 'SCALAR' && $$e eq 'truncated';
        die $e;
    }
    $LAST_NETORDER = $cx->{net};
    return $root;
}

# ------------------------------------------------------------------ API --

sub freeze {
    my $self = shift;
    _croak('not a reference') if !ref $self;
    _croak('too many arguments') if @_;
    return _do_store($self, 0);
}

sub nfreeze {
    my $self = shift;
    _croak('not a reference') if !ref $self;
    _croak('too many arguments') if @_;
    return _do_store($self, 1);
}

sub thaw {
    my ($frozen, $flg) = @_;
    return undef if !defined $frozen;
    return _do_retrieve($frozen, undef, 0, $flg);
}

sub dclone {
    my ($self) = @_;
    _croak('Not a reference') if !ref $self;
    my $frozen = _do_store($self, 0);
    return _do_retrieve($frozen, undef, 0, FLAGS_COMPAT);
}

sub _store_file {
    my ($net, $lock, $self, @rest) = @_;
    _croak('not a reference') if !ref $self;
    _croak('wrong argument number') if @rest != 1;
    my ($file) = @rest;
    my $fh;
    if ($lock) {
        open($fh, '>>', $file) || _croak("can't write into $file: $!");
        flock($fh, 2) || _croak("can't get exclusive lock on $file: $!");
        truncate $fh, 0;
    }
    else {
        open($fh, '>', $file) || _croak("can't create $file: $!");
    }
    binmode $fh;
    my $data = eval { _do_store($self, $net) };
    if (my $e = $@) {
        close $fh;
        unlink $file;
        die $e;
    }
    print {$fh} 'pst0', $data;
    if (!close $fh) {
        unlink $file;
        return undef;
    }
    return 1;
}

sub store       { _store_file(0, 0, @_) }
sub nstore      { _store_file(1, 0, @_) }
sub lock_store  { _store_file(0, 1, @_) }
sub lock_nstore { _store_file(1, 1, @_) }

sub _store_fd {
    my ($net, $self, @rest) = @_;
    _croak('not a reference') if !ref $self;
    _croak('too many arguments') if @rest != 1;
    my ($fh) = @rest;
    _croak('not a valid file descriptor') if !defined fileno($fh);
    print {$fh} 'pst0', _do_store($self, $net);
    return 1;
}

sub store_fd  { _store_fd(0, @_) }
sub nstore_fd { _store_fd(1, @_) }

sub _retrieve_file {
    my ($file, $lock, $flg) = @_;
    my $fh;
    open($fh, '<', $file) || _croak("can't open $file: $!");
    binmode $fh;
    if ($lock) {
        flock($fh, 1) || _croak("can't get shared lock on $file: $!");
    }
    my $self = _do_retrieve('', $fh, 1, $flg);
    close $fh;
    return $self;
}

sub retrieve      { _retrieve_file($_[0], 0, $_[1]) }
sub lock_retrieve { _retrieve_file($_[0], 1, $_[1]) }

sub fd_retrieve {
    my ($fh, $flg) = @_;
    _croak('not a valid file descriptor') if !defined fileno($fh);
    return _do_retrieve('', $fh, 1, $flg);
}
sub retrieve_fd { &fd_retrieve }

sub file_magic {
    _croak('Storable::file_magic is not implemented in PCL (docs/not-supported.md)');
}
sub read_magic {
    _croak('Storable::read_magic is not implemented in PCL (docs/not-supported.md)');
}

# The TIE class a retrieved tied container is tied through: its TIE* returns
# the stored tie object, so the container dispatches to that object's class.
package Storable::_TieWith;
sub TIEARRAY  { $_[1] }
sub TIEHASH   { $_[1] }
sub TIESCALAR { $_[1] }

1;
