use strict;
use warnings;

my $CAN_ENDIAN = 'sSiIlLqQjJfFdDpP';
my $CAN_SHRIEK = 'sSiIlLnNvVxX.@';

# Returns (nbytes, signed_flag, big_endian_default) for integer types; () otherwise.
sub _pack_type_info {
    my ($ch, $bang) = @_;
    if    ($ch eq 'c') { return (1, 1, 0) }
    elsif ($ch eq 'C') { return (1, 0, 0) }
    elsif ($ch eq 's') { return (2, 1, 0) }
    elsif ($ch eq 'S') { return (2, 0, 0) }
    elsif ($ch eq 'n') { return (2, ($bang ? 1 : 0), 1) }
    elsif ($ch eq 'v') { return (2, ($bang ? 1 : 0), 0) }
    elsif ($ch eq 'i') { return (4, 1, 0) }
    elsif ($ch eq 'I') { return (4, 0, 0) }
    elsif ($ch eq 'l') { return (($bang ? 8 : 4), 1, 0) }
    elsif ($ch eq 'L') { return (($bang ? 8 : 4), 0, 0) }
    elsif ($ch eq 'N') { return (4, ($bang ? 1 : 0), 1) }
    elsif ($ch eq 'V') { return (4, ($bang ? 1 : 0), 0) }
    elsif ($ch eq 'q') { return (8, 1, 0) }
    elsif ($ch eq 'Q') { return (8, 0, 0) }
    elsif ($ch eq 'j') { return (8, 1, 0) }
    elsif ($ch eq 'J') { return (8, 0, 0) }
    return ();
}

sub _pack_skip_ws {
    my ($s, $ti) = @_;
    my $tlen = length($s);
    while ($ti < $tlen) {
        my $ch = substr($s, $ti, 1);
        # Perl pack templates allow commas as separators (like whitespace)
        if ($ch eq ' ' || $ch eq "\t" || $ch eq "\n" || $ch eq "\r" || $ch eq "\f" || $ch eq ',') {
            $ti++;
        } elsif ($ch eq '#') {
            $ti++;
            while ($ti < $tlen && substr($s, $ti, 1) ne "\n") { $ti++ }
            $ti++ if $ti < $tlen;
        } else {
            last;
        }
    }
    return $ti;
}

# Returns index of matching ')'. Starts just after the opening '('.
sub _pack_find_group_end {
    my ($s, $ti) = @_;
    my $tlen = length($s);
    my $depth = 1;
    while ($ti < $tlen && $depth > 0) {
        my $ch = substr($s, $ti, 1);
        if ($ch eq '#') {
            $ti++;
            while ($ti < $tlen && substr($s, $ti, 1) ne "\n") { $ti++ }
        } elsif ($ch eq '(') {
            $depth++; $ti++;
        } elsif ($ch eq ')') {
            $depth--;
            $ti++ if $depth > 0;
        } else {
            $ti++;
        }
    }
    return $ti;
}

# Parse !, >, < modifiers. Updates $$ti_ref. Dies on invalid modifier.
sub _pack_parse_mods {
    my ($tmpl, $ti_ref, $inh_be, $inh_le, $ch, $ctx) = @_;
    my $tlen = length($tmpl);
    my ($bang, $be, $le) = (0, $inh_be, $inh_le);
    my ($got_be, $got_le) = (0, 0);
    while ($$ti_ref < $tlen) {
        my $m = substr($tmpl, $$ti_ref, 1);
        if ($m eq '!') {
            die "'!' allowed only after types $CAN_SHRIEK in $ctx\n"
                unless index($CAN_SHRIEK, $ch) >= 0;
            $bang = 1; $$ti_ref++;
        } elsif ($m eq '>') {
            die "'>' allowed only after types $CAN_ENDIAN in $ctx\n"
                unless index($CAN_ENDIAN, $ch) >= 0 || $ch eq '(';
            die "Can't use both '<' and '>' after type '$ch' in $ctx\n" if $got_le;
            $be = 1; $le = 0; $got_be = 1; $$ti_ref++;
        } elsif ($m eq '<') {
            die "'<' allowed only after types $CAN_ENDIAN in $ctx\n"
                unless index($CAN_ENDIAN, $ch) >= 0 || $ch eq '(';
            die "Can't use both '<' and '>' after type '$ch' in $ctx\n" if $got_be;
            $le = 1; $be = 0; $got_le = 1; $$ti_ref++;
        } else {
            last;
        }
    }
    return ($bang, $be, $le);
}

# Compute the byte size that template $tmpl would produce/consume.
# Used for [TEMPLATE] count notation (e.g. x[A3 N] means skip sizeof(A3 N) bytes).
sub _pack_template_size {
    my ($tmpl) = @_;
    my $pos = 0;
    my $ti = 0;
    my $tlen = length($tmpl);
    while (1) {
        $ti = _pack_skip_ws($tmpl, $ti);
        last if $ti >= $tlen;
        my $ch = substr($tmpl, $ti, 1); $ti++;
        my ($grpbeg, $grpend) = (undef, undef);
        if ($ch eq '(') {
            $grpend = _pack_find_group_end($tmpl, $ti);
            $grpbeg = $ti; $ti = $grpend + 1;
        }
        my $bang = 0;
        while ($ti < $tlen && substr($tmpl, $ti, 1) =~ /[!<>]/) {
            $bang = 1 if substr($tmpl, $ti, 1) eq '!';
            $ti++;
        }
        $ti = _pack_skip_ws($tmpl, $ti);
        my ($all, $count, $nrep) = _pack_parse_count($tmpl, \$ti);
        $nrep = 1 unless defined $nrep && $nrep >= 1;
        if (defined $grpbeg) {
            my $inner = substr($tmpl, $grpbeg, $grpend - $grpbeg);
            $pos += _pack_template_size($inner) * $nrep;
            next;
        }
        if ($ch eq '@') { $pos = $bang ? (defined($count) ? $count : 0) : (0 + (defined($count) ? $count : 0)); next }
        # Note: @!N is absolute, @N is relative to group (here group offset is 0 so same)
        if ($ch eq 'x') {
            if ($bang) {
                my $n = $nrep > 0 ? $nrep : 1;
                $pos += ($n - ($pos % $n)) % $n;
            } else { $pos += $nrep }
            next;
        }
        if ($ch eq 'X') {
            if ($bang) {
                my $n = $nrep > 0 ? $nrep : 1;
                $pos = int($pos / $n) * $n;
            } else { $pos -= $nrep; $pos = 0 if $pos < 0 }
            next;
        }
        my ($nb) = _pack_type_info($ch, $bang);
        if ($nb) { $pos += $nb * $nrep; next }
        if ($ch eq 'A' || $ch eq 'a' || $ch eq 'Z') { $pos += $nrep; next }
        if ($ch eq 'B' || $ch eq 'b') { $pos += int(($nrep+7)/8); next }
        if ($ch eq 'H' || $ch eq 'h') { $pos += int(($nrep+1)/2); next }
        if ($ch eq 'f' || $ch eq 'F') { $pos += 4*$nrep; next }
        if ($ch eq 'd' || $ch eq 'D') { $pos += 8*$nrep; next }
        if ($ch eq 'p' || $ch eq 'P') { $pos += 8*$nrep; next }
        if ($ch eq 'W' || $ch eq 'U' || $ch eq 'w') { $pos += $nrep; next }
        if ($ch eq '.') { next }  # position marker, no bytes
        # u: variable, skip
    }
    return $pos;
}

# Parse count: *, [N], [TEMPLATE], digits. Returns ($all, $count, $nrep).
sub _pack_parse_count {
    my ($tmpl, $ti_ref) = @_;
    my $tlen = length($tmpl);
    $$ti_ref = _pack_skip_ws($tmpl, $$ti_ref);
    if ($$ti_ref < $tlen && substr($tmpl, $$ti_ref, 1) eq '*') {
        $$ti_ref++; return (1, undef, 1);
    }
    if ($$ti_ref < $tlen && substr($tmpl, $$ti_ref, 1) eq '[') {
        $$ti_ref++;  # skip '['
        my $start = $$ti_ref;
        my $depth = 1;
        while ($$ti_ref < $tlen && $depth > 0) {
            my $c = substr($tmpl, $$ti_ref, 1); $$ti_ref++;
            if ($c eq '[') { $depth++ }
            elsif ($c eq ']') { $depth-- }
        }
        die "No group ending character ']' found in template\n" if $depth > 0;
        # $$ti_ref is now just past the closing ']'
        my $inner = substr($tmpl, $start, $$ti_ref - $start - 1);
        if ($inner =~ /^\d+$/) {
            my $n = $inner + 0;
            return (0, $n, $n);
        }
        my $n = _pack_template_size($inner);
        return (0, $n, $n);
    }
    if ($$ti_ref < $tlen && substr($tmpl, $$ti_ref, 1) =~ /\d/) {
        my $n = 0;
        while ($$ti_ref < $tlen && substr($tmpl, $$ti_ref, 1) =~ /\d/) {
            $n = $n * 10 + substr($tmpl, $$ti_ref, 1); $$ti_ref++;
        }
        return (0, $n, $n);
    }
    return (0, 1, 1);
}

# Pack integer value as nbytes in specified byte order.
sub _pack_emit_int {
    my ($val, $nbytes, $signed, $be) = @_;
    $val = int($val + 0);
    my $result = '';
    if ($be) {
        for (my $k = $nbytes - 1; $k >= 0; $k--) {
            $result .= chr(($val >> (8 * $k)) & 0xFF);
        }
    } else {
        for (my $k = 0; $k < $nbytes; $k++) {
            $result .= chr(($val >> (8 * $k)) & 0xFF);
        }
    }
    return $result;
}

# Read nbytes from $s at $si as integer.
sub _unpack_read_int {
    my ($s, $si, $nbytes, $be, $signed) = @_;
    my $slen = length($s);
    my $v = 0;
    if ($be) {
        for (my $k = 0; $k < $nbytes; $k++) {
            $v = ($v << 8) | (($si+$k < $slen) ? ord(substr($s,$si+$k,1)) : 0);
        }
    } else {
        for (my $k = $nbytes - 1; $k >= 0; $k--) {
            $v = ($v << 8) | (($si+$k < $slen) ? ord(substr($s,$si+$k,1)) : 0);
        }
    }
    if ($signed) {
        if ($nbytes == 8) {
            # Avoid NV precision issues for 64-bit: use UV bitwise shift to detect sign bit
            if ($v >> 63) { $v = -(~$v + 1) }  # two's-complement negation via UV bitwise ops
        } else {
            my $max = 2 ** ($nbytes * 8);
            $v -= $max if $v >= $max / 2;
        }
    }
    return $v;
}

# Float stubs — bodies replaced with SBCL sb-kernel calls after ./pl2cl.
sub _pack_float32   { my ($val, $be) = @_; return "" }
sub _pack_float64   { my ($val, $be) = @_; return "" }
sub _unpack_float32 { my ($s, $si, $be) = @_; return 0.0 }
sub _unpack_float64 { my ($s, $si, $be) = @_; return 0.0 }

# Pack string/bit/uuencode types. Each consumes ONE arg from $args_ref.
sub _pack_str_one {
    my ($ch, $arg, $nrep, $star, $result_ref) = @_;
    $arg = '' unless defined $arg;
    my $slen = length($arg);
    if ($ch eq 'a') {
        my $len = $star ? $slen : $nrep;
        for (my $k = 0; $k < $len; $k++) {
            $$result_ref .= $k < $slen ? substr($arg,$k,1) : chr(0);
        }
    } elsif ($ch eq 'A') {
        my $len = $star ? $slen : $nrep;
        for (my $k = 0; $k < $len; $k++) {
            $$result_ref .= $k < $slen ? substr($arg,$k,1) : ' ';
        }
    } elsif ($ch eq 'Z') {
        my $len = $star ? $slen + 1 : $nrep;
        my $body = $len > 1 ? $len - 1 : 0;
        for (my $k = 0; $k < $body; $k++) {
            $$result_ref .= $k < $slen ? substr($arg,$k,1) : chr(0);
        }
        $$result_ref .= chr(0);
    } elsif ($ch eq 'b') {
        my $nbits = $star ? $slen : $nrep;
        for (my $bs = 0; $bs < $nbits; $bs += 8) {
            my $byte = 0;
            for (my $bit = 0; $bit < 8 && $bs+$bit < $nbits; $bit++) {
                my $idx = $bs + $bit;
                $byte |= (1 << $bit) if $idx < $slen && substr($arg,$idx,1) eq '1';
            }
            $$result_ref .= chr($byte);
        }
    } elsif ($ch eq 'B') {
        my $nbits = $star ? $slen : $nrep;
        for (my $bs = 0; $bs < $nbits; $bs += 8) {
            my $byte = 0;
            for (my $bit = 0; $bit < 8 && $bs+$bit < $nbits; $bit++) {
                my $idx = $bs + $bit;
                $byte |= (1 << (7 - $bit)) if $idx < $slen && substr($arg,$idx,1) eq '1';
            }
            $$result_ref .= chr($byte);
        }
    } elsif ($ch eq 'H') {
        my $nyb = $star ? $slen : $nrep;  # count is number of nybbles
        for (my $k = 0; $k < $nyb; $k += 2) {
            my $hi = $k   < $slen ? hex(substr($arg,$k,  1)) : 0;
            my $lo = $k+1 < $slen ? hex(substr($arg,$k+1,1)) : 0;
            $$result_ref .= chr(($hi << 4) | $lo);
        }
    } elsif ($ch eq 'h') {
        my $nyb = $star ? $slen : $nrep;  # count is number of nybbles
        for (my $k = 0; $k < $nyb; $k += 2) {
            my $lo = $k   < $slen ? hex(substr($arg,$k,  1)) : 0;
            my $hi = $k+1 < $slen ? hex(substr($arg,$k+1,1)) : 0;
            $$result_ref .= chr(($hi << 4) | $lo);
        }
    } elsif ($ch eq 'u') {
        for (my $cs = 0; $cs < $slen; $cs += 45) {
            my $ce = $cs + 45 < $slen ? $cs + 45 : $slen;
            my $chunk = substr($arg, $cs, $ce - $cs);
            my $clen = length($chunk);
            $$result_ref .= chr(32 + $clen);
            for (my $k = 0; $k < $clen; $k += 3) {
                my $b0 = ord(substr($chunk,$k,1));
                my $b1 = $k+1 < $clen ? ord(substr($chunk,$k+1,1)) : 0;
                my $b2 = $k+2 < $clen ? ord(substr($chunk,$k+2,1)) : 0;
                my $cm = ($b0 << 16) | ($b1 << 8) | $b2;
                my $uu = sub { my $c = 32 + ($_[0] & 63); $c == 32 ? 96 : $c };
                $$result_ref .= chr($uu->(($cm>>18)&63)) . chr($uu->(($cm>>12)&63))
                              . chr($uu->(($cm>> 6)&63)) . chr($uu->( $cm    &63));
            }
            $$result_ref .= "\n";
        }
    }
}

# Encode one UTF-8 codepoint, append to $$r.
sub _pack_utf8_char {
    my ($code, $r) = @_;
    if    ($code < 0x80)    { $$r .= chr($code) }
    elsif ($code < 0x800)   { $$r .= chr(0xC0|($code>>6)) . chr(0x80|($code&0x3F)) }
    elsif ($code < 0x10000) { $$r .= chr(0xE0|($code>>12)) . chr(0x80|(($code>>6)&0x3F)) . chr(0x80|($code&0x3F)) }
    else  { $$r .= chr(0xF0|($code>>18)) . chr(0x80|(($code>>12)&0x3F)) . chr(0x80|(($code>>6)&0x3F)) . chr(0x80|($code&0x3F)) }
}

# Main pack loop. Reads from $args_ref via $$ai_ref, appends to $$result_ref.
# $out_base: position in $$result_ref where the current group started (for @ relative offsets).
sub _pack_tmpl {
    my ($tmpl, $ai_ref, $args_ref, $result_ref, $inh_be, $inh_le, $out_base) = @_;
    $out_base = 0 unless defined $out_base;
    my $nargs = scalar(@$args_ref);
    my $ti = 0;
    my $tlen = length($tmpl);
    while (1) {
        $ti = _pack_skip_ws($tmpl, $ti);
        last if $ti >= $tlen;
        my $ch = substr($tmpl, $ti, 1); $ti++;
        my ($grpbeg, $grpend) = (undef, undef);
        if ($ch eq '(') {
            $grpend = _pack_find_group_end($tmpl, $ti);
            $grpbeg = $ti; $ti = $grpend + 1; $ch = '(';
        }
        my ($bang, $be, $le) = _pack_parse_mods($tmpl, \$ti, $inh_be, $inh_le, $ch, 'pack');
        $ti = _pack_skip_ws($tmpl, $ti);
        my ($star, $count, $nrep) = _pack_parse_count($tmpl, \$ti);
        $ti = _pack_skip_ws($tmpl, $ti);

        # Slash — count prefix: ch encodes length, next char is data format
        if ($ti < $tlen && substr($tmpl, $ti, 1) eq '/') {
            $ti++;
            my $darg = ($$ai_ref < $nargs) ? $args_ref->[$$ai_ref++] : '';
            $darg = '' unless defined $darg;
            my $dlen = length($darg);
            my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang);
            if ($nb) {
                $$result_ref .= _pack_emit_int($dlen, $nb, $sig, $be ? 1 : ($le ? 0 : $dbe));
            } elsif ($ch eq 'A' || $ch eq 'a') {
                _pack_str_one($ch, "$dlen", 1, 0, $result_ref);
            }
            $ti = _pack_skip_ws($tmpl, $ti); last if $ti >= $tlen;
            my $dfmt = substr($tmpl, $ti, 1); $ti++;
            my ($dbang, $dbe2, $dle2) = _pack_parse_mods($tmpl, \$ti, $be, $le, $dfmt, 'pack');
            $ti = _pack_skip_ws($tmpl, $ti);
            my ($dstar2, $dcnt2, $dnrep2) = _pack_parse_count($tmpl, \$ti);
            _pack_str_one($dfmt, $darg, $dlen, 0, $result_ref)
                if $dfmt eq 'a' || $dfmt eq 'A' || $dfmt eq 'Z';
            if ($dfmt ne 'a' && $dfmt ne 'A' && $dfmt ne 'Z') {
                my ($dnb, $dsig, $ddbe) = _pack_type_info($dfmt, $dbang);
                if ($dnb) {
                    $$result_ref .= _pack_emit_int($dlen, $dnb, $dsig, $dbe2 ? 1 : ($dle2 ? 0 : $ddbe));
                }
            }
            next;
        }

        # Group
        if (defined $grpbeg) {
            my $inner = substr($tmpl, $grpbeg, $grpend - $grpbeg);
            if ($star) {
                while ($$ai_ref < $nargs) {
                    my $ai_before = $$ai_ref;
                    my $iter_base = length($$result_ref);
                    _pack_tmpl($inner, $ai_ref, $args_ref, $result_ref, $be, $le, $iter_base);
                    last if $$ai_ref == $ai_before;  # no progress: avoid infinite loop
                }
            } else {
                for (my $r = 0; $r < $nrep; $r++) {
                    my $iter_base = length($$result_ref);
                    _pack_tmpl($inner, $ai_ref, $args_ref, $result_ref, $be, $le, $iter_base);
                }
            }
            next;
        }

        # Position ops (no arg)
        if ($ch eq 'x') {
            if ($bang) {
                # x!N: pad to N-byte alignment
                my $n = $nrep > 0 ? $nrep : 1;
                my $cur = length($$result_ref);
                my $pad = ($n - ($cur % $n)) % $n;
                $$result_ref .= chr(0) x $pad;
            } else {
                $$result_ref .= chr(0) x $nrep;
            }
            next;
        }
        if ($ch eq 'X') {
            if ($bang) {
                # X!N: truncate back to N-byte alignment
                my $n = $nrep > 0 ? $nrep : 1;
                my $cur = length($$result_ref);
                $$result_ref = substr($$result_ref, 0, int($cur/$n)*$n);
            } else {
                my $fp = length($$result_ref) - $nrep;
                $$result_ref = substr($$result_ref, 0, $fp < 0 ? 0 : $fp);
            }
            next;
        }
        if ($ch eq '@') {
            # @N: relative to group start (out_base). @!N: absolute byte position.
            my $n = defined($count) ? $count : 0;
            my $t = $bang ? $n : $out_base + $n;
            if (length($$result_ref) < $t) { $$result_ref .= chr(0) x ($t - length($$result_ref)) }
            elsif (length($$result_ref) > $t) { $$result_ref = substr($$result_ref, 0, $t) }
            next;
        }
        if ($ch eq '.') {
            # Position format: reads target from arg list.
            # .* = absolute; . or .N = relative to current group's start (out_base).
            my $tgt = ($$ai_ref < $nargs) ? int(($args_ref->[$$ai_ref++] // 0) + 0) : 0;
            my $abs_tgt = $star ? $tgt : $out_base + $tgt;
            my $cur = length($$result_ref);
            if ($cur < $abs_tgt) { $$result_ref .= chr(0) x ($abs_tgt - $cur) }
            elsif ($cur > $abs_tgt) { $$result_ref = substr($$result_ref, 0, $abs_tgt) }
            next;
        }
        if ($ch eq 'p' || $ch eq 'P' || $ch eq 'D') {
            $$ai_ref++ if $$ai_ref < $nargs; next;
        }

        # For multi-arg formats, * means use all remaining args
        $nrep = $nargs - $$ai_ref if $star;

        # Integer types
        my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang);
        if ($nb) {
            my $be2 = $be ? 1 : ($le ? 0 : $dbe);
            for (my $r = 0; $r < $nrep; $r++) {
                my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;
                $$result_ref .= _pack_emit_int(int($v+0), $nb, $sig, $be2);
            }
            next;
        }

        # Float types
        if ($ch eq 'f') {
            my $be2 = $be ? 1 : ($le ? 0 : 0);
            for (my $r = 0; $r < $nrep; $r++) {
                my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;
                $$result_ref .= _pack_float32($v+0.0, $be2);
            }
            next;
        }
        if ($ch eq 'd' || $ch eq 'F') {
            my $be2 = $be ? 1 : ($le ? 0 : 0);
            for (my $r = 0; $r < $nrep; $r++) {
                my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;
                $$result_ref .= _pack_float64($v+0.0, $be2);
            }
            next;
        }

        # Single-arg string types
        if ($ch eq 'a'||$ch eq 'A'||$ch eq 'Z'||$ch eq 'b'||$ch eq 'B'||
            $ch eq 'H'||$ch eq 'h'||$ch eq 'u') {
            my $arg = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // '') : '';
            _pack_str_one($ch, $arg, $nrep, $star, $result_ref);
            next;
        }

        # Per-rep types: U W w
        if ($ch eq 'U') {
            for (my $r = 0; $r < $nrep; $r++) {
                my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;
                _pack_utf8_char(int($v+0), $result_ref);
            }
            next;
        }
        if ($ch eq 'W') {
            for (my $r = 0; $r < $nrep; $r++) {
                my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;
                $$result_ref .= chr(int($v+0));
            }
            next;
        }
        if ($ch eq 'w') {
            for (my $r = 0; $r < $nrep; $r++) {
                my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;
                $v = $v + 0;
                die "Cannot compress negative numbers in pack\n" if $v < 0;
                die "Cannot compress Inf in pack\n" if $v != 0 && $v == $v * 2;
                die "Can only compress unsigned integers in pack\n" if $v != int($v);
                $v = int($v);
                if ($v == 0) { $$result_ref .= chr(0); next }
                my @bytes;
                while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }
                for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }
                $$result_ref .= chr($bytes[-1]);
            }
            next;
        }

        die "Invalid type '$ch' in pack\n";
    }
}

sub _pack_check_brackets {
    my ($tmpl) = @_;
    my ($n_open, $n_close) = (0, 0);
    my $tlen = length($tmpl);
    for (my $i = 0; $i < $tlen; $i++) {
        my $c = substr($tmpl, $i, 1);
        if    ($c eq '[') { $n_open++ }
        elsif ($c eq ']') { $n_close++ }
    }
    die "No group ending character ']' found in template\n" if $n_open > $n_close;
    return unless $n_open > 0;
    # Structural check: brackets must not cross parens
    my @stk = ();
    for (my $i = 0; $i < $tlen; $i++) {
        my $c = substr($tmpl, $i, 1);
        if    ($c eq '[') { push @stk, '[' }
        elsif ($c eq '(') { push @stk, '(' }
        elsif ($c eq ']') {
            die "Mismatched brackets in template\n"
                if !@stk || $stk[-1] ne '[';
            pop @stk;
        }
        elsif ($c eq ')') {
            pop @stk if @stk && $stk[-1] eq '(';
        }
    }
}

sub p_pack {
    my ($tmpl, @args) = @_;
    _pack_check_brackets($tmpl);
    my $result = '';
    my $ai = 0;
    _pack_tmpl($tmpl, \$ai, \@args, \$result, 0, 0);
    return $result;
}

# Decode one UTF-8 sequence from $s at $$si_ref. Returns codepoint, advances si.
sub _unpack_utf8_char {
    my ($s, $si_ref) = @_;
    my $slen = length($s);
    return 0 if $$si_ref >= $slen;
    my $b0 = ord(substr($s, $$si_ref, 1));
    my ($nb, $code);
    if    ($b0 < 0x80) { $nb=1; $code=$b0 }
    elsif ($b0 < 0xE0) { $nb=2; $code=$b0&0x1F }
    elsif ($b0 < 0xF0) { $nb=3; $code=$b0&0x0F }
    else               { $nb=4; $code=$b0&0x07 }
    for (my $k=1; $k<$nb; $k++) {
        $code = ($code<<6)|(ord(substr($s, $$si_ref+$k, 1))&0x3F) if $$si_ref+$k<$slen;
    }
    $$si_ref += $nb;
    return $code;
}

# Unpack string/bit/uu types. $checksum_p: for B/b push individual bits.
sub _unpack_str {
    my ($ch, $nrep, $all, $s, $si_ref, $push_val, $checksum_p) = @_;
    my $slen = length($s);
    if ($ch eq 'A' || $ch eq 'a' || $ch eq 'Z') {
        my $n = $all ? ($slen - $$si_ref) : $nrep;
        $n = 0 if $n < 0;
        my $raw = $$si_ref < $slen ? substr($s, $$si_ref, $n) : '';
        $$si_ref += $n;
        $raw =~ s/[ \x00]+$// if $ch eq 'A';
        $raw =~ s/\x00.*//s   if $ch eq 'Z';
        $push_val->($raw);
    } elsif ($ch eq 'H') {
        my $n = $all ? (2 * ($slen - $$si_ref)) : $nrep;  # n = number of nybbles
        my $hex = '';
        for (my $i = 0; $i < int($n/2); $i++) {
            my $b = $$si_ref+$i < $slen ? ord(substr($s,$$si_ref+$i,1)) : 0;
            $hex .= sprintf('%02x', $b);
        }
        $hex = substr($hex, 0, $n);
        $$si_ref += int(($n+1)/2);
        $push_val->($hex);
    } elsif ($ch eq 'h') {
        my $n = $all ? (2 * ($slen - $$si_ref)) : $nrep;  # n = number of nybbles
        my $hex = '';
        for (my $i = 0; $i < int($n/2); $i++) {
            my $b = $$si_ref+$i < $slen ? ord(substr($s,$$si_ref+$i,1)) : 0;
            $hex .= sprintf('%x%x', $b&0xF, ($b>>4)&0xF);
        }
        $hex = substr($hex, 0, $n);
        $$si_ref += int(($n+1)/2);
        $push_val->($hex);
    } elsif ($ch eq 'B') {
        my $nbits = $all ? (8*($slen-$$si_ref)) : $nrep;
        if ($checksum_p) {
            for (my $i=0; $i<$nbits; $i++) {
                my $b = $$si_ref+int($i/8)<$slen ? ord(substr($s,$$si_ref+int($i/8),1)) : 0;
                $push_val->( ($b>>(7-($i%8))) & 1 );
            }
        } else {
            my $bits = '';
            for (my $i=0; $i<$nbits; $i++) {
                my $b = $$si_ref+int($i/8)<$slen ? ord(substr($s,$$si_ref+int($i/8),1)) : 0;
                $bits .= (($b>>(7-($i%8)))&1) ? '1' : '0';
            }
            $push_val->($bits);
        }
        $$si_ref += int(($nbits+7)/8);
    } elsif ($ch eq 'b') {
        my $nbits = $all ? (8*($slen-$$si_ref)) : $nrep;
        if ($checksum_p) {
            for (my $i=0; $i<$nbits; $i++) {
                my $b = $$si_ref+int($i/8)<$slen ? ord(substr($s,$$si_ref+int($i/8),1)) : 0;
                $push_val->( ($b>>($i%8)) & 1 );
            }
        } else {
            my $bits = '';
            for (my $i=0; $i<$nbits; $i++) {
                my $b = $$si_ref+int($i/8)<$slen ? ord(substr($s,$$si_ref+int($i/8),1)) : 0;
                $bits .= (($b>>($i%8))&1) ? '1' : '0';
            }
            $push_val->($bits);
        }
        $$si_ref += int(($nbits+7)/8);
    } elsif ($ch eq 'u') {
        my $decoded = '';
        while ($$si_ref < $slen) {
            my $lc = ord(substr($s, $$si_ref, 1));
            my $nb = ($lc - 32) & 63; $$si_ref++;
            last if $nb == 0;
            my $ng = int(($nb+2)/3);
            for (my $k=0; $k<$ng; $k++) {
                my $get = sub { my $i=$$si_ref+$_[0]; $i<$slen?(ord(substr($s,$i,1))-32)&63:0 };
                my $cm = ($get->(4*$k)<<18)|($get->(4*$k+1)<<12)|($get->(4*$k+2)<<6)|$get->(4*$k+3);
                $decoded .= chr(($cm>>16)&0xFF) if $k*3   < $nb;
                $decoded .= chr(($cm>> 8)&0xFF) if $k*3+1 < $nb;
                $decoded .= chr( $cm     &0xFF) if $k*3+2 < $nb;
            }
            $$si_ref += $ng*4;
            $$si_ref++ if $$si_ref < $slen && substr($s,$$si_ref,1) eq "\n";
        }
        $push_val->($decoded);
    } elsif ($ch eq 'U') {
        my $n = $all ? 9**9 : $nrep;
        my $done = 0;
        while ($done < $n && $$si_ref < $slen) {
            $push_val->(_unpack_utf8_char($s, $si_ref)); $done++;
        }
    } elsif ($ch eq 'W') {
        my $n = $all ? ($slen-$$si_ref) : $nrep;
        for (my $i=0; $i<$n && $$si_ref<$slen; $i++) {
            $push_val->(ord(substr($s, $$si_ref++, 1)));
        }
    } elsif ($ch eq 'w') {
        my $done = 0;
        while (($all || $done < $nrep) && $$si_ref < $slen) {
            my ($v, $more) = (0, 1);
            while ($more) {
                die "Unterminated compressed integer in unpack\n" if $$si_ref >= $slen;
                my $b = ord(substr($s, $$si_ref++, 1));
                $more = $b & 0x80; $v = ($v<<7)|($b&0x7F);
            }
            $push_val->($v); $done++;
        }
    }
}

# Core unpack loop. Reads from $s using $$si_ref, pushes items via $push_val.
# $group_base: the si value at the start of the current group iteration (for @ relative offsets).
sub _unpack_tmpl {
    my ($tmpl, $s, $si_ref, $push_val, $inh_be, $inh_le, $checksum_p, $group_base) = @_;
    $group_base = 0 unless defined $group_base;
    my $slen = length($s);
    my $ti = 0;
    my $tlen = length($tmpl);
    while (1) {
        $ti = _pack_skip_ws($tmpl, $ti);
        last if $ti >= $tlen;
        my $ch = substr($tmpl, $ti, 1); $ti++;
        my ($grpbeg, $grpend) = (undef, undef);
        if ($ch eq '(') {
            $grpend = _pack_find_group_end($tmpl, $ti);
            $grpbeg = $ti; $ti = $grpend + 1; $ch = '(';
        }
        my ($bang, $be, $le) = _pack_parse_mods($tmpl, \$ti, $inh_be, $inh_le, $ch, 'unpack');
        $ti = _pack_skip_ws($tmpl, $ti);
        my ($all, $count, $nrep) = _pack_parse_count($tmpl, \$ti);
        $ti = _pack_skip_ws($tmpl, $ti);

        # Slash mode: count/data pairs, with support for chained slashes (A /A /A ...)
        if ($ti < $tlen && substr($tmpl, $ti, 1) eq '/') {
            $ti++;
            my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang);
            my $slash_n = 0;
            if ($nb) {
                my $be2 = $be ? 1 : ($le ? 0 : $dbe);
                last if $$si_ref + $nb > $slen;
                $slash_n = _unpack_read_int($s, $$si_ref, $nb, $be2, $sig);
                $$si_ref += $nb;
            } elsif ($ch eq 'w') {
                my $more = 1;
                while ($more) {
                    die "Unterminated compressed integer in unpack\n" if $$si_ref >= $slen;
                    my $b = ord(substr($s, $$si_ref++, 1));
                    $more = $b & 0x80; $slash_n = ($slash_n<<7)|($b&0x7F);
                }
            } else {
                my $n = $all ? ($slen-$$si_ref) : $nrep;
                my $raw = $$si_ref < $slen ? substr($s, $$si_ref, $n) : '';
                $$si_ref += $n;
                $raw =~ s/[ \x00]+$// if $ch eq 'A';
                $raw =~ s/\x00.*//s   if $ch eq 'Z';
                $slash_n = $raw + 0;  # numeric value of the count string
            }
            # Process the data field(s). Loop to support chained slashes: A /A /A ...
            while (1) {
                $ti = _pack_skip_ws($tmpl, $ti); last if $ti >= $tlen;
                my $dch = substr($tmpl, $ti, 1); $ti++;
                my ($dbang, $dbe2, $dle2) = _pack_parse_mods($tmpl, \$ti, $be, $le, $dch, 'unpack');
                $ti = _pack_skip_ws($tmpl, $ti);
                my ($dall, $dcnt, $dnrep) = _pack_parse_count($tmpl, \$ti);
                $ti = _pack_skip_ws($tmpl, $ti);

                # Check for another chained slash: this data field is itself a count
                my $chain = ($ti < $tlen && substr($tmpl, $ti, 1) eq '/');
                if ($chain) { $ti++ }

                my ($dnb, $dsig, $ddbe) = _pack_type_info($dch, $dbang);
                if ($chain) {
                    # This data field is a count for the next slash — read it, don't push
                    if ($dnb) {
                        my $dbe3 = $dbe2 ? 1 : ($dle2 ? 0 : $ddbe);
                        if ($$si_ref + $dnb <= $slen) {
                            $slash_n = _unpack_read_int($s, $$si_ref, $dnb, $dbe3, $dsig);
                            $$si_ref += $dnb;
                        } else { $slash_n = 0 }
                    } elsif ($dch eq 'w') {
                        $slash_n = 0;
                        my $more = 1;
                        while ($more) {
                            last if $$si_ref >= $slen;
                            my $b = ord(substr($s, $$si_ref++, 1));
                            $more = $b & 0x80; $slash_n = ($slash_n<<7)|($b&0x7F);
                        }
                    } else {
                        # String type as intermediate count
                        my $raw2 = $$si_ref < $slen ? substr($s, $$si_ref, $slash_n) : '';
                        $$si_ref += $slash_n;
                        $raw2 =~ s/[ \x00]+$// if $dch eq 'A';
                        $raw2 =~ s/\x00.*//s   if $dch eq 'Z';
                        $slash_n = $raw2 + 0;
                    }
                    # Loop: continue to process the next data field
                } else {
                    # Final data field — read and push
                    if ($dnb) {
                        my $dbe3 = $dbe2 ? 1 : ($dle2 ? 0 : $ddbe);
                        for (my $i=0; $i<$slash_n && $$si_ref+$dnb<=$slen; $i++) {
                            $push_val->(_unpack_read_int($s, $$si_ref, $dnb, $dbe3, $dsig));
                            $$si_ref += $dnb;
                        }
                    } elsif ($dch eq 'A'||$dch eq 'a'||$dch eq 'Z') {
                        _unpack_str($dch, $slash_n, 0, $s, $si_ref, $push_val, $checksum_p);
                    } elsif ($dch eq '(') {
                        my $ge = _pack_find_group_end($tmpl, $ti);
                        my $inner = substr($tmpl, $ti, $ge - $ti); $ti = $ge + 1;
                        for (my $r=0; $r<$slash_n; $r++) {
                            my $iter_base = $$si_ref;
                            _unpack_tmpl($inner, $s, $si_ref, $push_val, $be, $le, $checksum_p, $iter_base);
                        }
                    }
                    last;  # exit the chain loop
                }
            }
            next;
        }

        # Group
        if (defined $grpbeg) {
            my $inner = substr($tmpl, $grpbeg, $grpend - $grpbeg);
            if ($all) {
                while ($$si_ref < $slen) {
                    my $si_before = $$si_ref;
                    my $iter_base = $$si_ref;
                    _unpack_tmpl($inner, $s, $si_ref, $push_val, $be, $le, $checksum_p, $iter_base);
                    last if $$si_ref == $si_before;  # no progress: avoid infinite loop
                }
            } else {
                for (my $r=0; $r<$nrep; $r++) {
                    my $iter_base = $$si_ref;
                    _unpack_tmpl($inner, $s, $si_ref, $push_val, $be, $le, $checksum_p, $iter_base);
                }
            }
            next;
        }

        # Position/skip (no push)
        if ($ch eq 'x') {
            if ($bang) {
                # x!N: advance to N-byte alignment
                my $n = $nrep > 0 ? $nrep : 1;
                $$si_ref += ($n - ($$si_ref % $n)) % $n;
            } elsif ($all) { $$si_ref = $slen }
            else { $$si_ref += $nrep }
            next;
        }
        if ($ch eq 'X') {
            if ($bang) {
                # X!N: back up to N-byte alignment
                my $n = $nrep > 0 ? $nrep : 1;
                $$si_ref = int($$si_ref / $n) * $n;
            } else { $$si_ref -= $nrep; $$si_ref = 0 if $$si_ref < 0 }
            next;
        }
        # @N: relative to group's base position. @!N: absolute byte position.
        if ($ch eq '@') {
            my $n = defined($count) ? $count : 0;
            $$si_ref = $bang ? $n : $group_base + $n;
            next;
        }
        if ($ch eq '%' || $ch eq '!' ) { next }
        if ($ch eq 'p'||$ch eq 'P'||$ch eq 'D') { next }
        if ($ch eq '.') {
            # Position format: pushes current offset, does not advance.
            # .* = absolute pos from string start.
            # . / .1 = relative to innermost group (group_base).
            # .0 = self offset = 0.
            # .N (N>=2) = would need full group stack; approximate with absolute.
            if ($all) {
                $push_val->($$si_ref);
            } elsif (defined($count) && $count == 0) {
                $push_val->(0);
            } elsif (defined($count) && $count >= 2) {
                $push_val->($$si_ref);  # approximate: absolute
            } else {
                $push_val->($$si_ref - $group_base);  # relative to innermost group
            }
            next;
        }

        # Integer types
        my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang);
        if ($nb) {
            my $be2 = $be ? 1 : ($le ? 0 : $dbe);
            my $n = $all ? int(($slen-$$si_ref)/$nb) : $nrep;
            for (my $i=0; $i<$n; $i++) {
                last if $$si_ref + $nb > $slen;
                $push_val->(_unpack_read_int($s, $$si_ref, $nb, $be2, $sig));
                $$si_ref += $nb;
            }
            next;
        }

        # Float types
        if ($ch eq 'f') {
            my $be2 = $be ? 1 : ($le ? 0 : 0);
            my $n = $all ? int(($slen-$$si_ref)/4) : $nrep;
            for (my $i=0; $i<$n; $i++) {
                last if $$si_ref + 4 > $slen;
                $push_val->(_unpack_float32($s, $$si_ref, $be2)); $$si_ref += 4;
            }
            next;
        }
        if ($ch eq 'd'||$ch eq 'F') {
            my $be2 = $be ? 1 : ($le ? 0 : 0);
            my $n = $all ? int(($slen-$$si_ref)/8) : $nrep;
            for (my $i=0; $i<$n; $i++) {
                last if $$si_ref + 8 > $slen;
                $push_val->(_unpack_float64($s, $$si_ref, $be2)); $$si_ref += 8;
            }
            next;
        }

        # String/bit/etc types
        if ($ch eq 'A'||$ch eq 'a'||$ch eq 'Z'||$ch eq 'H'||$ch eq 'h'||
            $ch eq 'B'||$ch eq 'b'||$ch eq 'u'||$ch eq 'U'||$ch eq 'W'||$ch eq 'w') {
            _unpack_str($ch, $nrep, $all, $s, $si_ref, $push_val, $checksum_p);
            next;
        }

        die "Invalid type '$ch' in unpack\n";
    }
}

sub p_unpack {
    my ($tmpl, $s) = @_;
    $s = '' unless defined $s;
    # %N checksum detection
    my $checksum_width = 0;
    if ($tmpl =~ s/^%(\d*)//) {
        $checksum_width = length($1) ? int($1) : 16;
    }
    # U0 UTF-8 byte mode
    my $utf8_mode = ($tmpl =~ s/^U0//);
    _pack_check_brackets($tmpl);
    if ($utf8_mode) {
        my $bytes = '';
        for my $c (split //, $s) {
            my $code = ord($c);
            if    ($code < 0x80)    { $bytes .= chr($code) }
            elsif ($code < 0x800)   { $bytes .= chr(0xC0|($code>>6)) . chr(0x80|($code&0x3F)) }
            elsif ($code < 0x10000) { $bytes .= chr(0xE0|($code>>12)).chr(0x80|(($code>>6)&0x3F)).chr(0x80|($code&0x3F)) }
            else { $bytes .= chr(0xF0|($code>>18)).chr(0x80|(($code>>12)&0x3F)).chr(0x80|(($code>>6)&0x3F)).chr(0x80|($code&0x3F)) }
        }
        $s = $bytes;
    }
    my @result;
    my $checksum = 0;
    my $push_val = $checksum_width
        ? sub { $checksum += $_[0] }
        : sub { push @result, $_[0] };
    my $si = 0;
    _unpack_tmpl($tmpl, $s, \$si, $push_val, 0, 0, $checksum_width ? 1 : 0);
    if ($checksum_width) {
        return $checksum % (2 ** $checksum_width);
    }
    return wantarray ? @result : $result[0];
}

1;
