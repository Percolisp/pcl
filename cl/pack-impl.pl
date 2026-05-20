## Copyright (c) 2025-2026
## This is free software; you can redistribute it and/or modify it
## under the same terms as the Perl 5 programming language system itself.

use strict;
use warnings;

# REBUILD PROCEDURE: after running ./pl2cl < cl/pack-impl.pl to regenerate cl/pcl-pack.lisp:
#   1. Keep the manually-maintained header in cl/pcl-pack.lisp (lines up to and including
#      the @INC setup). The header begins with (in-package :pcl) and must stay in :pcl.
#   2. Remove the generated (p-defpackage :main) and (in-package :main) lines — pack-impl.pl
#      has no `package` declaration, so PCL defaults to :main. Those two lines must be deleted
#      so all functions stay in the :pcl package (matching pcl-pack.lisp.bak behavior).
#      Switching to :main causes "MAIN also shadows" SBCL warnings in subsequent test code.
#   3. Ensure the p-pack / p-unpack wrapper defuns are appended at the end (they call
#      pl-p_pack / pl-p_unpack, the transpiled names of p_pack / p_unpack below).

my $CAN_ENDIAN = 'sSiIlLqQjJfFdDpP';
my $CAN_SHRIEK = 'sSiIlLnNvVxX.@';
my $MAX_GROUP_DEPTH = 100;

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

our $pcl_pack_comma_warned = 0;  # reset at start of each p_pack call

sub _pack_skip_ws {
    my ($s, $ti) = @_;
    my $tlen = length($s);
    while ($ti < $tlen) {
        my $ch = substr($s, $ti, 1);
        if ($ch eq ' ' || $ch eq "\t" || $ch eq "\n" || $ch eq "\r" || $ch eq "\f") {
            $ti++;
        } elsif ($ch eq ',') {
            # Perl warns on commas (once per pack call) but treats as separator
            unless ($pcl_pack_comma_warned) {
                warn "Invalid type ',' in pack\n";
                $pcl_pack_comma_warned = 1;
            }
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
    my ($got_be, $got_le, $got_bang) = (0, 0, 0);
    while ($$ti_ref < $tlen) {
        my $m = substr($tmpl, $$ti_ref, 1);
        if ($m eq '!') {
            die "'!' allowed only after types $CAN_SHRIEK in $ctx\n"
                unless index($CAN_SHRIEK, $ch) >= 0;
            warn "Duplicate modifier '!' after '$ch' in $ctx\n" if $got_bang;
            $bang = 1; $got_bang = 1; $$ti_ref++;
        } elsif ($m eq '>') {
            die "'>' allowed only after types $CAN_ENDIAN in $ctx\n"
                unless index($CAN_ENDIAN, $ch) >= 0 || $ch eq '(';
            die "Can't use both '<' and '>' after type '$ch' in $ctx\n" if $got_le;
            die "Can't use '>' in a group with different byte-order in $ctx\n" if $inh_le;
            warn "Duplicate modifier '>' after '$ch' in $ctx\n" if $got_be;
            $be = 1; $le = 0; $got_be = 1; $$ti_ref++;
        } elsif ($m eq '<') {
            die "'<' allowed only after types $CAN_ENDIAN in $ctx\n"
                unless index($CAN_ENDIAN, $ch) >= 0 || $ch eq '(';
            die "Can't use both '<' and '>' after type '$ch' in $ctx\n" if $got_be;
            die "Can't use '<' in a group with different byte-order in $ctx\n" if $inh_be;
            warn "Duplicate modifier '<' after '$ch' in $ctx\n" if $got_le;
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
        # No ws skip here: space between type+mods and count is invalid in Perl.
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
    # NOTE: do NOT skip whitespace here — space between type and count is invalid in Perl.
    # The caller (_pack_tmpl, _unpack_tmpl) skips whitespace BEFORE the type, not between type and count.
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
        die "Within \[\]-length '\@' not allowed\n" if index($inner, '@') >= 0;
        die "Malformed integer in \[\]\n" if $inner =~ /^\d/ && $inner !~ /^\d+$/;
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
        # In Perl, 2**64 is a float and loses precision, but pack-impl.pl is transpiled to CL
        # where (expt 2 64) is exact. The general formula works for all sizes in CL.
        my $max = 2 ** ($nbytes * 8);
        $v -= $max if $v >= $max / 2;
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
        my $line_len = 45;
        if (!$star && $nrep > 45) {
            if ($nrep > 63) {
                warn "Field too wide in 'u' format in pack";
                $line_len = 63;
            } else {
                $line_len = $nrep;
            }
        }
        for (my $cs = 0; $cs < $slen; $cs += $line_len) {
            my $ce = $cs + $line_len < $slen ? $cs + $line_len : $slen;
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
# $depth: nesting depth for ()-groups (dies if > $MAX_GROUP_DEPTH).
sub _pack_tmpl {
    my ($tmpl, $ai_ref, $args_ref, $result_ref, $inh_be, $inh_le, $out_base, $depth) = @_;
    $out_base = 0 unless defined $out_base;
    $depth = 0 unless defined $depth;
    die "Too deeply nested \(\)-groups in pack\n" if $depth > $MAX_GROUP_DEPTH;
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
        # No ws skip here: space between type+mods and count is invalid in Perl.
        my $ti_before_count = $ti;
        my ($star, $count, $nrep) = _pack_parse_count($tmpl, \$ti);
        my $had_count = ($star || $ti > $ti_before_count);
        $ti = _pack_skip_ws($tmpl, $ti);

        # Slash — count prefix: ch encodes length, next char is data format
        if ($ti < $tlen && substr($tmpl, $ti, 1) eq '/') {
            $ti++;
            $ti = _pack_skip_ws($tmpl, $ti); last if $ti >= $tlen;
            # '/' must not have a count applied directly to it (e.g. c/*a or c/1a are invalid;
            # Z*/A* is valid because * is the count for Z, not for /).
            { my $c = substr($tmpl, $ti, 1);
              die "'/' does not take a repeat count in pack\n"
                if $c eq '*' || $c eq '[' || $c =~ /\d/; }
            my $darg = ($$ai_ref < $nargs) ? $args_ref->[$$ai_ref++] : '';
            $darg = '' unless defined $darg;
            my $dlen = length($darg);
            my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang);
            if ($nb) {
                $$result_ref .= _pack_emit_int($dlen, $nb, $sig, $be ? 1 : ($le ? 0 : $dbe));
            } elsif ($ch eq 'A' || $ch eq 'a') {
                _pack_str_one($ch, "$dlen", 1, 0, $result_ref);
            } elsif ($ch eq 'Z') {
                # Z*/A*: write count as decimal string followed by null byte
                _pack_str_one('Z', "$dlen", length("$dlen") + 1, 0, $result_ref);
            } elsif ($ch eq 'w') {
                # w/A*: write count as BER-encoded integer
                my $v = $dlen;
                if ($v == 0) { $$result_ref .= chr(0); }
                else {
                    my @bytes;
                    while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }
                    for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }
                    $$result_ref .= chr($bytes[-1]);
                }
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
            # Check: group must not start with a count
            my $gti = _pack_skip_ws($inner, 0);
            if ($gti < length($inner)) {
                my $fc = substr($inner, $gti, 1);
                die "\(\)-group starts with a count in pack\n" if $fc =~ /^[\d\*\[]/;
            }
            if ($star) {
                while ($$ai_ref < $nargs) {
                    my $ai_before = $$ai_ref;
                    my $iter_base = length($$result_ref);
                    _pack_tmpl($inner, $ai_ref, $args_ref, $result_ref, $be, $le, $iter_base, $depth + 1);
                    last if $$ai_ref == $ai_before;  # no progress: avoid infinite loop
                }
            } else {
                for (my $r = 0; $r < $nrep; $r++) {
                    my $iter_base = length($$result_ref);
                    _pack_tmpl($inner, $ai_ref, $args_ref, $result_ref, $be, $le, $iter_base, $depth + 1);
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
            die "Invalid type '$ch' in pack\n";
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
                $$result_ref .= _pack_float32($v, $be2);
            }
            next;
        }
        if ($ch eq 'd' || $ch eq 'F') {
            my $be2 = $be ? 1 : ($le ? 0 : 0);
            for (my $r = 0; $r < $nrep; $r++) {
                my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;
                $$result_ref .= _pack_float64($v, $be2);
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
                my $raw = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;
                my $orig_s = "$raw";  # stringify BEFORE numeric coercion
                my $v = $raw + 0;
                die "Cannot compress negative numbers in pack\n" if $v < 0;
                die "Cannot compress Inf in pack\n" if $v != 0 && $v == $v * 2;
                die "Can only compress unsigned integers in pack\n" if $v != int($v);
                # Perl also rejects float-notation strings (e.g. "1e21") when value > UV_MAX.
                # Pure-digit strings (even > UV_MAX) succeed; so does any non-string source.
                # In CL: pure-digit strings → exact bignum; e/E strings → double-float.
                # The $v != int($v) check cannot distinguish a large integer-valued float from
                # an exact bignum, so check the original string form for scientific notation.
                die "Can only compress unsigned integers in pack\n"
                    if $orig_s =~ /[eE]/ && $v >= 2**64;
                $v = int($v);
                if ($v == 0) { $$result_ref .= chr(0); next }
                my @bytes;
                while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }
                for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }
                $$result_ref .= chr($bytes[-1]);
            }
            next;
        }

        die "Invalid type '/' in pack\n" if $ch eq '/';
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
    local $pcl_pack_comma_warned = 0;  # reset comma warning flag for this call
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
# $depth: nesting depth for ()-groups.
sub _unpack_tmpl {
    my ($tmpl, $s, $si_ref, $push_val, $inh_be, $inh_le, $checksum_p, $group_base, $depth) = @_;
    $group_base = 0 unless defined $group_base;
    $depth = 0 unless defined $depth;
    die "Too deeply nested \(\)-groups in unpack\n" if $depth > $MAX_GROUP_DEPTH;
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
        # No ws skip here: space between type+mods and count is invalid in Perl.
        my $ti_before_count = $ti;
        my ($all, $count, $nrep) = _pack_parse_count($tmpl, \$ti);
        my $had_count = ($all || $ti > $ti_before_count);
        $ti = _pack_skip_ws($tmpl, $ti);

        # Slash mode: count/data pairs, with support for chained slashes (A /A /A ...)
        if ($ti < $tlen && substr($tmpl, $ti, 1) eq '/') {
            $ti++;
            $ti = _pack_skip_ws($tmpl, $ti);
            die "Code missing after '/' in unpack\n" if $ti >= $tlen;
            # '/' must not have a count applied directly to it (e.g. c/*a or c/1a are invalid;
            # Z*/A* is valid because * is the count for Z, not for /).
            { my $c = substr($tmpl, $ti, 1);
              die "'/' does not take a repeat count in unpack\n"
                if $c eq '*' || $c eq '[' || $c =~ /\d/; }
            my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang);
            my $slash_n = 0;
            if ($nb) {
                my $be2 = $be ? 1 : ($le ? 0 : $dbe);
                if ($$si_ref + $nb > $slen) {
                    last unless $depth > 0;
                    die "length/code after end of string in unpack\n";
                }
                $slash_n = _unpack_read_int($s, $$si_ref, $nb, $be2, $sig);
                $$si_ref += $nb;
            } elsif ($ch eq 'w') {
                my $more = 1;
                while ($more) {
                    die "Unterminated compressed integer in unpack\n" if $$si_ref >= $slen;
                    my $b = ord(substr($s, $$si_ref++, 1));
                    $more = $b & 0x80; $slash_n = ($slash_n<<7)|($b&0x7F);
                }
            } elsif ($ch eq 'Z') {
                # Z*/...: read null-terminated decimal count string
                my $end = index($s, "\0", $$si_ref);
                if ($end < 0) { $end = $slen; }  # no null → read to end
                my $raw = substr($s, $$si_ref, $end - $$si_ref);
                $$si_ref = $end + 1;  # skip past the null byte
                $$si_ref = $slen if $$si_ref > $slen;
                $slash_n = $raw + 0;
            } else {
                my $n = $all ? ($slen-$$si_ref) : $nrep;
                my $raw = $$si_ref < $slen ? substr($s, $$si_ref, $n) : '';
                $$si_ref += $n;
                $raw =~ s/[ \x00]+$// if $ch eq 'A';
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
                        die "length/code after end of string in unpack\n"
                            if $$si_ref + $dnb > $slen;
                        $slash_n = _unpack_read_int($s, $$si_ref, $dnb, $dbe3, $dsig);
                        $$si_ref += $dnb;
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
                    } elsif ($dch eq 'A'||$dch eq 'a'||$dch eq 'Z'
                             ||$dch eq 'B'||$dch eq 'b'||$dch eq 'H'||$dch eq 'h'
                             ||$dch eq 'u'||$dch eq 'U') {
                        _unpack_str($dch, $slash_n, 0, $s, $si_ref, $push_val, $checksum_p);
                    } elsif ($dch eq '(') {
                        my $ge = _pack_find_group_end($tmpl, $ti);
                        my $inner = substr($tmpl, $ti, $ge - $ti); $ti = $ge + 1;
                        for (my $r=0; $r<$slash_n; $r++) {
                            my $iter_base = $$si_ref;
                            _unpack_tmpl($inner, $s, $si_ref, $push_val, $be, $le, $checksum_p, $iter_base, $depth + 1);
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
            my $gti = _pack_skip_ws($inner, 0);
            if ($gti < length($inner)) {
                my $fc = substr($inner, $gti, 1);
                die "\(\)-group starts with a count in unpack\n" if $fc =~ /^[\d\*\[]/;
            }
            if ($all) {
                while ($$si_ref < $slen) {
                    my $si_before = $$si_ref;
                    my $iter_base = $$si_ref;
                    _unpack_tmpl($inner, $s, $si_ref, $push_val, $be, $le, $checksum_p, $iter_base, $depth + 1);
                    last if $$si_ref == $si_before;  # no progress: avoid infinite loop
                }
            } else {
                for (my $r=0; $r<$nrep; $r++) {
                    my $iter_base = $$si_ref;
                    _unpack_tmpl($inner, $s, $si_ref, $push_val, $be, $le, $checksum_p, $iter_base, $depth + 1);
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
        if ($ch eq 'p'||$ch eq 'P'||$ch eq 'D') { die "Invalid type '$ch' in unpack\n" }
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

        die "'/' must follow a numeric type in unpack\n" if $ch eq '/';
        die "Invalid type '$ch' in unpack\n";
    }
}

sub p_unpack {
    my ($tmpl, $s) = @_;
    $s = '' unless defined $s;
    # Strip leading whitespace and #-comments before prefix detection
    $tmpl =~ s/\A(?:[ \t\n\r\f,]|#[^\n]*\n?)*//;
    # %N checksum detection
    my $checksum_width = 0;
    if ($tmpl =~ s/^%(\d*)//) {
        $checksum_width = length($1) ? int($1) : 16;
    }
    # U0 UTF-8 byte mode
    my $utf8_mode = ($tmpl =~ s/^U0//);
    # Strip leading whitespace again in case %32 was followed by spaces
    $tmpl =~ s/\A(?:[ \t\n\r\f,]|#[^\n]*\n?)*// if $checksum_width;
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
        # Use floor-division modulo: works for both negative integers and float checksums.
        # Perl's % truncates to int first (wrong for floats); CL's p-% does (mod trunc trunc).
        # Formula: r = checksum - floor(checksum/mod)*mod, avoiding POSIX::floor.
        my $mod = 2 ** $checksum_width;
        my $q = int($checksum / $mod);
        $q-- if $q * $mod > $checksum;
        return $checksum - $q * $mod;
    }
    return wantarray ? @result : $result[0];
}

1;
