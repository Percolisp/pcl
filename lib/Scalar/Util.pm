# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Scalar::Util;
use strict;
use warnings;
use Exporter 'import';

our @EXPORT_OK = qw(
    blessed reftype weaken isweak looks_like_number
    readonly tainted dualvar isdual isvstring openhandle
    set_prototype refaddr unweaken
);

our $VERSION = '1.63';

sub blessed {
    # Must distinguish a *blessed* ref from a plain one: ref() returns the
    # reftype ("ARRAY"/"HASH"/...) for an UNblessed ref, but blessed() must
    # return undef there.  That blessed-vs-not distinction lives in the runtime
    # box flag, exposed via the core builtin (which checks it correctly).
    return builtin::blessed($_[0]);
}

sub reftype {
    # Underlying reference type regardless of blessing — again a runtime-level
    # fact (a blessed arrayref is still "ARRAY"), so delegate to the builtin
    # rather than re-deriving it from ref()/UNIVERSAL::isa (which keys on @ISA).
    return builtin::reftype($_[0]);
}

sub weaken { }
sub isweak { 0 }

# Perl's grok_number: what the CORE numeric conversion would accept without a
# warning.  Three things this must get right that a naive /^\d+$/ does not:
#   * Inf / Infinity / NaN (any case, optional sign) ARE numbers;
#   * the literal "0 but true" is a number (perl's blessed zero);
#   * the digit class is ASCII 0-9 ONLY — \d also matches MONGOLIAN DIGIT FIVE
#     and friends, which perl does NOT accept (Scalar-List-Utils t/lln.t).
# A plain reference is not a number, but an OVERLOADED object answers on its
# stringification (t/lln.t's Math::BigInt rows) — that is what perl does via
# SvAMAGIC before it ever looks at the buffer.
sub looks_like_number {
    my ($val) = @_;
    return 0 unless defined $val;
    if (ref $val) {
        return 0 unless blessed($val) && overload::Overloaded($val);
        $val = "$val";
    }
    return 1 if $val eq '0 but true';
    return 1 if $val =~ /\A\s*[+-]?(?:Inf(?:inity)?|NaN)\s*\z/i;
    return 1 if $val =~ /\A\s*[+-]?(?:[0-9]+\.?[0-9]*|\.[0-9]+)(?:[Ee][+-]?[0-9]+)?\s*\z/;
    return 0;
}

sub readonly { 0 }
sub tainted  { 0 }

sub dualvar {
    my ($num, $str) = @_;
    # A genuine dualvar: numeric value $num, string value $str.  Pure Perl can't
    # construct one, so route to the runtime primitive (p-dualvar) via the
    # builtin:: dispatch namespace, exactly as weaken/blessed/reftype do.
    return builtin::dualvar($num, $str);
}

# Both ask about the SCALAR'S REPRESENTATION, which no plain Perl can inspect —
# so they route to the runtime through the same builtin:: dispatch namespace
# blessed/reftype/dualvar already use.
sub isdual    { return builtin::is_dual($_[0]) }
sub isvstring { return builtin::is_vstring($_[0]) }
sub openhandle { $_[0] }
sub set_prototype { }

# refaddr($ref) — the address of the referent, or undef for a non-ref.
# In Perl numifying a ref yields its address; PCL routes ref-numification
# through the same stable object-identity (object-address) that ref
# stringification uses, so `0 + $r` and the hex of `"$r"` agree, and the id is
# invariant for the referent's lifetime (see object-address in pcl-runtime).
sub refaddr {
    my ($r) = @_;
    return undef unless ref $r;
    return 0 + $r;
}

# Not yet implemented in PCL — exported as a dying stub so `use Scalar::Util
# qw(unweaken)` succeeds (the real Exporter hard-dies on a missing export,
# which kills the whole importing file); only an actual CALL fails.
sub unweaken { die "Scalar::Util::unweaken is not yet implemented in PCL\n" }

1;
