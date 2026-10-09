# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pcl-shim: must-win -- the real module is XS.  This shim is found BEFORE @INC is
# searched, so a PERL5LIB or -I copy of the real module cannot shadow it
# (task #2462, docs/ir-spec.md 9, docs/shipped-modules.md).

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

# Every sub carries the REAL module's prototype (perl 5.40.3 `prototype`; task
# #2870): it decides the parse -- `blessed $o && $o->isa("K")` is blessed($o) && ...,
# not blessed($o && ...) -- in the program and in a string eval alike.
sub blessed ($) {
    # Must distinguish a *blessed* ref from a plain one: ref() returns the
    # reftype ("ARRAY"/"HASH"/...) for an UNblessed ref, but blessed() must
    # return undef there.  That blessed-vs-not distinction lives in the runtime
    # box flag, exposed via the core builtin (which checks it correctly).
    return builtin::blessed($_[0]);
}

sub reftype ($) {
    # Underlying reference type regardless of blessing — again a runtime-level
    # fact (a blessed arrayref is still "ARRAY"), so delegate to the builtin
    # rather than re-deriving it from ref()/UNIVERSAL::isa (which keys on @ISA).
    return builtin::reftype($_[0]);
}

# The WEAK flag (s500a): weaken marks the VARIABLE $_[0] aliases (the caller's
# box), a copy is strong, and a store into it clears the mark -- the runtime
# owns the flag (builtin::weaken / is_weak).  No refcount: the referent's
# lifetime is unchanged (docs/not-supported.md).
sub weaken ($)   { builtin::weaken($_[0]); return }
sub isweak ($)   { return builtin::is_weak($_[0]) ? 1 : !1 }
sub unweaken ($) { builtin::unweaken($_[0]); return }

# Perl's grok_number: what the CORE numeric conversion would accept without a
# warning.  Three things this must get right that a naive /^\d+$/ does not:
#   * Inf / Infinity / NaN (any case, optional sign) ARE numbers;
#   * the literal "0 but true" is a number (perl's blessed zero);
#   * the digit class is ASCII 0-9 ONLY — \d also matches MONGOLIAN DIGIT FIVE
#     and friends, which perl does NOT accept (Scalar-List-Utils t/lln.t).
# A plain reference is not a number, but an OVERLOADED object answers on its
# stringification (t/lln.t's Math::BigInt rows) — that is what perl does via
# SvAMAGIC before it ever looks at the buffer.
sub looks_like_number ($) {
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

sub readonly ($) { 0 }
sub tainted ($)  { 0 }

sub dualvar ($$) {
    my ($num, $str) = @_;
    # A genuine dualvar: numeric value $num, string value $str.  Pure Perl can't
    # construct one, so route to the runtime primitive (p-dualvar) via the
    # builtin:: dispatch namespace, exactly as weaken/blessed/reftype do.
    return builtin::dualvar($num, $str);
}

# Both ask about the SCALAR'S REPRESENTATION, which no plain Perl can inspect —
# so they route to the runtime through the same builtin:: dispatch namespace
# blessed/reftype/dualvar already use.
sub isdual ($)    { return builtin::is_dual($_[0]) }
sub isvstring ($) { return builtin::is_vstring($_[0]) }
# Open-stream test (s500a, #1571): $_[0] when it is an OPEN handle (a glob,
# a glob ref, a lexical handle), else undef -- never a NAME lookup.
sub openhandle ($) { return builtin::openhandle($_[0]) }
# set_prototype(\&code, $proto) (s502e, #2538): Scalar::Util's order is the
# CODE REF first -- the reverse of Sub::Util's -- and it returns the code ref;
# an undef $proto clears the prototype.  The one runtime registrar.
sub set_prototype (&$) {
    my ($code, $proto) = @_;
    # The XS dies on a bad first argument (s502e review, probed vs perl).
    if (!ref $code) { require Carp; Carp::croak("set_prototype: not a reference") }
    if (builtin::reftype($code) ne 'CODE') { require Carp; Carp::croak("set_prototype: not a subroutine reference") }
    return __pcl_set_prototype($code, $proto);
}

# refaddr($ref) — the address of the referent, or undef for a non-ref: the
# number `0 + $r` answers and the hex of `"$r"`, but NEVER through a `0+`
# overload (File::Temp's own NUMIFY handler IS `refaddr($_[0])`), so it goes
# through the builtin:: dispatch namespace, the one address reading the
# runtime shares with numification (task #2682).
sub refaddr ($) {
    return builtin::refaddr($_[0]);
}

1;
