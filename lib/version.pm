# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package version;
use strict;
use warnings;

use overload
    '""'  => \&stringify,
    '0+'  => \&numify,
    'cmp' => \&vcmp,
    '<=>' => \&vcmp,
    fallback => 1;

our $VERSION = '0.9933';

# The real version.pm declares its own warning category as it loads (`use
# warnings::register` plus an explicit call).  That registration is the whole
# difference between perl's 81 `%warnings::Offsets` keys and PCL's 80 after
# `require experimental`, which loads this module — task #875.
warnings::register_categories(qw/version/);

sub new {
    my ($class, $val) = @_;
    $val //= '';
    # Store as string
    my $str = "$val";
    return bless { string => $str, original => $val }, $class;
}

# Shorthand: version->parse same as version->new
sub parse { shift->new(@_) }

sub stringify { $_[0]->{string} }

# ---------------------------------------------------------------------------
# _parse — perl's version::scan_version in miniature (task #870).
#
# A version string is NOT a number and NOT a string: it is a tuple of integer
# components, and every one of numify / normal / <=> / cmp reads that tuple.
# Two spellings reach it:
#
#   DOTTED-DECIMAL ("qv"): a leading `v`, OR two or more dots.  Every
#     dot-separated part is one component, and the tuple is padded to at
#     least three (perl pads at scan time, which is why `v1.2`->numify is
#     1.002000 and not 1.002).
#
#   DECIMAL: the integer part is the first component and the FRACTION is cut
#     into groups of three digits, the last group right-padded with zeros —
#     so 1.2 is v1.200.0 and 1.02 is v1.20.0, which is exactly why a string
#     compare of two version strings is wrong ("5.030000" lt "5.14.0" while
#     v5.30.0 gt v5.14.0).  The tuple is NOT padded here: `5.005`->numify is
#     "5.005", two components.
#
# An `_` (an alpha version) is simply REMOVED on both paths, and that one
# rule reproduces perl on every measured case — because the two paths then
# re-chunk differently, which is the whole difference: a decimal fraction is
# re-cut into threes (5.005_03 -> v5.5.30, 1.23_01 -> v1.230.100) while a
# dotted component just gets longer (v1.2_3 -> v1.23.0, 1.2.3_4 -> v1.2.34).
# Turning it into a component separator in the dotted form is the plausible
# reading and it is WRONG — measured against real perl, 24 rows apart.
sub _parse {
    my $s = shift;
    $s = '' unless defined $s;
    $s = "$s";
    $s =~ s/\A\s+//;
    $s =~ s/\s+\z//;
    my $alpha = $s =~ /_/ ? 1 : 0;
    my $qv = 0;
    $qv = 1 if $s =~ s/\Av//;
    my $dots = ($s =~ tr/.//);
    $qv = 1 if $dots > 1;
    my @parts;
    if ($qv) {
        $s =~ s/_//g;
        for my $p (split /\./, $s, -1) {
            $p =~ s/\D//g;
            push @parts, length($p) ? 0 + $p : 0;
        }
        @parts = (0) if !@parts;
        push @parts, 0 while @parts < 3;
    }
    else {
        my ($int, $frac) = split /\./, $s, 2;
        $int = '' if !defined $int;
        $frac = '' if !defined $frac;
        $int =~ s/\D//g;
        $frac =~ s/\D//g;
        @parts = (length($int) ? 0 + $int : 0);
        while (length $frac) {
            my $g = substr($frac, 0, 3);
            $frac = substr($frac, length $g);
            $g .= '0' while length($g) < 3;
            push @parts, 0 + $g;
        }
    }
    return (\@parts, $qv, $alpha);
}

sub _parts { my @r = _parse($_[0]); return $r[0] }

sub numify {
    my $self = shift;
    my $p = _parts($self->{string});
    my $str = sprintf('%d.', $p->[0]);
    if (@$p > 1) {
        for my $i (1 .. $#$p) { $str .= sprintf('%03d', $p->[$i]) }
    }
    else { $str .= '000' }
    return $str;
}

sub normal {
    my $self = shift;
    my @c = @{ _parts($self->{string}) };
    push @c, 0 while @c < 3;
    return 'v' . join('.', @c);
}

# Componentwise, missing components 0 — never a string compare (#870).
sub vcmp {
    my ($l, $r, $swap) = @_;
    my $lp = _parts("$l");
    my $rp = _parts(ref($r) ? "$r" : $r);
    my $n = @$lp > @$rp ? scalar(@$lp) : scalar(@$rp);
    my $res = 0;
    for my $i (0 .. $n - 1) {
        my $a = $i < @$lp ? $lp->[$i] : 0;
        my $b = $i < @$rp ? $rp->[$i] : 0;
        if ($a != $b) { $res = $a <=> $b; last }
    }
    # Negate rather than swapping the two tuples: `($x,$y) = ($y,$x)` on two
    # REFERENCES is silently wrong under PCL today (task #891) and this is
    # also perl's own version.pm idiom.
    return $swap ? -$res : $res;
}

sub is_alpha { my @r = _parse($_[0]->{string}); return $r[2] }
sub is_qv    { my @r = _parse($_[0]->{string}); return $r[1] }

# ---------------------------------------------------------------------------
# is_strict / is_lax — the two version-string acceptance predicates.  Perl
# ships them in version/regex.pm, which builds each pattern by interpolating
# qr// objects into other qr// objects; the patterns below are those same
# grammars expanded into one literal each, so there is no qr-in-qr to compile.
#
#   STRICT = STRICT_DECIMAL | STRICT_DOTTED_DECIMAL
#     STRICT_DECIMAL        = (0|[1-9][0-9]*) (\.[0-9]+)?      no leading zeros
#     STRICT_DOTTED_DECIMAL = v (0|[1-9][0-9]*) (\.[0-9]{1,3}){2,}
#                                          leading v AND at least three parts
#
#   LAX = undef | LAX_DOTTED_DECIMAL | LAX_DECIMAL
#     LAX_DOTTED_DECIMAL = v [0-9]+ ((\.[0-9]+)+ (_[0-9]+)?)?
#                        | [0-9]*   (\.[0-9]+){2,} (_[0-9]+)?
#     LAX_DECIMAL        = [0-9]+ ((\.[0-9]+)|\.)? (_[0-9]+)?
#                        | (\.[0-9]+) (_[0-9]+)?
#
# VERIFIED against the real `version::` (perl 5.40.3) over all 40 strings
# t/op/packagev.t feeds these two, plus 12 more (empty string, 1.2.3.4.5,
# v1.2.3_4, 1..2, 0.0.0, v1.02_03_04, 1.0_0, v9.9.9.9.9, .5, 5., 007,
# v1.234.5): every answer agrees, both predicates.
our $STRICT = qr/\A(?:(?:0|[1-9][0-9]*)(?:\.[0-9]+)?|v(?:0|[1-9][0-9]*)(?:\.[0-9]{1,3}){2,})\z/;
our $LAX    = qr/\A(?:undef|v[0-9]+(?:(?:\.[0-9]+)+(?:_[0-9]+)?)?|(?:[0-9]+)?(?:\.[0-9]+){2,}(?:_[0-9]+)?|[0-9]+(?:(?:\.[0-9]+)|\.)?(?:_[0-9]+)?|(?:\.[0-9]+)(?:_[0-9]+)?)\z/;

sub is_strict { defined $_[0] && $_[0] =~ $STRICT ? 1 : 0 }
sub is_lax    { defined $_[0] && $_[0] =~ $LAX    ? 1 : 0 }

1;
