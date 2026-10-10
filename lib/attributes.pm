# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pcl-shim: must-win -- the real module needs its XS half.  This shim is found
# BEFORE @INC is searched, so a PERL5LIB or -I copy of the real module cannot
# shadow it (task #2462, docs/ir-spec.md 9, docs/shipped-modules.md).
#
# attributes without XS (task #3162, batch s514e).  perl's attributes.pm is
# plain Perl over four XS primitives -- _modify_attrs, _fetch_attrs,
# _guess_stash, reftype -- and PCL transpiled it, then died at the first of
# them ("undefined function attributes::_guess_stash").  This file is perl's
# Perl half unchanged in behaviour, with the four primitives written over the
# runtime's ONE sub-attribute record (builtin::sub_attributes /
# builtin::set_sub_attribute, beside the prototype registry: docs/ir-spec.md
# "Sub attributes").
#
# What is real: get() of a sub's built-in attributes (lvalue, method, const --
# never its prototype), `use attributes PKG, \&f, LIST` setting them,
# `prototype(...)` setting the prototype through the same registry
# prototype() reads, MODIFY_/FETCH_CODE_ATTRIBUTES dispatch, "Invalid CODE
# attribute" for a name nothing accepts, reftype.
#
# What ANNOUNCES (docs/not-supported.md "attributes: lvalue set at run
# time"): setting `lvalue` (or `-lvalue`) at RUN time records it for get(),
# but PCL decides lvalue-ness when it COMPILES the sub, so the sub's
# behaviour does not change.  _guess_stash knows the package only of a sub that
# carries an attribute (undef otherwise, so get() asks the CALLER's package,
# where perl asks the package the sub was compiled in).

package attributes;

our $VERSION = 0.36;

@attributes::EXPORT_OK = qw(get reftype);
@attributes::EXPORT = ();
%attributes::EXPORT_TAGS = (ALL => [qw(get reftype)]);

use strict;

sub croak {
    require Carp;
    goto &Carp::croak;
}

sub carp {
    require Carp;
    goto &Carp::carp;
}

my %msg = (
    lvalue => 'lvalue attribute applied to already-defined subroutine',
   -lvalue => 'lvalue attribute removed from already-defined subroutine',
    const  => 'Useless use of attribute "const"',
);

# --- the four primitives perl implements in XS -------------------------------

sub reftype ($) { builtin::reftype($_[0]) }

sub _guess_stash { builtin::sub_stash($_[0]) }

sub _fetch_attrs {
    my ($ref) = @_;
    return () if (builtin::reftype($ref) // '') ne 'CODE';
    return builtin::sub_attributes($ref);
}

# Apply LIST to the referent; return the attributes NOT applied (perl's
# contract: the caller hands those to MODIFY_*_ATTRIBUTES or dies).
sub _modify_attrs {
    my ($ref, @attrs) = @_;
    my $type = builtin::reftype($ref) // '';
    my @bad;
    for my $a (@attrs) {
        if ($type eq 'CODE') {
            if ($a =~ /\Aprototype\((.*)\)\z/s) {
                __pcl_set_prototype($ref, $1);
            }
            elsif (builtin::set_sub_attribute($ref, $a)) {
                if ($a =~ /\A-?lvalue\z/) {
                    print STDERR "PCL: attribute `$a' set at run time is recorded "
                        . "for attributes::get, but the sub's lvalue-ness was decided "
                        . "when it was compiled (see docs/not-supported.md)\n";
                }
            }
            else {
                push @bad, $a;
            }
        }
        elsif ($a ne 'shared') {
            push @bad, $a;
        }
    }
    return @bad;
}

# --- perl's attributes.pm, unchanged in behaviour ----------------------------

sub _modify_attrs_and_deprecate {
    my $svtype = shift;
    grep {
        $svtype eq 'CODE' && exists $msg{$_} ? do {
            require warnings;
            warnings::warnif('misc', $msg{$_});
            0;
        } : 1
    } _modify_attrs(@_);
}

sub import {
    @_ > 2 && ref $_[2] or do {
        require Exporter;
        goto &Exporter::import;
    };
    my (undef,$home_stash,$svref,@attrs) = @_;

    my $svtype = uc reftype($svref);
    my $pkgmeth;
    $pkgmeth = UNIVERSAL::can($home_stash, "MODIFY_${svtype}_ATTRIBUTES")
        if defined $home_stash && $home_stash ne '';
    my @badattrs;
    if ($pkgmeth) {
        my @pkgattrs = _modify_attrs_and_deprecate($svtype, $svref, @attrs);
        @badattrs = $pkgmeth->($home_stash, $svref, @pkgattrs);
    }
    else {
        @badattrs = _modify_attrs_and_deprecate($svtype, $svref, @attrs);
    }
    if (@badattrs) {
        croak "Invalid $svtype attribute" .
            (( @badattrs == 1 ) ? '' : 's') .
            ": " .
            join(' : ', @badattrs);
    }
}

sub get ($) {
    @_ == 1  && ref $_[0] or
        croak 'Usage: '.__PACKAGE__.'::get $ref';
    my $svref = shift;
    my $svtype = uc reftype($svref);
    my $stash = _guess_stash($svref);
    $stash = caller unless defined $stash;
    my $pkgmeth;
    $pkgmeth = UNIVERSAL::can($stash, "FETCH_${svtype}_ATTRIBUTES")
        if defined $stash && $stash ne '';
    return $pkgmeth ?
                (_fetch_attrs($svref), $pkgmeth->($stash, $svref)) :
                (_fetch_attrs($svref))
        ;
}

sub require_version { goto &UNIVERSAL::VERSION }

1;
