# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package constant;

# PCL shim for core constant.pm (task #1506).
#
# WHY A SHIM.  `use constant` never loads a module in PCL: the compiler lowers
# it at compile time (Pl/Parser.pm's specially-handled `use` set).  This file
# is reached ONLY by an explicit `require constant` followed by a run-time
# `constant->import(...)` / `import constant LIST` -- Scalar-List-Utils'
# t/dualvar.t does exactly that inside a BEGIN.  Core constant.pm cannot run
# here: its BEGIN installs `_CAN_PCS` & co. by storing a SCALAR REF into its
# own stash (perl's proto-constant-sub rule), which PCL does not model, so
# every `if (_CAN_PCS)` in its import died.
#
# What it does: installs `sub NAME () { VALUE }` into the CALLER's package --
# `NAME => VALUE`, `NAME => LIST` (a list constant), `NAME` alone (the empty
# list) and the hash form `{ A => 1, B => 2 }`.  Name validation is perl's
# basic rule; the 1.21+ refinements (reserved names, warnings) are not here.

use strict;
no warnings;

our $VERSION = '1.33';

sub _install {
    my ($pkg, $name, @list) = @_;
    die "Constant name '$name' is invalid\n"
        if $name !~ /\A_?[^\W_0-9]\w*\z/ && $name !~ /\A__/;
    no strict 'refs';
    my $full = "${pkg}::$name";
    if (@list == 1) {
        my $scalar = $list[0];
        *$full = sub () { $scalar };
    } elsif (@list) {
        *$full = sub () { @list };
    } else {
        *$full = sub () { };
    }
}

sub import {
    my $class = shift;
    return unless @_;
    my $pkg = caller;
    if (ref $_[0] eq 'HASH') {
        my $constants = shift;
        _install($pkg, $_, $constants->{$_}) for keys %$constants;
        return;
    }
    die "Invalid reference type '" . ref($_[0]) . "' not 'HASH'\n" if ref $_[0];
    my $name = shift;
    die "Can't use undef as constant name\n" if !defined $name;
    _install($pkg, $name, @_);
}

1;
