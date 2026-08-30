# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

#
# experimental - PCL shim.
#
# `use experimental 'try';` is the spelling most real code uses to switch on a
# perl feature that is still marked experimental.  It does two things: enable
# the feature, and silence the `experimental::<name>` warning category.
#
# PCL cannot run the REAL experimental.pm.
#
# The ORIGINAL reason is GONE (s457ai, task #817): the module does
#
#     $_ = version->new($_) for values %min_version;
#
# and `for values %h` did not alias in PCL, so the write landed on a copy and
# the next line called ->stringify on "5.34.0".  `values` (and slices) now
# hand out the container's own slots, so that line works.
#
# The shim stands on a SECOND, unrelated blocker, measured by moving this file
# aside and running `use experimental 'try'` (task #840): PCL's feature.pm and
# warnings.pm shims leave %feature::feature, %warnings::Offsets and
# %warnings::NoOp EMPTY, and the real module builds its whole dispatch table
# from those three hashes.  With them empty every pragma falls past the
# "is it a known feature" arms into the version check and croaks
# "Need perl 5.34.0 or later for feature try" — a wrong answer on a perl whose
# $] is 5.040003.
#
# DELETE-WHEN: `%feature::feature` / `%warnings::Offsets` are populated.  At
# that point the real module loads and this file should go; the guard for the
# trigger is in Pl/t/feature-pragma-01.t.
#
# What it deliberately does NOT do: version checking.  The real module croaks
# when a feature is not available in the running perl ("Feature X is not
# supported by Perl 5.x"), and refuses unknown names.  PCL's target is perl
# 5.40 semantics, and its own answer to "is this feature supported" is the
# feature table in Pl/Parser.pm (task #360) — which is consulted at PARSE time,
# where the question actually gets decided.  A name this shim passes through to
# feature->import that feature.pm does not know is that module's error to give.
#
# The warning half is a genuine no-op here rather than an omission: PCL does
# not model default-off warning categories at all (docs/not-supported.md
# "Warnings-gated diagnostics are absent", task #221), so there is no
# `experimental::try` warning for anyone to silence.

package experimental;
use strict;
use warnings;

our $VERSION = '0.032';

sub import {
    my (undef, @features) = @_;
    return if !@features;
    require feature;
    feature->import(@features);
    require warnings;
    warnings->unimport(map { "experimental::$_" } @features);
    return;
}

sub unimport {
    my (undef, @features) = @_;
    return if !@features;
    require feature;
    feature->unimport(@features);
    require warnings;
    warnings->import(map { "experimental::$_" } @features);
    return;
}

1;
