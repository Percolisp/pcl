# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pcl-shim: must-win -- the real Carp.pm cannot run under PCL (measured s499j: perl's own
# Carp.pm on PERL5LIB leaves `croak` undefined in the importer, task #2491).  This shim is found BEFORE @INC is
# searched, so a PERL5LIB or -I copy of the real module cannot shadow it
# (task #2462, docs/ir-spec.md 9, docs/shipped-modules.md).

#
# PCL shim for Carp.  The real Carp.pm pulls in machinery (utf8, deep
# caller() introspection) that PCL does not fully model, so we ship a small
# pure-Perl replacement that provides the public API.  PCL does not track
# runtime source file/line (see docs/not-supported.md), so croak/carp do not
# append the " at FILE line N" suffix; the message text is preserved.

package Carp;
use strict;
use warnings;
use Exporter 'import';

our @EXPORT    = qw(carp croak confess);
our @EXPORT_OK = qw(cluck longmess shortmess verbose);

our $VERSION = '1.50';

# A REFERENCE as the first argument is an exception OBJECT and is passed
# through untouched (Carp's longmess_heavy: "don't break references as
# exceptions"), so `croak { code => 1 }` dies with the hash itself (#3867).
sub longmess  { return wantarray ? @_ : $_[0] if ref $_[0]; return join('', @_); }
sub shortmess { return wantarray ? @_ : $_[0] if ref $_[0]; return join('', @_); }

# Internal helper used by some callers (e.g. Method::Generate::Constructor)
# to find the caller depth at which to report an error.  The real Carp walks
# the call stack skipping Carp-internal frames; PCL does not track runtime
# file/line (see docs/not-supported.md), so the precise depth only affects an
# error-message location string.  Return the conventional first non-internal
# frame.
sub short_error_loc { return 1; }
sub long_error_loc  { return 1; }

sub croak   { die @_ if ref $_[0]; die join('', @_), "\n"; }
sub confess { die @_ if ref $_[0]; die join('', @_), "\n"; }

sub carp  { return warn @_ if ref $_[0]; warn join('', @_), "\n"; }
sub cluck { return warn @_ if ref $_[0]; warn join('', @_), "\n"; }

1;
