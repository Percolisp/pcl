# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.
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

sub longmess  { return join('', @_); }
sub shortmess { return join('', @_); }

sub croak   { die join('', @_), "\n"; }
sub confess { die join('', @_), "\n"; }

sub carp  { warn join('', @_), "\n"; }
sub cluck { warn join('', @_), "\n"; }

1;
