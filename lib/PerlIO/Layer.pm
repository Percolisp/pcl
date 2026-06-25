# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

# Minimal PerlIO::Layer shim.
#
# In real perl, PerlIO::Layer is loaded as part of the PerlIO core and exposes
# `PerlIO::Layer->find($name)`, which reports whether a named I/O layer is
# known (returning a layer object, i.e. a true value, or undef).  Code uses it
# to gate layer-dependent behaviour, e.g.:
#
#     skip(...) unless find PerlIO::Layer 'perlio';
#
# PCL does not model PerlIO layers as first-class objects — it builds on SBCL
# streams and treats binmode/layer pushes as (near) no-ops — so there is nothing
# to introspect.  We report the standard core layer names as "known" (a true
# value) so that layer-conditional code takes the normal "available" path rather
# than skipping or dying; any other name returns undef.  PCL loads this shim
# automatically on first `PerlIO::Layer->...` dispatch (the method-call
# auto-require), matching perl's "always preloaded" behaviour.
package PerlIO::Layer;

use strict;
use warnings;

my %known = map { $_ => 1 } qw(
    perlio unix stdio crlf raw bytes utf8 scalar mmap flock creat
    pop encoding pending
);

sub find {
    my ($class, $name) = @_;
    return undef unless defined $name;
    return $known{$name} ? 1 : undef;
}

1;
