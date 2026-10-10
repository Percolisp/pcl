# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pcl-shim: must-win -- the real module is XS.  This shim is found BEFORE @INC is
# searched, so a PERL5LIB or -I copy of the real module cannot shadow it
# (task #2462, docs/ir-spec.md 9, docs/shipped-modules.md).
#
# PerlIO::encoding without XS (task #2946, s513i).  The `:encoding(NAME)` layer
# itself is the runtime's (the same codec table Encode uses); what a program
# reaches through this module is that it LOADS (`require PerlIO::encoding` is
# how Encode::Encoding::perlio_ok asks) and the CHECK value the layer uses,
# `$PerlIO::encoding::fallback`, which carries perl's value.

package PerlIO::encoding;
use strict;
use warnings;

our $VERSION = '0.30';

# PERLQQ | WARN_ON_ERR | ONLY_PRAGMA_WARNINGS, as perl sets it.
our $fallback = 0x0100 | 0x0002 | 0x0010;

1;
