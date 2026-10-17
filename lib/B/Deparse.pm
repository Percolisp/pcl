# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pcl-shim: must-win -- the real module walks perl's op tree through B's XS.
# This file is found BEFORE @INC is searched, so perl's own B/Deparse.pm (which
# would load and then fail at its first op) cannot be reached instead.
#
# B::Deparse is not supported (task #3281, s518e): it turns a compiled sub back
# into Perl source by walking perl's OP TREE, and PCL compiles Perl to Lisp --
# there is no op tree to walk, and no copy of the source is kept per sub.
# `require B::Deparse` therefore DIES here, perl-shaped and trappable, so a
# caller that tries it inside an eval (Data::Dumper's $Deparse, Storable's
# $Deparse, Type::Tiny's error explanations) takes its own fallback.  See
# docs/not-supported.md "B".

package B::Deparse;

die "B::Deparse is not supported by PCL (it decompiles perl's op tree, which PCL does not have; see docs/not-supported.md \"B\")\n";
