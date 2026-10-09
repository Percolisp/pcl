# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# PCL FACTS OVERLAY for File::Path (task #2878; docs/shipped-modules.md
# "Facts overlays").  Declarations only: PCL reads this beside the real
# module's source; the real module still runs and installs the real subs.
package File::Path;

# File::Path installs these four at BEGIN time in a loop over a name list
# (`*{"_IS_\U$_"} = $^O eq $_ ? sub () { 1 } : sub () { 0 }`), which a
# static parse cannot see.  The declaration supplies the prototype; the
# module supplies the value.
sub _IS_VMS ();
sub _IS_MACOS ();
sub _IS_MSWIN32 ();
sub _IS_OS2 ();

# The same BEGIN block glob-assigns one of two `sub () {...}` constants to
# each of these, chosen by an expression over $^O.
sub _FORCE_WRITABLE ();
sub _NEED_STAT_CHECK ();

1;
