# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# PCL FACTS OVERLAY for Sub::Quote (task #2878; docs/shipped-modules.md
# "Facts overlays").  Declarations only: PCL reads this beside the real
# module's source; the real module still runs and installs the real subs.
package Sub::Quote;

# Sub::Quote's BEGIN block glob-assigns one of two lexical code refs,
# `my $TRUE = sub(){!!1}` or `my $FALSE = sub(){!!0}`, to each of these
# (`*_HAVE_IS_UTF8 = defined &utf8::is_utf8 ? $TRUE : $FALSE`).  A static
# parse cannot see which sub -- or which prototype -- a glob receives from
# a variable.  Both candidates are `()` constants.
sub _HAVE_IS_UTF8 ();
sub _CAN_TRACK_BOOLEANS ();
sub _CAN_TRACK_NUMBERS ();
sub _HAVE_HEX_FLOAT ();

1;
