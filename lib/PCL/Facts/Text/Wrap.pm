# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# PCL FACTS OVERLAY for Text::Wrap (task #2878; docs/shipped-modules.md
# "Facts overlays").  Declarations only: PCL reads this beside the real
# module's source; the real module still runs and installs the real subs.
package Text::Wrap;

# Text::Wrap defines this constant with a STRING eval in a BEGIN block
# (`eval sprintf 'sub REGEXPS_USE_BYTES () { %d }', ...`), whose text a
# static parse does not compile.
sub REGEXPS_USE_BYTES ();

1;
