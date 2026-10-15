# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# PCL FACTS OVERLAY for IO::Compress::Base::Common (task #3205, s515a;
# docs/shipped-modules.md "Facts overlays").  Declarations only: PCL reads this
# beside the real module's source; the real module still runs and installs the
# real subs.
package IO::Compress::Base::Common;

# The prototype scan skips every IO:: module for cost, so a caller's
# `use IO::Compress::Base::Common;` (Compress::Zlib, IO::Uncompress::Base, ...)
# never learned these prototypes: `isaFilename "x", "y"` passed both arguments
# where perl passes one (probed 5.40.3).  The prototypes are the module's own.
sub hasEncode ();
sub getEncoding ($$$);
sub setBinModeInput ($);
sub setBinModeOutput ($);
sub isaFilehandle ($);
sub isaFilename ($);
sub whatIsInput ($;$);
sub whatIsOutput ($;$);
# NOT whatIs ($;$): it is not exported, and the module's OWN unit reads this
# overlay at its package statement -- so the prototype applied to
# `whatIs(@_)` inside whatIsOutput, ABOVE whatIs's definition, where perl
# applies none: @_ became its COUNT and every output buffer `\$buf` was
# taken for a FILENAME "SCALAR(0x1)" (#3341, s517b).

# `use constant` in the module: nullary, exported (STATUS_* also through :Status).
sub STATUS_OK ();
sub STATUS_ENDSTREAM ();
sub STATUS_EOF ();
sub STATUS_ERROR ();
sub MAX16 ();
sub MAX32 ();
sub MAX32cmp ();

1;
