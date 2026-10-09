# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# PCL FACTS OVERLAY for IO::Socket::UNIX (task #2878; docs/shipped-modules.md
# "Facts overlays").  Declarations only: PCL reads this beside the real
# module's source; the real module still runs and installs the real subs.
package IO::Socket::UNIX;

# IO::Socket::UNIX gets these from `use IO::Socket;`, whose own import is
# CODE (`Exporter::export 'Socket', $callpkg, @_`): it re-exports Socket's
# default list into the caller.  The export list PCL reads is Socket's, but
# the re-export is a run-time call, so a static parse of IO::Socket::UNIX
# never learns that these names are `()` constants in its package.
sub AF_UNIX ();
sub SOCK_STREAM ();
sub SOCK_DGRAM ();

1;
