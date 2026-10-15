# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# PCL FACTS OVERLAY for IO::Compress::Zlib::Constants (task #3343, s517b;
# docs/shipped-modules.md "Facts overlays").  Declarations only: PCL reads this
# beside the real module's source; the real module still runs and installs the
# real subs.
package IO::Compress::Zlib::Constants;

# The prototype scan skips every IO:: module by name, so an importer never
# learned that these `use constant` names are NULLARY:
# `GZIP_MIN_HEADER_SIZE - GZIP_ID_SIZE` in core IO::Uncompress::Gunzip parsed
# as GZIP_MIN_HEADER_SIZE(-GZIP_ID_SIZE) = 10, not 8, and gunzip read two
# bytes of the deflate stream as header ("Inflation Error: data error").
# The export list is the module's own, literal.
our @EXPORT = qw(
    ZLIB_HEADER_SIZE
    ZLIB_TRAILER_SIZE
    ZLIB_CMF_CM_OFFSET
    ZLIB_CMF_CM_BITS
    ZLIB_CMF_CM_DEFLATED
    ZLIB_CMF_CINFO_OFFSET
    ZLIB_CMF_CINFO_BITS
    ZLIB_CMF_CINFO_MAX
    ZLIB_FLG_FCHECK_OFFSET
    ZLIB_FLG_FCHECK_BITS
    ZLIB_FLG_FDICT_OFFSET
    ZLIB_FLG_FDICT_BITS
    ZLIB_FLG_LEVEL_OFFSET
    ZLIB_FLG_LEVEL_BITS
    ZLIB_FLG_LEVEL_FASTEST
    ZLIB_FLG_LEVEL_FAST
    ZLIB_FLG_LEVEL_DEFAULT
    ZLIB_FLG_LEVEL_SLOWEST
    ZLIB_FDICT_SIZE
);

sub ZLIB_HEADER_SIZE ();
sub ZLIB_TRAILER_SIZE ();
sub ZLIB_CMF_CM_OFFSET ();
sub ZLIB_CMF_CM_BITS ();
sub ZLIB_CMF_CM_DEFLATED ();
sub ZLIB_CMF_CINFO_OFFSET ();
sub ZLIB_CMF_CINFO_BITS ();
sub ZLIB_CMF_CINFO_MAX ();
sub ZLIB_FLG_FCHECK_OFFSET ();
sub ZLIB_FLG_FCHECK_BITS ();
sub ZLIB_FLG_FDICT_OFFSET ();
sub ZLIB_FLG_FDICT_BITS ();
sub ZLIB_FLG_LEVEL_OFFSET ();
sub ZLIB_FLG_LEVEL_BITS ();
sub ZLIB_FLG_LEVEL_FASTEST ();
sub ZLIB_FLG_LEVEL_FAST ();
sub ZLIB_FLG_LEVEL_DEFAULT ();
sub ZLIB_FLG_LEVEL_SLOWEST ();
sub ZLIB_FDICT_SIZE ();

1;
