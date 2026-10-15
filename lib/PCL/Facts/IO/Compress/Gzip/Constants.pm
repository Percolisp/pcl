# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# PCL FACTS OVERLAY for IO::Compress::Gzip::Constants (task #3343, s517b;
# docs/shipped-modules.md "Facts overlays").  Declarations only: PCL reads this
# beside the real module's source; the real module still runs and installs the
# real subs.
package IO::Compress::Gzip::Constants;

# The prototype scan skips every IO:: module by name, so an importer never
# learned that these `use constant` names are NULLARY:
# `GZIP_MIN_HEADER_SIZE - GZIP_ID_SIZE` in core IO::Uncompress::Gunzip parsed
# as GZIP_MIN_HEADER_SIZE(-GZIP_ID_SIZE) = 10, not 8, and gunzip read two
# bytes of the deflate stream as header ("Inflation Error: data error").
# The export list is the module's own, literal.
our @EXPORT = qw(
    GZIP_ID_SIZE
    GZIP_ID1
    GZIP_ID2
    GZIP_FLG_DEFAULT
    GZIP_FLG_FTEXT
    GZIP_FLG_FHCRC
    GZIP_FLG_FEXTRA
    GZIP_FLG_FNAME
    GZIP_FLG_FCOMMENT
    GZIP_FLG_RESERVED
    GZIP_CM_DEFLATED
    GZIP_MIN_HEADER_SIZE
    GZIP_TRAILER_SIZE
    GZIP_MTIME_DEFAULT
    GZIP_XFL_DEFAULT
    GZIP_FEXTRA_HEADER_SIZE
    GZIP_FEXTRA_MAX_SIZE
    GZIP_FEXTRA_SUBFIELD_HEADER_SIZE
    GZIP_FEXTRA_SUBFIELD_ID_SIZE
    GZIP_FEXTRA_SUBFIELD_LEN_SIZE
    GZIP_FEXTRA_SUBFIELD_MAX_SIZE
    $GZIP_FNAME_INVALID_CHAR_RE
    $GZIP_FCOMMENT_INVALID_CHAR_RE
    GZIP_FHCRC_SIZE
    GZIP_ISIZE_MAX
    GZIP_ISIZE_MOD_VALUE
    GZIP_NULL_BYTE
    GZIP_OS_DEFAULT
    %GZIP_OS_Names
    GZIP_MINIMUM_HEADER
);

sub GZIP_ID_SIZE ();
sub GZIP_ID1 ();
sub GZIP_ID2 ();
sub GZIP_FLG_DEFAULT ();
sub GZIP_FLG_FTEXT ();
sub GZIP_FLG_FHCRC ();
sub GZIP_FLG_FEXTRA ();
sub GZIP_FLG_FNAME ();
sub GZIP_FLG_FCOMMENT ();
sub GZIP_FLG_RESERVED ();
sub GZIP_CM_DEFLATED ();
sub GZIP_MIN_HEADER_SIZE ();
sub GZIP_TRAILER_SIZE ();
sub GZIP_MTIME_DEFAULT ();
sub GZIP_XFL_DEFAULT ();
sub GZIP_FEXTRA_HEADER_SIZE ();
sub GZIP_FEXTRA_MAX_SIZE ();
sub GZIP_FEXTRA_SUBFIELD_HEADER_SIZE ();
sub GZIP_FEXTRA_SUBFIELD_ID_SIZE ();
sub GZIP_FEXTRA_SUBFIELD_LEN_SIZE ();
sub GZIP_FEXTRA_SUBFIELD_MAX_SIZE ();
sub GZIP_FHCRC_SIZE ();
sub GZIP_ISIZE_MAX ();
sub GZIP_ISIZE_MOD_VALUE ();
sub GZIP_NULL_BYTE ();
sub GZIP_OS_DEFAULT ();
sub GZIP_MINIMUM_HEADER ();

1;
