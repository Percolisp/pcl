# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# PCL FACTS OVERLAY for IO::Compress::Zip::Constants (task #3343, s517b;
# docs/shipped-modules.md "Facts overlays").  Declarations only: PCL reads this
# beside the real module's source; the real module still runs and installs the
# real subs.
package IO::Compress::Zip::Constants;

# The prototype scan skips every IO:: module by name, so an importer never
# learned that these `use constant` names are NULLARY:
# `GZIP_MIN_HEADER_SIZE - GZIP_ID_SIZE` in core IO::Uncompress::Gunzip parsed
# as GZIP_MIN_HEADER_SIZE(-GZIP_ID_SIZE) = 10, not 8, and gunzip read two
# bytes of the deflate stream as header ("Inflation Error: data error").
# The export list is the module's own, literal.
our @EXPORT = qw(
    ZIP_CM_STORE
    ZIP_CM_DEFLATE
    ZIP_CM_BZIP2
    ZIP_CM_LZMA
    ZIP_CM_PPMD
    ZIP_CM_XZ
    ZIP_CM_ZSTD
    ZIP_CM_AES
    ZIP_LOCAL_HDR_SIG
    ZIP_DATA_HDR_SIG
    ZIP_CENTRAL_HDR_SIG
    ZIP_END_CENTRAL_HDR_SIG
    ZIP64_END_CENTRAL_REC_HDR_SIG
    ZIP64_END_CENTRAL_LOC_HDR_SIG
    ZIP64_ARCHIVE_EXTRA_SIG
    ZIP64_DIGITAL_SIGNATURE_SIG
    ZIP_GP_FLAG_ENCRYPTED_MASK
    ZIP_GP_FLAG_STREAMING_MASK
    ZIP_GP_FLAG_PATCHED_MASK
    ZIP_GP_FLAG_STRONG_ENCRYPTED_MASK
    ZIP_GP_FLAG_LZMA_EOS_PRESENT
    ZIP_GP_FLAG_LANGUAGE_ENCODING
    ZIP_EXTRA_ID_ZIP64
    ZIP_EXTRA_ID_EXT_TIMESTAMP
    ZIP_EXTRA_ID_INFO_ZIP_UNIX2
    ZIP_EXTRA_ID_INFO_ZIP_UNIXN
    ZIP_EXTRA_ID_INFO_ZIP_Upath
    ZIP_EXTRA_ID_INFO_ZIP_Ucom
    ZIP_EXTRA_ID_JAVA_EXE
    ZIP_OS_CODE_UNIX
    ZIP_OS_CODE_DEFAULT
    ZIP_IFA_TEXT_MASK
    %ZIP_CM_MIN_VERSIONS
    ZIP64_MIN_VERSION
    ZIP_A_RONLY
    ZIP_A_HIDDEN
    ZIP_A_SYSTEM
    ZIP_A_LABEL
    ZIP_A_DIR
    ZIP_A_ARCHIVE
);

sub ZIP_CM_STORE ();
sub ZIP_CM_DEFLATE ();
sub ZIP_CM_BZIP2 ();
sub ZIP_CM_LZMA ();
sub ZIP_CM_PPMD ();
sub ZIP_CM_XZ ();
sub ZIP_CM_ZSTD ();
sub ZIP_CM_AES ();
sub ZIP_LOCAL_HDR_SIG ();
sub ZIP_DATA_HDR_SIG ();
sub ZIP_CENTRAL_HDR_SIG ();
sub ZIP_END_CENTRAL_HDR_SIG ();
sub ZIP64_END_CENTRAL_REC_HDR_SIG ();
sub ZIP64_END_CENTRAL_LOC_HDR_SIG ();
sub ZIP64_ARCHIVE_EXTRA_SIG ();
sub ZIP64_DIGITAL_SIGNATURE_SIG ();
sub ZIP_GP_FLAG_ENCRYPTED_MASK ();
sub ZIP_GP_FLAG_STREAMING_MASK ();
sub ZIP_GP_FLAG_PATCHED_MASK ();
sub ZIP_GP_FLAG_STRONG_ENCRYPTED_MASK ();
sub ZIP_GP_FLAG_LZMA_EOS_PRESENT ();
sub ZIP_GP_FLAG_LANGUAGE_ENCODING ();
sub ZIP_EXTRA_ID_ZIP64 ();
sub ZIP_EXTRA_ID_EXT_TIMESTAMP ();
sub ZIP_EXTRA_ID_INFO_ZIP_UNIX2 ();
sub ZIP_EXTRA_ID_INFO_ZIP_UNIXN ();
sub ZIP_EXTRA_ID_INFO_ZIP_Upath ();
sub ZIP_EXTRA_ID_INFO_ZIP_Ucom ();
sub ZIP_EXTRA_ID_JAVA_EXE ();
sub ZIP_OS_CODE_UNIX ();
sub ZIP_OS_CODE_DEFAULT ();
sub ZIP_IFA_TEXT_MASK ();
sub ZIP64_MIN_VERSION ();
sub ZIP_A_RONLY ();
sub ZIP_A_HIDDEN ();
sub ZIP_A_SYSTEM ();
sub ZIP_A_LABEL ();
sub ZIP_A_DIR ();
sub ZIP_A_ARCHIVE ();

1;
