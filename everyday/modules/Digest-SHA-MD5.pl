# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-15-Digest-SHA-MD5.pl
use strict; use warnings; use Digest::SHA qw(sha1_hex sha256_hex); use Digest::MD5 qw(md5_hex);
print sha1_hex("abc"), " ", substr(sha256_hex("abc"), 0, 16), " ", md5_hex("abc"), " ", Digest::SHA->new(256)->add("a")->add("bc")->hexdigest eq sha256_hex("abc") ? "oo-same" : "oo-diff", "\n";
