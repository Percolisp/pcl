# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-16-MIME-Base64.pl
use strict; use warnings; use MIME::Base64 qw(encode_base64 decode_base64);
my $e = encode_base64("Hello, World! \x00\xff", ""); print "$e ", (decode_base64($e) eq "Hello, World! \x00\xff" ? "rt" : "bad"), " ", length(encode_base64("x" x 100)), "\n";
