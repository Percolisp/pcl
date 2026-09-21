# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-34-sprintf-utf8-io.pl
no warnings;
use utf8; binmode(STDOUT, ":encoding(UTF-8)"); my $s = "naïve café ☺"; print length($s), " ", uc($s), " ", scalar reverse($s), " ", ($s =~ /\w+\s(\w+)/ ? $1 : "no"), "\n";
