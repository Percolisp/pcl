# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-23-substr-lvalue.pl
no warnings;
my $s = "Hello World"; substr($s, 0, 5) = "HELLO"; substr($s, -5, 5, "Perl!"); my $r = \ substr($s, 0, 1); print "$s\n";
