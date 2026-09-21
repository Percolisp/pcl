# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-22-string-funcs.pl
no warnings;
my $s = "The quick brown fox"; print join("|", index($s, "quick"), rindex($s, "o"), substr($s, 4, 5), lc(substr($s, -3)), sprintf("%s", join ",", map { ucfirst } split / /, lc $s), length($s), ord("A"), chr(97), "abc" lt "abd" ? 1 : 0, "10" == 10.0 ? 1 : 0), "\n";
