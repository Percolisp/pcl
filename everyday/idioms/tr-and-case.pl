# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-13-tr-and-case.pl
no warnings;
my $s = "Hello World"; (my $t = $s) =~ tr/a-z/A-Z/; my $n = ($s =~ tr/o//); print "$t $n ", lc($s), " ", ucfirst(lc("ÅBC")), " ", scalar reverse("abc"), " ", "ab" x 3, "\n";
