# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-38-sprintf-local-time.pl
no warnings;
my @parts = (localtime)[3,4,5]; my ($d, $m, $y) = @parts; print( (($y + 1900) > 2000 ? "ok" : "bad"), " ", scalar(@parts), "\n");
