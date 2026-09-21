# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-02-wantarray.pl
no warnings;
sub c { wantarray ? "list" : defined(wantarray) ? "scalar" : "void" } my @a = c(); my $s = c(); print "$a[0] $s\n";
