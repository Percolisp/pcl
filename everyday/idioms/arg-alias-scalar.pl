# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-03-arg-alias-scalar.pl
no warnings;
sub inc { $_[0]++ } my $x = 1; inc($x); my @a = (5); inc($a[0]); my %h = (k => 7); inc($h{k}); print "$x $a[0] $h{k}\n";
