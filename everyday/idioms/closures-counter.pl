# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-05-closures-counter.pl
no warnings;
sub mk { my $n = shift; return sub { return $n++ } } my ($a, $b) = (mk(5), mk(10)); $a->(); print $a->(), $b->(), "\n";
