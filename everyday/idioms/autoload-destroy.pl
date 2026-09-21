# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-18-autoload-destroy.pl
no warnings;
package P; our $AUTOLOAD; my $gone = 0; sub new { bless {}, shift } sub AUTOLOAD { my $n = $AUTOLOAD; $n =~ s/.*:://; return if $n eq "DESTROY"; "auto($n)" } package main; print P->new->foo, " ", P->new->bar(1), "\n";
